#' @noRd
.build_file_inventory <- function(submission_year, all_files, latest_files = TRUE) {

  map_df <- data.frame(full_path = all_files) |>
    mutate(
      data_type_name = basename(dirname(full_path)),
      file_name = basename(full_path),
      modified_at = file.info(full_path)$mtime,
      created_at = file.info(full_path)$ctime,

    )

  mapping_filename <- system.file(
    "mapping_template.csv",
    package = "SciDataCheck"
  )
  if (file.exists(mapping_filename)) {
    mapping_pattern <-
      readr::read_csv(mapping_filename, show_col_types = FALSE) |>
      mutate(template_name = na_if(template_name, "")) |>
      filter(!is.na(template_name))

    # here we are maintaining only the csv files inside the data_type folders
    sel_files <- inner_join(map_df, mapping_pattern, by = "data_type_name")

    if (latest_files){
      message("Note that if multiple files are found in the data type folder, only the most recently created will be selected for validation.")
      sel_files <- sel_files |>
        group_by(template_name) |>
        slice_max(n = 1, order_by = created_at) |>
        slice_head(n = 1) |> # maybe a bit repetitive but to make sure we end up with a single file
        mutate(submission_year = submission_year) |>
        ungroup()
    }
  }else{
    stop("Mapping file missing from inst/ ")
  }

  sel_files <- sel_files |>
    dplyr::select(submission_year,
                  full_path,
                  data_type_name,
                  file_name,
                  template_name,
                  title) |>
    mutate_all(as.character)

  return(sel_files)

}

# Function to validate a data file (CSV)
validate_file <- function(
    base_url,
    endpoint,
    template_name,
    file_path,
    submission_year = NA
) {
  # Check if file exists
  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }

  # Check file extension is one we know how to validate
  if (!grepl("\\.(csv)$", file_path, ignore.case = TRUE)) {
    stop("File must be a CSV file: ", file_path)
  }

  # Construct endpoint URL
  url <- paste0(base_url, endpoint, "/", template_name)

  # Add query parameter - now always true
  url <- paste0(url, "?return_details=true")

  if (!is.na(submission_year)) {
    url <- paste0(url, "&submission_years=", as.character(submission_year))
  }

  # Make POST request
  cat("Validating file:", file_path, "\n")
  cat("Using template:", template_name, "\n")

  response <- POST(
    url = url,
    body = list(file = upload_file(file_path)),
    encode = "multipart"
  )

  # Parse response
  content <- content(response, as = "text", encoding = "UTF-8")
  result <- fromJSON(content, flatten = TRUE)

  # Handle response based on status code
  if (status_code(response) == 200) {
    cat("\n✓ Validation PASSED\n")
    cat("Rows validated:", result$rows_validated, "\n")
    cat("Columns validated:", result$columns_validated, "\n")
    cat("Message:", result$message, "\n")

    return(invisible(result))
  } else if (status_code(response) == 422) {
    cat("\n✗ Validation FAILED\n")
    cat("Rows processed:", result$rows_processed, "\n")
    cat("Error count:", result$error_count, "\n")
    cat("Message:", result$message, "\n\n")

    if (!is.null(result$error_summary)) {
      cat("Error Summary:\n")
      for (error_type in names(result$error_summary)) {
        cat(
          "  -",
          error_type,
          ":",
          result$error_summary[[error_type]],
          "occurrences\n"
        )
      }
    }

    if (!is.null(result$hint)) {
      cat("\n", result$hint, "\n")
    }

    if (!is.null(result$errors) && length(result$errors) > 0) {
      cat("\nDetailed Errors:\n")
      print(as.data.frame(result$errors))
    }

    return(invisible(result))
  } else if (status_code(response) == 404) {
    stop("Template '", template_name, "' not found")
  } else if (status_code(response) == 400) {
    stop("Bad request: ", result$detail)
  } else {
    stop("API error (", status_code(response), "): ", result$detail)
  }
}


# Validate a data file ####

validate_files <- function(
    df_files,
    base_url = .api_base_url,
    endpoint = .endpoint_validate
) {
  results <- list()

  for (i in seq_len(nrow(df_files))) {
    r <- validate_file(
      base_url = base_url,
      endpoint = endpoint,
      template_name = df_files$template_name[i],
      file_path = df_files$full_path[i],
      submission_year = df_files$submission_year[i]
    )

    if (!(r$message %in% "All data passed validation")) {
      r_df <- data.frame(errors = r$errors, status = r$message)
    } else {
      r_df <- data.frame(status = r$message)
    }

    r_df$filename <- df_files$file_name[i]
    r_df$data_type_name <- df_files$data_type_name[i]
    r_df$template_name <- df_files$template_name[i]
    results[[i]] <- r_df
  }

  dplyr::bind_rows(results)
}
