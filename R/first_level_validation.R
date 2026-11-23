#' @noRd
.build_file_inventory <- function(submission_year, all_files) {
  # TODO: add this as an argument in the function:
  dir_submission <- paste0(submission_year, "/submissions")

  if (length(all_files) == 0) {
    message(paste0(
      "No csv files were found under the following directory: `",
      dir_submission,
      "`. Please make sure you add the files",
      " you would like to validate in the appropriate data type folders."
    ))
    return(invisible())
  } else {
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

      more_than_one_files <- sel_files |>
        group_by(template_name) |>
        summarise(n = n_distinct(file_name)) |>
        filter(n > 1)

      if (nrow(more_than_one_files) > 0) {
        folders_check <- unique(more_than_one_files$template_name)

        message(glue::glue(
          "You need to have a single file inside these data type folders: {folders_check}"
        ))
        return(invisible())
      }
    } else {
      stop("Mapping file missing from folder inst/ ")
    }

    sel_files <- sel_files |>
      dplyr::select(
        full_path,
        data_type_name,
        file_name,
        template_name,
        title
      ) |>
      mutate(submission_year = submission_year) |>
      mutate_all(as.character)

    return(sel_files)
  }
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

  results <- dplyr::bind_rows(results)

  # make sure the result schema is always the same  -  same cols and data types
  error_columns <- c(
    errors.schema_context = NA,
    errors.column = NA,
    errors.check = NA,
    errors.row_index = NA,
    errors.failure_case = NA
  )

  # add related error cols if not yet present
  results <- add_column(
    results,
    !!!error_columns[setdiff(names(error_columns), names(results))]
  )

  # select all columns and parse them all to character
  results <- results |>
    select(
      status,
      filename,
      data_type_name,
      template_name,
      errors.schema_context,
      errors.column,
      errors.check,
      errors.row_index,
      errors.failure_case
    ) |>
    dplyr::mutate_all(as.character)

  results
}
