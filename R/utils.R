library(httr)
library(jsonlite)
library(tidyverse)
library(tidyr)
library(arrow)
library(gt)
library(httr2)
library(tibble)
library(glue)

# create folders in case they don't exist
dir.create("input/", showWarnings = FALSE)
dir.create("output/", showWarnings = FALSE)

options(scipen = 999)

API_BASE_URL <- "https://scidata-standard-api-dev-dhc5fcajg3cvbmag.australiaeast-01.azurewebsites.net/"
# "https://scidata-validator.fame.spc.int/"; "http://localhost:8000/"

endpoint_validate <- "api/v1/validate"
endpoint_validation_rules <- "api/v1/templates/validation_rules"

# Helper function for null coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' @noRd
create_folder_structure <- function(
  year_dirs,
  country_codes #,
  # mapping_path = "input/mapping_template.csv"
) {
  # submissions/{year}/{data_type_name}/{country_code}/{filename}.{csv|parquet}

  mapping_filename <- system.file(
    "mapping_template.csv",
    package = "SciDataCheck"
  )
  if (file.exists(mapping_filename)) {
    for (year_dir in year_dirs) {
      for (country_code in country_codes) {
        mapping_pattern <-
          readr::read_csv(mapping_filename, show_col_types = FALSE) |>
          mutate(template_name = na_if(template_name, "")) |>
          filter(!is.na(template_name)) |>
          mutate(
            template_path = paste0(
              "submissions/",
              year_dir,
              "/",
              data_type_name,
              "/",
              country_code
            )
          )

        dir_names <- mapping_pattern |> pull(template_path)

        for (dir in dir_names) {
          if (!dir.exists(file.path('./', dir))) {
            dir.create(file.path('./', dir), recursive = TRUE)
          }
        }
      }
    }
  } else {
    print("object mapping_template not found")
  }
}
# ── Template discovery / inspection ──────────────────────────────

# Function to list available templates
list_latest_templates <- function(base_url, endpoint) {
  url <- paste0(base_url, endpoint)

  response <- GET(url)

  if (status_code(response) != 200) {
    stop("Failed to fetch templates: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")
  result <- fromJSON(content, flatten = TRUE)

  cat("Available Templates:\n")
  for (i in seq_len(nrow(result$templates))) {
    template <- result$templates[i, ]
    cat("\n", i, ". ", template$name, "\n", sep = "")
  }

  return(invisible(result$templates))
}

get_template_details <- function(base_url, endpoint, template_name = NULL) {
  # Build URL with optional template_name parameter
  url <- if (!is.null(template_name)) {
    paste0(base_url, endpoint, "?template_name=", template_name)
  } else {
    paste0(base_url, endpoint)
  }

  # Make API request
  response <- httr::GET(url)

  # Check if request was successful
  if (httr::http_error(response)) {
    stop(sprintf(
      "API request failed with status %s: %s",
      httr::status_code(response),
      httr::content(response, "text", encoding = "UTF-8")
    ))
  }

  # Parse JSON response
  content <- httr::content(response, "text", encoding = "UTF-8")
  data <- jsonlite::fromJSON(content, simplifyVector = FALSE)

  # Check if templates exist
  if (is.null(data$templates) || length(data$templates) == 0) {
    warning("No templates found")
    return(data.frame())
  }

  # Process each template and flatten the structure
  result_list <- lapply(seq_along(data$templates), function(i) {
    template <- data$templates[[i]]

    # Extract template-level info
    template_info <- data.frame(
      template_name = template$template_name %||% NA_character_,
      alias_name = template$alias_name %||% NA_character_,
      version = template$version %||% NA_character_,
      stringsAsFactors = FALSE
    )

    # Extract column specifications
    if (!is.null(template$definition$columns)) {
      columns <- template$definition$columns

      column_details <- lapply(names(columns), function(col_name) {
        col_spec <- columns[[col_name]]

        # Create a data frame with all possible column specification fields
        data.frame(
          column_name = col_name,
          type = col_spec$type %||% NA_character_,
          nullable = col_spec$nullable %||% NA,
          min = col_spec$min %||% NA_real_,
          max = col_spec$max %||% NA_real_,
          min_length = col_spec$min_length %||% NA_integer_,
          max_length = col_spec$max_length %||% NA_integer_,
          default = as.character(col_spec$default %||% NA_character_),
          description = col_spec$description %||% NA_character_,
          stringsAsFactors = FALSE
        )
      })

      # Combine column details
      column_df <- do.call(rbind, column_details)

      # Cross join template info with column details
      result <- merge(template_info, column_df, by = NULL)
    } else {
      # No columns defined
      result <- template_info
      result$column_name <- NA_character_
    }

    return(result)
  })

  # Combine all results into a single data frame
  final_df <- do.call(rbind, result_list)

  # Reset row names
  rownames(final_df) <- NULL

  return(final_df)
}

# Function to fetch an example data template
example_data_template <- function(base_url, endpoint, template_name) {
  url <- paste0(base_url, endpoint, "/", template_name)

  response <- GET(url)

  if (status_code(response) != 200) {
    stop("Failed to fetch templates: ", status_code(response))
  }

  content <- content(response, as = "text", encoding = "UTF-8")
  result <- fromJSON(content, flatten = TRUE)

  df <- result$data |>
    data.frame()

  # df$version <- result$template_version
  # df$name <- result$template_name
  return(df)
}

# ── File validation ───────────────────────────────────────────

# Function to validate a data file (CSV or Parquet)
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
  if (!grepl("\\.(csv|parquet)$", file_path, ignore.case = TRUE)) {
    stop("File must be a CSV or Parquet file: ", file_path)
  }

  # The API only accepts CSV - convert parquet to a temp CSV before upload
  upload_path <- file_path
  tmp_file <- NULL
  if (grepl("\\.parquet$", file_path, ignore.case = TRUE)) {
    tmp_file <- tempfile(fileext = ".csv")
    arrow::read_parquet(file_path) |> write.csv(tmp_file, row.names = FALSE)
    upload_path <- tmp_file
  }

  on.exit(if (!is.null(tmp_file)) unlink(tmp_file), add = TRUE)

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
    body = list(file = upload_file(upload_path)),
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

## 1. Prepare data to be validated ####

# Discovers files under:
#   {root_dir}/{year}/{data_type_name}/{country_code}/{filename}.{csv|parquet}
#
# The submission year and country code are now read straight from the folder
# structure, so filenames no longer need to encode either of those - any
# filename is fine (including the generated/UUID-style names used for
# parquet exports).
#
# `mapping_pattern`, if supplied, is a data.frame with columns:
#   data_type_name  - matches the folder name under {root_dir}/{year}/
#   template_name   - the template name to validate that folder's files against
#   data_submission - (optional) carried through as `scidata_portal`, same as before
#
# Any data_type_name folder with no row in `mapping_pattern` (or when
# `mapping_pattern` is NULL) is validated using the folder name itself as the
# template_name - i.e. folder name == template name unless told otherwise.
#
# All of `years`, `country_codes` and `data_types` are optional filters;
# leave them NULL to auto-discover and validate everything found.
build_file_inventory <- function(
  years = NULL,
  country_codes = NULL,
  root_dir = "submissions",
  data_types = NULL,
  mapping_dir = "./input/mapping_template.csv",
  file_regex = "\\.(csv|parquet)$",
  latest_file = FALSE
) {
  if (!dir.exists(root_dir)) {
    stop("Root submissions directory not found: ", root_dir)
  }

  if (!file.exists(mapping_dir)) {
    stop("File doesn't exist: ", mapping_dir)
  } else {
    mapping_pattern <- read.csv(mapping_dir)
  }

  # 1. Which year folders to look at
  year_dirs <- list.dirs(root_dir, recursive = FALSE, full.names = TRUE)
  if (!is.null(years)) {
    year_dirs <- year_dirs[basename(year_dirs) %in% as.character(years)]
  }

  if (length(year_dirs) == 0) {
    warning("No matching year folders found under: ", root_dir)
    return(data.frame())
  }

  results <- list()

  for (year_dir in year_dirs) {
    yr <- basename(year_dir)

    # 2. data_type_name folders (e.g. ace, agg_catch_ll, log_trip_pl, ...)
    dt_dirs <- list.dirs(year_dir, recursive = FALSE, full.names = TRUE)
    if (!is.null(data_types)) {
      dt_dirs <- dt_dirs[basename(dt_dirs) %in% data_types]
    }

    for (dt_dir in dt_dirs) {
      data_type_name <- basename(dt_dir)

      # 3. country_code folders
      cc_dirs <- list.dirs(dt_dir, recursive = FALSE, full.names = TRUE)
      if (!is.null(country_codes)) {
        cc_dirs <- cc_dirs[
          tolower(basename(cc_dirs)) %in% tolower(country_codes)
        ]
      }

      for (cc_dir in cc_dirs) {
        cc <- basename(cc_dir)

        files <- list.files(
          cc_dir,
          pattern = file_regex,
          ignore.case = TRUE,
          full.names = TRUE
        )
        if (length(files) == 0) {
          next
        }

        # keep only the most recently modified file in this folder
        if (latest_file && length(files) > 1) {
          mtimes <- file.info(files)$mtime
          files <- files[which.max(mtimes)]
        }

        # 4. resolve template_name / scidata_portal via the optional mapping,
        #    falling back to the folder name itself when there's no mapping entry
        template_name <- data_type_name
        scidata_portal <- NA
        title <- data_type_name

        if (!is.null(mapping_pattern) && nrow(mapping_pattern) > 0) {
          match_row <- mapping_pattern[
            mapping_pattern$data_type_name == data_type_name,
          ]
          if (nrow(match_row) > 0) {
            template_name <- match_row$template_name[1]
            scidata_portal <- match_row$data_submission[1]
            title <- match_row$title[1]
          }
        }

        results[[length(results) + 1]] <- data.frame(
          files = files,
          title = title,
          data_type_name = data_type_name,
          template_name = template_name,
          scidata_portal = scidata_portal,
          country_code = cc,
          submission_year = suppressWarnings(as.numeric(yr)),
          stringsAsFactors = FALSE
        )
      }
    }
  }

  bind_rows(results)
}

## 4. Validate a data file ####

validate_files <- function(
  df_files,
  base_url = API_BASE_URL,
  endpoint = endpoint_validate
) {
  results <- list()

  for (i in seq_len(nrow(df_files))) {
    r <- validate_file(
      base_url = base_url,
      endpoint = endpoint,
      template_name = df_files$template_name[i],
      file_path = df_files$files[i],
      submission_year = df_files$submission_year[i]
    )

    if (!(r$message %in% "All data passed validation")) {
      r_df <- data.frame(errors = r$errors, status = r$message)
    } else {
      r_df <- data.frame(status = r$message)
    }

    r_df$filename <- df_files$files[i]
    r_df$data_type_name <- df_files$data_type_name[i]
    r_df$template_name <- df_files$template_name[i]
    r_df$country_code <- df_files$country_code[i]
    results[[i]] <- r_df
  }

  bind_rows(results)
}

## 5. Human-readable descriptions for validation checks ####
#
# Pulls the live rule definitions from the same API used for validation, so
# check descriptions can never drift out of sync with what's actually being
# checked (no manually-maintained lookup table to keep updated).

fetch_validation_rules <- function(
  base_url,
  endpoint = "api/v1/templates/validation_rules"
) {
  httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

# keep only the latest version per template_name - templates carry full
# version history, and only the current schema is relevant for describing
# checks against files validated today
pick_latest <- function(templates) {
  versions <- map_dbl(templates, ~ as.numeric(.x$version))
  names <- map_chr(templates, ~ .x$template_name)
  keep_idx <- tapply(seq_along(templates), names, function(i) {
    i[which.max(versions[i])]
  })
  templates[unlist(keep_idx)]
}

# Column-level checks: both explicit ones declared in the `checks` array,
# and ones synthesized from the `mandatory` / `nullable` flags (the API
# doesn't list these as named checks itself).
flatten_column_checks <- function(tmpl) {
  cols <- tmpl$definition$columns
  map_dfr(names(cols), function(col_name) {
    col <- cols[[col_name]]
    col_desc <- col$description %||% ""
    out <- tibble()

    if (!is.null(col$checks) && length(col$checks) > 0) {
      out <- map_dfr(col$checks, function(chk) {
        tibble(errors.check = chk$id, description = chk$description)
      })
    }
    if (isTRUE(col$mandatory)) {
      # NOTE: synthesized id. validation_report.qmd's setup chunk renames the
      # raw API id "column_in_dataframe" to "missing_column_in_dataframe" for
      # display - this id is kept in sync with that renamed value, not the
      # raw API id. If that rename is ever changed/removed, update both.
      out <- bind_rows(
        out,
        tibble(
          errors.check = "missing_column_in_dataframe",
          description = glue(
            "The required column '{col_name}' is missing from this file. ({col_desc})"
          )
        )
      )
    }
    if (identical(col$nullable, FALSE)) {
      out <- bind_rows(
        out,
        tibble(
          errors.check = "not_nullable",
          description = glue(
            "The '{col_name}' field cannot be blank. ({col_desc})"
          )
        )
      )
    }
    if (nrow(out) == 0) {
      return(tibble())
    }
    out |> mutate(errors.column = col_name, .before = 1)
  }) |>
    mutate(
      template_name = tmpl$template_name,
      alias_name = tmpl$alias_name,
      .before = 1
    )
}

# Dataframe-level (cross-column) checks - these already carry an id and
# description directly in the API response.
flatten_dataframe_checks <- function(tmpl) {
  dchecks <- tmpl$definition$dataframe_checks
  if (is.null(dchecks) || length(dchecks) == 0) {
    return(tibble())
  }
  map_dfr(
    dchecks,
    ~ tibble(errors.check = .x$id, description = .x$description)
  ) |>
    mutate(
      template_name = tmpl$template_name,
      alias_name = tmpl$alias_name,
      errors.column = NA_character_,
      .before = 1
    )
}

# One call to fetch + flatten everything into a single lookup table, keyed
# on (template_name, errors.column, errors.check).
get_check_descriptions <- function(
  base_url,
  endpoint = "api/v1/templates/validation_rules"
) {
  raw <- fetch_validation_rules(base_url, endpoint)
  templates <- pick_latest(raw$templates) # <-- extract $templates here

  purrr::map_dfr(templates, function(t) {
    bind_rows(flatten_column_checks(t), flatten_dataframe_checks(t))
  }) |>
    distinct()
}
