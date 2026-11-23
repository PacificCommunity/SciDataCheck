#' @export
validate_data <- function(year = NULL) {
  if (is.null(year)) {
    year <- as.numeric(Sys.Date() |> format("%Y")) - 1
  }

  # TODO: folder structure should be {year}/submissions/{data_types} and {year}/reports/
  dir_submission <- paste0(year, "/submissions")

  if (dir.exists(dir_submission)) {
    print("folder structure exists")

    # check number of csv files available for validation if any
    all_files <- list.files(
      dir_submission,
      pattern = ".csv$",
      recursive = T,
      full.names = T
    )

    # NOTE: If more than one csv in the data type folder only the most recently created will be validated
    file_inventory <- .build_file_inventory(
      submission_year = year,
      all_files = all_files
    )

    if (is.null(file_inventory)) {
      return(invisible())
    }

    # Validate files - save output into a temp file
    res <- validate_files(file_inventory)

    if (nrow(res |> filter(!(status == "Validation failed"))) > 1) {
      # second level validation in files that passed the first level
    }

    # TODO
    # this should be changed to if-else statement at some point
    # if passes all the second-checks then show plots??
    inc_plots_bool <- TRUE

    # Build report
    report <- render_validation_report(
      res,
      file_inventory,
      year = year,
      include_plots = inc_plots_bool,
      output_dir = dir_submission
    )

    invisible(list(results = res, report = report))
  } else {
    .create_folder_structure(submission_year = year)
    message(paste0(
      "The folder structure for the ",
      year,
      " scidata submissions was created.",
      " Run `validate_data()` again after adding the files you would like to",
      " validate in the appropriate data type folders."
    ))
    return(invisible())
  }
}
