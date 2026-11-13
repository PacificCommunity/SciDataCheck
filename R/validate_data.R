#' @export
validate_data <- function(year = NULL) {
  if (is.null(year)) {
    year <- as.numeric(Sys.Date() |> format("%Y")) - 1
  }

  # TODO: folder structure should be {year}/submissions/{data_types} and {year}/reports/
  dir_submission <- paste0("submissions/", year)
  dir_reports <- paste0(dir_submission, "/reports")

  if (dir.exists(dir_submission)) {
    print("folder structure exists")

    # check number of csv files available for validation if any
    all_files <- list.files(dir_submission, pattern = ".csv$", recursive = T, full.names = T)

    if (length(all_files) == 0){
      message(paste0("No csvs were found under the following directory: `",
                     dir_submission,"`. Please make sure you add the files",
                     " you would like to validate in the appropriate data type folders."))
      return(invisible())
    }

    # NOTE: If more than one csv in the data type folder only the most recently created will be validated
    file_inventory <- .build_file_inventory(submission_year = year,
                                                all_files = all_files,
                                                latest_files = TRUE)

    # Validate files - save output into a temp file
    res <- validate_files(file_inventory)

    # Build report
    report <- render_validation_report(res,
                                       file_inventory,
                                       year = year,
                                       output_dir = dir_reports
    )

    invisible(list(results = res, report = report))


  } else {
    .create_folder_structure(submission_year = year)
    message(paste0("The folder structure for the ", year, " scidata submissions was created.",
                   " Run `validate_data()` again after adding the files you would like to",
                   " validate in the appropriate data type folders."))
    return(invisible())
  }


}
