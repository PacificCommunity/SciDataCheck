#' @export
validate_data <- function(year = NULL) {
  if (is.null(year)) {
    year <- as.numeric(Sys.Date() |> format("%Y")) - 1
  }

  dir_submission <- paste0("submissions/", year)

  if (dir.exists(dir_submission)) {
    print("folder structure exists")

    # check number of csv files available for validation if any
    all_files <- list.files(dir_submission, pattern = ".csv$", recursive = T, full.names = T)

    if (length(all_files) == 0){
      message(paste0("No csvs were found under the following directory: `", dir_submission,"`.
              Please make sure you add the files you would like to validate in the appropriate data type folders."))
      return(invisible())
    }

    # if any csv - generate the table mapping the files and their associated data types
    # note that if more than one csv is available in the data type folder only the file
    # was most recently created will be validated

    map_validation <- .map_files_for_validation(submission_year = year, all_files = all_files, latest_files = TRUE)

    ## config variables that should go somewhere else
    options(scipen = 999)
    API_BASE_URL <- "https://scidata-standard-api-dev-dhc5fcajg3cvbmag.australiaeast-01.azurewebsites.net/"
    endpoint_validate         <- "api/v1/validate"
    ##

    res <- validate_files(map_validation)
    print(res)


  } else {
    create_folder_structure(submission_year = year)
    message(paste0("The folder structure for the ", year, " scidata submissions was created.
                   Run `validate_data()` again after adding the files you would like to validate in the appropriate data type folders."))
    return(invisible())
  }


}
