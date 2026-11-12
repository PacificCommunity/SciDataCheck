#' @export
validate_data <- function(year = NULL) {
  if (is.null(year)) {
    year <- as.numeric(Sys.Date() |> format("%Y")) - 1
  }
  if (dir.exists(paste0("submissions/", year))) {
    print("folder structure exists")
  } else {
    create_folder_structure(submission_year = year)
  }
}
