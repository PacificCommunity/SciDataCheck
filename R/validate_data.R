#' @export
validate_data <- function(year = NULL) {
  if (is.null(year)) {
    year <- as.numeric(Sys.Date() |> format("%Y")) - 1
  }

  if (.check_dirs_exist()) {
    print("folder structure exists")
  } else {
    create_folder_structure(year_dirs = year, country_codes = "CN")
  }
}
