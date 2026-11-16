read_dataset <- function(year, type) {
  filename <- list.files(
    glue::glue("submissions/{test_flag}{year}/{type}"),
    full.names = TRUE
  )

  if (length(filename) > 1) {
    stop(glue::glue("More than one file detected in {type} folder for {year}"))
  }

  if (length(filename) == 0) {
    stop(glue::glue("No file detected in {type} folder for {year}"))
  }

  imported_table <- readr::read_csv(filename)
  return(imported_table)
}
