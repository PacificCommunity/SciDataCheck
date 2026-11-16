# make this generic, as each folder should only have one file.
read_ace <- function(year) {
  ace_filename <- list.files(
    glue::glue("submissions/{test_flag}{year}/ace"),
    full.names = TRUE
  )

  if (length(ace_filename) > 1) {
    stop(glue::glue("More than one file detected in ACE folder for {year}"))
  }

  if (length(ace_filename) == 0) {
    stop(glue::glue("No file detected in ACE folder for {year}"))
  }

  ace_output <- readr::read_csv(ace_filename)
  return(ace_output)
}
