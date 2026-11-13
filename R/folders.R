#' @noRd
.create_folder_structure <- function(submission_year) {
  # submissions/{year}/{data_type_name}/{filename}.{csv}

  mapping_filename <- system.file(
    "mapping_template.csv",
    package = "SciDataCheck"
  )
  if (file.exists(mapping_filename)) {
    mapping_pattern <-
      readr::read_csv(mapping_filename, show_col_types = FALSE) |>
      mutate(template_name = na_if(template_name, "")) |>
      filter(!is.na(template_name)) |>
      mutate(
        template_path = paste0(
          "submissions/",
          submission_year,
          "/",
          data_type_name
        )
      )

    dir_names <- mapping_pattern |> pull(template_path)

    for (dir in dir_names) {
      if (!dir.exists(file.path('./', dir))) {
        dir.create(file.path('./', dir), recursive = TRUE)
      }
    }
  }
  else {
    print("object mapping_template not found")
  }
}
