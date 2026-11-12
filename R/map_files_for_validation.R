#' @noRd
.map_files_for_validation <- function(submission_year, all_files, latest_files = TRUE) {

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

    if (latest_files){
      message("Note that if multiple files are found in the data type folder, only the most recently created will be selected for validation.")
      sel_files <- sel_files |>
        group_by(template_name) |>
        slice_max(n = 1, order_by = created_at) |>
        slice_head(n = 1) |> # maybe a bit repetitive but to make sure we end up with a single file
        mutate(submission_year = submission_year) |>
        ungroup()
    }
  }else{
    stop("Mapping file missing from inst/ ")
  }

  sel_files <- sel_files |>
    dplyr::select(submission_year,
                  full_path,
                  data_type_name,
                  file_name,
                  template_name,
                  title) |>
    mutate_all(as.character)

  return(sel_files)

}
