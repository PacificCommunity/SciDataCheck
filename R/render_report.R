render_validation_report <- function(res, file_inventory, year, output_dir = ".", run_date = NULL) {
  if (is.null(quarto::quarto_path())) {
    stop("Quarto CLI not found. Install it from https://quarto.org", call. = FALSE)
  }

  if (is.null(run_date)){
    run_date = as.character(Sys.Date() |> format("%Y-%m-%d"))
  }

  work_dir <- tempfile("validation_report_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  template_dir <- system.file("report", package = "SciDataCheck", mustWork = TRUE)
  file.copy(list.files(template_dir, full.names = TRUE), work_dir, recursive = TRUE)

  # everything the report needs goes in work_dir
  results_path    <- file.path(work_dir, "results.rds")
  inventory_path  <- file.path(work_dir, "file_inventory.rds")
  error_desc_path <- file.path(work_dir, "error_descriptions.rds")

  saveRDS(res, results_path)
  saveRDS(file_inventory, inventory_path)
  saveRDS(get_check_descriptions(), error_desc_path)

  quarto::quarto_render(
    input          = file.path(work_dir, "validation_report.qmd"),
    execute_params = list(results_path    = results_path,
                          inventory_path  = inventory_path,
                          error_desc_path = error_desc_path,
                          submission_year = as.character(year),
                          run_date = run_date),
    quiet          = FALSE
  )

  # move the output to submissions folder
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(
    output_dir,
    paste0("validation_report_", year, "_", format(Sys.time(), "%Y%m%d"), ".html")
  )
  file.copy(file.path(work_dir, "validation_report.html"), out_path, overwrite = TRUE)

  invisible(normalizePath(out_path))
}
