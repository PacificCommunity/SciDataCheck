# R/second_validation_engine.R

get_applicable_rules <- function(rules, available_data_types) {
  purrr::keep(rules, \(r) all(r$data_types %in% available_data_types))
}

load_datasets <- function(filenames_by_type) {
  purrr::map(filenames_by_type, arrow::read_parquet)
}

run_second_level_validation <- function(sel_second_validation, rules, group_col = "country_code") {
  sel_second_validation |>
    group_by(.data[[group_col]]) |>
    group_split() |>
    purrr::map(function(df_grp) {
      filenames_by_type <- df_grp |> select(data_type_name, filename) |> tibble::deframe()
      available_types   <- names(filenames_by_type)
      applicable         <- get_applicable_rules(rules, available_types)
      
      if (length(applicable) == 0) return(NULL)
      
      needed_types <- unique(unlist(purrr::map(applicable, "data_types")))
      datasets     <- load_datasets(filenames_by_type[needed_types])
      
      purrr::map_dfr(applicable, function(r) {
        out <- r$fn(datasets)
        tibble(
          !!group_col  := unique(df_grp[[group_col]]),
          # rule_id      = r$id,
          validation_rule  = r$description,
          # severity     = r$severity,
          status       = out$status,
          message      = out$message #,
          # details      = list(out$details)
        )
      })
    }) |>
    purrr::list_rbind()
}