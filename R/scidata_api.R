## 5. Human-readable descriptions for validation checks ####
# Pulls the live rule definitions from the same API used for validation, so
# check descriptions can never drift out of sync with what's actually being
# checked (no manually-maintained lookup table to keep updated).

fetch_validation_rules <- function(
    base_url = .api_base_url,
    endpoint = .endpoint_validation_rules
) {
  httr2::request(base_url) |>
    httr2::req_url_path_append(endpoint) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

# keep only the latest version per template_name - templates carry full
# version history, and only the current schema is relevant for describing
# checks against files validated today
pick_latest <- function(templates) {
  versions <- map_dbl(templates, ~ as.numeric(.x$version))
  names <- map_chr(templates, ~ .x$template_name)
  keep_idx <- tapply(seq_along(templates), names, function(i) {
    i[which.max(versions[i])]
  })
  templates[unlist(keep_idx)]
}

# Column-level checks: both explicit ones declared in the `checks` array,
# and ones synthesized from the `mandatory` / `nullable` flags (the API
# doesn't list these as named checks itself).
flatten_column_checks <- function(tmpl) {
  cols <- tmpl$definition$columns
  map_dfr(names(cols), function(col_name) {
    col <- cols[[col_name]]
    col_desc <- col$description %||% ""
    out <- tibble()

    if (!is.null(col$checks) && length(col$checks) > 0) {
      out <- map_dfr(col$checks, function(chk) {
        tibble(errors.check = chk$id, description = chk$description)
      })
    }
    if (isTRUE(col$mandatory)) {
      # NOTE: synthesized id. validation_report.qmd's setup chunk renames the
      # raw API id "column_in_dataframe" to "missing_column_in_dataframe" for
      # display - this id is kept in sync with that renamed value, not the
      # raw API id. If that rename is ever changed/removed, update both.
      out <- bind_rows(
        out,
        tibble(
          errors.check = "missing_column_in_dataframe",
          description = glue(
            "The required column '{col_name}' is missing from this file. ({col_desc})"
          )
        )
      )
    }
    if (identical(col$nullable, FALSE)) {
      out <- bind_rows(
        out,
        tibble(
          errors.check = "not_nullable",
          description = glue(
            "The '{col_name}' field cannot be blank. ({col_desc})"
          )
        )
      )
    }
    if (nrow(out) == 0) {
      return(tibble())
    }
    out |> mutate(errors.column = col_name, .before = 1)
  }) |>
    mutate(
      template_name = tmpl$template_name,
      alias_name = tmpl$alias_name,
      .before = 1
    )
}

# Dataframe-level (cross-column) checks - these already carry an id and
# description directly in the API response.
flatten_dataframe_checks <- function(tmpl) {
  dchecks <- tmpl$definition$dataframe_checks
  if (is.null(dchecks) || length(dchecks) == 0) {
    return(tibble())
  }
  map_dfr(
    dchecks,
    ~ tibble(errors.check = .x$id, description = .x$description)
  ) |>
    mutate(
      template_name = tmpl$template_name,
      alias_name = tmpl$alias_name,
      errors.column = NA_character_,
      .before = 1
    )
}

# One call to fetch + flatten everything into a single lookup table, keyed
# on (template_name, errors.column, errors.check).
get_check_descriptions <- function(
    base_url = .api_base_url,
    endpoint = .endpoint_validation_rules
) {
  raw <- fetch_validation_rules(base_url, endpoint)
  templates <- pick_latest(raw$templates) # <-- extract $templates here

  purrr::map_dfr(templates, function(t) {
    bind_rows(flatten_column_checks(t), flatten_dataframe_checks(t))
  }) |>
    distinct()
}
