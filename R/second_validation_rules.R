# R/second_validation_rules.R

# ── 1. Rule factories (the "how") ─────────────────────────────

new_rule <- function(id, description, data_types, severity = c("error", "warning"), fn) {
  severity <- match.arg(severity)
  list(id = id, description = description, data_types = data_types, severity = severity, fn = fn)
}

status_for <- function(severity) if (severity == "error") "failed" else "warning"

#' Build a "does every child record have a matching parent id" rule
make_child_has_parent_rule <- function(child_type, parent_type, id_col, severity = "error") {
  new_rule(
    id          = glue::glue("{child_type}_has_{parent_type}_ref"),
    description = glue::glue("All {child_type} records reference an existing {id_col} in {parent_type}"),
    data_types  = c(child_type, parent_type),
    severity    = severity,
    fn = function(datasets) {
      child  <- datasets[[child_type]]
      parent <- datasets[[parent_type]]
      missing_ids <- setdiff(unique(child[[id_col]]), unique(parent[[id_col]]))
      
      if (length(missing_ids) == 0) {
        list(status = "passed",
             message = glue::glue("All {child_type} records have a matching {id_col}"),
             details = NULL)
      } else {
        list(status = status_for(severity),
             message = glue::glue("{length(missing_ids)} {id_col}(s) in {child_type} not found in {parent_type}"),
             details = length(missing_ids)) # todo: update this, let user define if they want to return all unique
      }
    }
  )
}

#' Build a "does every parent record have at least one matching child" rule
make_parent_has_child_rule <- function(child_type, parent_type, id_col, severity = "warning") {
  new_rule(
    id          = glue::glue("{parent_type}_has_{child_type}_ref"),
    description = glue::glue("All {parent_type} records have at least one matching {child_type} record"),
    data_types  = c(child_type, parent_type),
    severity    = severity,
    fn = function(datasets) {
      child  <- datasets[[child_type]]
      parent <- datasets[[parent_type]]
      missing_ids <- setdiff(unique(parent[[id_col]]), unique(child[[id_col]]))
      
      if (length(missing_ids) == 0) {
        list(status = "passed",
             message = glue::glue("All {parent_type} records have matching {child_type}"),
             details = NULL)
      } else {
        list(status = status_for(severity),
             message = glue::glue("{length(missing_ids)} {id_col}(s) in {parent_type} have no matching {child_type}"),
             details = length(missing_ids))
      }
    }
  )
}

#' Build a "does every fishing activity have at least one catch event" rule
#' Only checks activity records where activity_type_id == 1 (fishing activity);
#' other activity types (e.g. transits) are not expected to have catch and are excluded.
#' This REPLACES make_parent_has_child_rule for log_activity_*/log_catch_* pairs, since
#' the unfiltered version would incorrectly flag non-fishing activities as missing catch.
make_fishing_activity_has_catch_rule <- function(activity_type, catch_type, id_col = "activity_id", severity = "warning") {
  new_rule(
    id          = glue::glue("{activity_type}_fishing_has_{catch_type}_ref"),
    description = glue::glue("All {activity_type} records with activity_type_id = 1 (fishing) have at least one matching {catch_type} record"),
    data_types  = c(catch_type, activity_type),
    severity    = severity,
    fn = function(datasets) {
      catch    <- datasets[[catch_type]]
      activity <- datasets[[activity_type]]
      
      stopifnot(
        "activity_type_id column missing from activity dataset" =
          "activity_type_id" %in% names(activity)
      )
      
      fishing_activity <- activity |> dplyr::filter(activity_type_id == 1)
      missing_ids <- setdiff(unique(fishing_activity[[id_col]]), unique(catch[[id_col]]))
      
      if (length(missing_ids) == 0) {
        list(status = "passed",
             message = glue::glue("All fishing {activity_type} records have matching {catch_type}"),
             details = NULL)
      } else {
        list(status = status_for(severity),
             message = glue::glue("{length(missing_ids)} fishing {id_col}(s) in {activity_type} have no matching {catch_type}"),
             details = length(missing_ids))
      }
    }
  )
}


# ── 2. Spec table (the "what" — this is what you'll actually edit) ─
cross_ref_specs <- tibble::tribble(
  ~child_type,     ~parent_type,      ~id_col,
  "agg_catch_ll",  "agg_effort_ll",   "effort_id",
  "agg_catch_ps",  "agg_effort_ps",   "effort_id",
  "agg_catch_pl",  "agg_effort_pl",   "effort_id",
  "agg_catch_tr",  "agg_effort_tr",   "effort_id",
  
  "log_catch_ll",  "log_activity_ll",    "activity_id",
  "log_activity_ll",  "log_trip_ll",      "trip_id",
  
  "log_catch_pl",  "log_activity_pl",    "activity_id",
  "log_activity_pl",  "log_trip_pl",      "trip_id",
  
  "log_catch_ps",  "log_activity_ps",    "activity_id",
  "log_activity_ps",  "log_trip_ps",      "trip_id",
  
  "log_catch_tr",  "log_activity_tr",    "activity_id",
  "log_activity_tr",  "log_trip_tr",      "trip_id",
  
)

# Pairs where the parent-has-child "warning" rule is replaced by the
# fishing-filtered version (activity_type_id == 1) instead of the generic one.
# Only log_activity_*/log_catch_* pairs qualify - log_activity_*/log_trip_*
# pairs and agg_catch_*/agg_effort_* pairs are unaffected and keep the
# generic parent-has-child warning.
fishing_activity_specs <- cross_ref_specs |>
  dplyr::filter(child_type %in% c("log_catch_ll", "log_catch_pl", "log_catch_ps", "log_catch_tr"),
                parent_type %in% c("log_activity_ll", "log_activity_pl", "log_activity_ps", "log_activity_tr"))

# ── 3. Instantiate + register ─────────────────────────────────

# error-severity "child has parent" rules: unchanged, generated for every pair
child_has_parent_rules <- purrr::pmap(cross_ref_specs, function(child_type, parent_type, id_col) {
  make_child_has_parent_rule(child_type, parent_type, id_col, severity = "error")
})

# warning-severity "parent has child" rules: generic version for every pair
# EXCEPT the logbook activity/catch pairs, which use the fishing-filtered version
generic_parent_has_child_specs <- dplyr::anti_join(
  cross_ref_specs, fishing_activity_specs,
  by = c("child_type", "parent_type", "id_col")
)

parent_has_child_rules <- purrr::pmap(generic_parent_has_child_specs, function(child_type, parent_type, id_col) {
  make_parent_has_child_rule(child_type, parent_type, id_col, severity = "warning")
})

fishing_activity_rules <- purrr::pmap(fishing_activity_specs, function(child_type, parent_type, id_col) {
  make_fishing_activity_has_catch_rule(activity_type = parent_type, catch_type = child_type, id_col = id_col)
})

cross_ref_rules <- c(child_has_parent_rules, parent_has_child_rules)

second_level_rules <- c(
  cross_ref_rules,
  fishing_activity_rules
  # one-off rules that don't fit the generic pattern go here
)