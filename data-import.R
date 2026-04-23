####~~~~~~~~~~~~~~~~~~~~~~Data Import~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Import all necessary tables from the database
## Date written: 2025-10-29
##
## Each table prints its name, row count, and elapsed time as it completes.
## flush.console() ensures the table name appears immediately so you can see
## which query is running rather than waiting for a block to finish.
##
## Full diagnostic summaries are in data-debug.R — source that separately.

####~~~~~~~~~~~~~~~~~~~~~~Import Helper~~~~~~~~~~~~~~~~~~~~~~~####

## Wraps collect() with per-table timing and immediate console feedback.
## cols:    optional character vector of column names — pushed to SQL before collect()
##          to avoid transferring heavy columns (geometry, WKT, BLOBs) over the wire.
## post_fn: optional function applied to the tibble after collect().
.import_table = function(con, table_name, cols = NULL, post_fn = NULL) {
  cat(sprintf("  %-28s", paste0(table_name, " ...")))
  flush.console()
  t = proc.time()
  tbl = dplyr::tbl(con, table_name)
  if (!is.null(cols)) tbl = dplyr::select(tbl, dplyr::all_of(cols))
  result = dplyr::collect(tbl)
  if (!is.null(post_fn)) result = post_fn(result)
  elapsed = round((proc.time() - t)[["elapsed"]], 1)
  cat(sprintf("%7d rows  (%.1fs)\n", nrow(result), elapsed))
  result
}

####~~~~~~~~~~~~~~~~~~~~~~Import Tables~~~~~~~~~~~~~~~~~~~~~~~####

t_import_start = proc.time()
cat("\n====== Data Import ======\n")

## Core alert tables
alert_user_raw = .import_table(connect, "alert_user")
alert_raw      = .import_table(connect, "alert")
alert_type_raw = .import_table(connect, "alert_type")

## Sighting and report tables
## Only the 9 columns used downstream — avoids pulling any geometry/spatial columns.
## If you add columns to sighting_clean, add them here too.
sighting_raw = .import_table(connect, "sighting", cols = c(
  "id", "created_at", "name", "sighting_start", "sighting_finish",
  "species_id", "status", "code", "organization_id"
))
report_raw   = .import_table(connect, "report", post_fn = function(df) {
  df %>%
    dplyr::mutate(
      historical_source_entity = extract_historical_source_entity(comments),
      source_entity = dplyr::if_else(
        !is.na(historical_source_entity),
        historical_source_entity,
        source_entity
      ),
      source_entity = source_entity_mapping(source_entity)
    ) %>%
    dplyr::select(-historical_source_entity)
})

## User and observer tables
user_raw          = .import_table(connect, "user")
observer_raw      = .import_table(connect, "observer")
observer_type_raw = .import_table(connect, "observer_type")

## Species and dictionary tables
species_raw      = .import_table(connect, "species")
dictionary_raw   = .import_table(connect, "dictionary")
organization_raw = .import_table(connect, "organization")

cat(sprintf(
  "=========================\nTotal import time: %.1fs\n",
  (proc.time() - t_import_start)[["elapsed"]]
))

rm(.import_table, t_import_start)
