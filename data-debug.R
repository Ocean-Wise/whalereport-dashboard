####~~~~~~~~~~~~~~~~~~~~~~Data Debug & QA Checks~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Diagnostic summaries and data quality checks for all cleaned datasets
## Date written: 2025-10-29
## Usage: Source AFTER config.R, data-import.R, and data-cleaning.R

####~~~~~~~~~~~~~~~~~~~~~~Sighting Deduplication Audit~~~~~~~~~~~~~~~~~~~~~~~####

## How many duplicate sighting events were removed?
cat("\n=== Sighting Deduplication ===\n")
cat("Duplicate sighting_ids removed:  ", length(duplicate_sighting_ids), "\n")
cat("Canonical sightings kept:        ", nrow(primary_reports), "\n")

## How many alerts were linked to the dropped sighting_ids?
## These alerts will NOT appear in main_dataset.
lost_alert_ids = alert_raw %>%
  dplyr::filter(sighting_id %in% duplicate_sighting_ids) %>%
  dplyr::pull(id)

lost_deliveries = alert_user_raw %>%
  dplyr::filter(alert_id %in% lost_alert_ids) %>%
  nrow()

cat("Alerts linked to dropped duplicates:", length(lost_alert_ids), "\n")
cat("Delivery rows linked to dropped duplicates:", lost_deliveries, "\n")
cat("(these are alerts for confirmed duplicate sightings — expected to be lost)\n")

## Inspect a sample of the duplicate groups
cat("\n--- Sample duplicate groups (top 10 by group size) ---\n")
primary_reports_all = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::arrange(sighting_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

dup_groups = primary_reports_all %>%
  dplyr::filter(
    !is.na(latitude), !is.na(longitude),
    !is.na(sighting_date), !is.na(species_id)
  ) %>%
  dplyr::group_by(latitude, longitude, sighting_date, species_id) %>%
  dplyr::summarise(
    n_sighting_ids = dplyr::n(),
    sighting_ids   = paste(sort(sighting_id), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::filter(n_sighting_ids > 1) %>%
  dplyr::arrange(dplyr::desc(n_sighting_ids))

cat("Total duplicate groups found:    ", nrow(dup_groups), "\n")
cat("Total extra rows to be removed:  ", sum(dup_groups$n_sighting_ids) - nrow(dup_groups), "\n")
print(head(dup_groups, 10))
rm(primary_reports_all, dup_groups)
cat("==============================\n")

####~~~~~~~~~~~~~~~~~~~~~~Orphaned Alerts~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n=== Orphaned Alerts (no valid sighting_id) ===\n")
cat("Total orphaned alert deliveries:", nrow(orphaned_alerts), "\n")
if (nrow(orphaned_alerts) > 0) {
  cat("Date range:",
      format(min(orphaned_alerts$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
      format(max(orphaned_alerts$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("Unique alert_ids:", dplyr::n_distinct(orphaned_alerts$alert_id), "\n")
  cat("Unique users affected:", dplyr::n_distinct(orphaned_alerts$user_id), "\n")
}
cat("==============================================\n")

####~~~~~~~~~~~~~~~~~~~~~~main_dataset Summary~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n====== main_dataset Summary ======\n")
cat("Total rows (one per sighting-user, or one per sighting if no alert):", nrow(main_dataset), "\n")
cat("  - Rows with an alert:", sum(!is.na(main_dataset$alert_id)), "\n")
cat("  - Rows without an alert (sighting only):", sum(is.na(main_dataset$alert_id)), "\n")
cat("Date range:",
    format(min(main_dataset$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(main_dataset$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("Unique sightings:", dplyr::n_distinct(main_dataset$sighting_id, na.rm = TRUE), "\n")
cat("Unique alerts:", dplyr::n_distinct(main_dataset$alert_id, na.rm = TRUE), "\n")
cat("Unique recipients:", dplyr::n_distinct(main_dataset$user_id, na.rm = TRUE), "\n")

cat("\nDelivery methods (collapsed per row):\n")
print(table(main_dataset$delivery_methods, useNA = "ifany"))
cat("\nAlert status:\n")
print(table(main_dataset$status, useNA = "ifany"))
cat("\nAlert context:\n")
print(table(main_dataset$context, useNA = "ifany"))
cat("\nSource entities:\n")
print(table(main_dataset$report_source_entity, useNA = "ifany"))
cat("\nEmail sent:\n")
print(table(main_dataset$email_sent, useNA = "ifany"))
cat("\nSMS sent:\n")
print(table(main_dataset$sms_sent, useNA = "ifany"))
cat("\nIn-app sent:\n")
print(table(main_dataset$inapp_sent, useNA = "ifany"))
cat("=====================================\n")

####~~~~~~~~~~~~~~~~~~~~~~sightings_main Summary~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n====== sightings_main Summary ======\n")
cat("Total sightings:", nrow(sightings_main), "\n")
cat("Date range:",
    format(min(sightings_main$sighting_date, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(sightings_main$sighting_date, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("\nSpecies breakdown:\n")
print(table(sightings_main$species_name, useNA = "ifany"))
cat("\nSource entity breakdown:\n")
print(table(sightings_main$report_source_entity, useNA = "ifany"))
cat("\nEcotype breakdown:\n")
print(table(sightings_main$ecotype_name, useNA = "ifany"))
cat("=====================================\n")

####~~~~~~~~~~~~~~~~~~~~~~alerts_main Summary~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n====== alerts_main Summary ======\n")
cat("Total alert records (one per sighting-user, alerts only):", nrow(alerts_main), "\n")
cat("Date range:",
    format(min(alerts_main$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(alerts_main$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("Unique sightings:", dplyr::n_distinct(alerts_main$sighting_id, na.rm = TRUE), "\n")
cat("Unique recipients:", dplyr::n_distinct(alerts_main$user_id, na.rm = TRUE), "\n")
cat("\nDelivery methods:\n")
print(table(alerts_main$delivery_methods, useNA = "ifany"))
cat("=====================================\n")

####~~~~~~~~~~~~~~~~~~~~~~Data Quality Validation~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n=== DATA QUALITY CHECKS ===\n")

## Check for alerts without sightings in main_dataset
alerts_without_sightings = sum(is.na(main_dataset$sighting_id))
cat("Alert records without sightings:", alerts_without_sightings, "\n")
if (alerts_without_sightings > 0) {
  warning("Found ", alerts_without_sightings, " alert records without valid sightings")
}

## Check for missing coordinates in main_dataset
missing_coords_main = sum(is.na(main_dataset$report_latitude) | is.na(main_dataset$report_longitude))
cat("main_dataset rows missing coordinates:", missing_coords_main, "of", nrow(main_dataset), "\n")

## Check for missing coordinates in sightings_main
missing_coords_sightings = sum(is.na(sightings_main$report_latitude) | is.na(sightings_main$report_longitude))
cat("sightings_main rows missing coordinates:", missing_coords_sightings, "of", nrow(sightings_main), "\n")

## Delivery method breakdown
cat("\nRows with email only:", sum(main_dataset$email_sent & !main_dataset$sms_sent, na.rm = TRUE), "\n")
cat("Rows with SMS only:", sum(main_dataset$sms_sent & !main_dataset$email_sent, na.rm = TRUE), "\n")
cat("Rows with both email + SMS:", sum(main_dataset$email_sent & main_dataset$sms_sent, na.rm = TRUE), "\n")
cat("Rows with in-app:", sum(main_dataset$inapp_sent, na.rm = TRUE), "\n")
cat("Rows with no delivery:", sum(!main_dataset$email_sent & !main_dataset$sms_sent & !main_dataset$inapp_sent, na.rm = TRUE), "\n")

## Dataset size comparison
cat("\n=== DATASET SIZE COMPARISON ===\n")
cat("main_dataset (all delivery attempts, deduplicated per sighting-user):", nrow(main_dataset), "\n")
cat("alerts_main (focused alert columns, same rows as main_dataset):", nrow(alerts_main), "\n")
cat("sightings_main (all sightings, incl. those without alerts):", nrow(sightings_main), "\n")
cat("Unique sightings in main_dataset:", dplyr::n_distinct(main_dataset$sighting_id, na.rm = TRUE), "\n")
cat("Unique sightings in sightings_main:", dplyr::n_distinct(sightings_main$sighting_id, na.rm = TRUE), "\n")
cat("Sightings in sightings_main with no alert (alert_id is NA in main_dataset):",
    sum(is.na(main_dataset$alert_id[!duplicated(main_dataset$sighting_id)])), "\n")
cat("Sightings that triggered at least one alert:",
    dplyr::n_distinct(alerts_main$sighting_id), "\n")
cat("=====================================\n")
