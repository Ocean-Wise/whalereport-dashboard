####~~~~~~~~~~~~~~~~~~~~~~Reporting Summary Tables~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Aggregate summary tables for reporting and dashboards
## Date written: 2025-10-29
## Usage: Source AFTER config.R, data-import.R, and data-cleaning.R

####~~~~~~~~~~~~~~~~~~~~~~Sightings by Source~~~~~~~~~~~~~~~~~~~~~~~####

## Monthly sightings count broken down by source entity
sightings_by_source = sightings_main %>%
  dplyr::group_by(
    year  = sighting_year,
    month = sighting_month,
    source = report_source_entity
  ) %>%
  dplyr::summarise(
    sightings_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, month, source)

####~~~~~~~~~~~~~~~~~~~~~~Notifications by Source~~~~~~~~~~~~~~~~~~~~~~~####

## Monthly unique notifications (email OR SMS) broken down by source entity.
## A "unique notification" = one per user per sighting, regardless of
## whether they received it via email, SMS, or both.
notifications_by_source = main_dataset %>%
  dplyr::filter(email_sent | sms_sent) %>%
  dplyr::group_by(
    year   = alert_year,
    month  = alert_month,
    source = report_source_entity
  ) %>%
  dplyr::summarise(
    unique_notifications  = dplyr::n(),
    email_notifications   = sum(email_sent, na.rm = TRUE),
    sms_notifications     = sum(sms_sent, na.rm = TRUE),
    both_email_and_sms    = sum(email_sent & sms_sent, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, month, source)

cat("\n====== Reporting Breakdowns Created ======\n")
cat("sightings_by_source records:", nrow(sightings_by_source), "\n")
cat("notifications_by_source records:", nrow(notifications_by_source), "\n")
cat("==========================================\n")
