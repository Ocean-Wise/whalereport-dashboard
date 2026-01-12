####~~~~~~~~~~~~~~~~~~~~~~Data Import~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Import all necessary tables from the database
## Date written: 2025-10-29

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## This script imports raw tables from the database
## It should be sourced after config.R

####~~~~~~~~~~~~~~~~~~~~~~Import Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Core alert tables
alert_user_raw = dplyr::tbl(connect, "alert_user") %>% 
  dplyr::collect()

alert_raw = dplyr::tbl(connect, "alert") %>% 
  dplyr::collect()

alert_type_raw = dplyr::tbl(connect, "alert_type") %>% 
  dplyr::collect()

## Sighting and report tables
sighting_raw = dplyr::tbl(connect, "sighting") %>% 
  dplyr::collect()

report_raw = dplyr::tbl(connect, "report") %>% 
  dplyr::collect()

## User and observer tables
user_raw = dplyr::tbl(connect, "user") %>% 
  dplyr::collect()

observer_raw = dplyr::tbl(connect, "observer") %>% 
  dplyr::collect()

observer_type_raw = dplyr::tbl(connect, "observer_type") %>% 
  dplyr::collect()

## Species and dictionary tables
species_raw = dplyr::tbl(connect, "species") %>% 
  dplyr::collect()

dictionary_raw = dplyr::tbl(connect, "dictionary") %>% 
  dplyr::collect()

organization_raw = dplyr::tbl(connect, "organization") %>% 
  dplyr::collect()

####~~~~~~~~~~~~~~~~~~~~~~Data Summary~~~~~~~~~~~~~~~~~~~~~~~####
## Print summary of imported data for verification

cat("====== Data Import Summary ======\n")
cat("alert_user records:", nrow(alert_user_raw), "\n")
cat("alert records:", nrow(alert_raw), "\n")
cat("sighting records:", nrow(sighting_raw), "\n")
cat("report records:", nrow(report_raw), "\n")
cat("user records:", nrow(user_raw), "\n")
cat("Date range in alert_user:", 
    format(min(alert_user_raw$created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(alert_user_raw$created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("=================================\n")