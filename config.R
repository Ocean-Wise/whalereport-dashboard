####~~~~~~~~~~~~~~~~~~~~~~Configuration~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Database connection and global configuration variables
## Date written: 2025-10-29

####~~~~~~~~~~~~~~~~~~~~~~Packages~~~~~~~~~~~~~~~~~~~~~~~####
library(magrittr)

####~~~~~~~~~~~~~~~~~~~~~~Database Connection~~~~~~~~~~~~~~~~~~~~~~~####
## Connect to the read-only database instance
connect = DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = 3306,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  ssl.ca = Sys.getenv("SSL_CA")
)

## Note: Connection must be closed at end of session with DBI::dbDisconnect(connect)

####~~~~~~~~~~~~~~~~~~~~~~Global Variables~~~~~~~~~~~~~~~~~~~~~~~####

## Date range for analysis (easily configurable)
start_date = lubridate::as_date("2019-01-01")
end_date = lubridate::today()

## Source filter - which data providers to include
# source_filter = c("Ocean Wise Conservation Association", "Orca Network via Conserve.io app", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert Alaska")

## Create a regex pattern that matches any of these to filter for data we are allowed to share.
# ocean_wise_data_only = paste(c("Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert", "testing", "BCHN/SWAG"), collapse = "|")

## Source entities to exclude from all visualizations
exclude_sources = c("BCHN/SWAG")

## Condensed source entity mapping for reporting
## Maps raw source_entity values to standardized categories
source_entity_mapping = function(source_entity) {
  dplyr::case_when(
    stringr::str_detect(source_entity,"Ocean Wise") ~ "Ocean Wise Conservation Association",
    stringr::str_detect(source_entity, "Orca Network") ~ "Orca Network via Conserve.io app",
    stringr::str_detect(source_entity, "Acartia") ~ "Orca Network via Conserve.io app",
    stringr::str_detect(source_entity, "JASCO") ~ "JASCO",
    stringr::str_detect(source_entity, "Whale Alert Alaska") ~ "Whale Alert Alaska",
    stringr::str_detect(source_entity, "WhaleSpotter") ~ source_entity,
    stringr::str_detect(source_entity, "SMRU") ~ "SMRU",
    stringr::str_detect(source_entity, "quiet") ~ source_entity,
    stringr::str_detect(source_entity, "BCHN/SWAG") ~ "BCHN/SWAG",
    TRUE ~ "Ocean Wise Conservation Association"
  )
}

## Helper function to extract source entity from historical import comments
extract_historical_source_entity = function(comments) {
  # First, try to extract "Source Entity: XXX" from comments field
  extracted = stringr::str_extract(comments, "(?<=Source Entity: )[^|]+")
  # Trim whitespace
  trimmed = stringr::str_trim(extracted)

  # If no "Source Entity:" pattern found, check for Orca Network patterns
  orca_network_detected = dplyr::case_when(
    # Check if comments contains "orca network" (case-insensitive)
    stringr::str_detect(tolower(comments), "orca network") ~ "Orca Network",
    # Check if comments contains "legacy sighting" with [Orca Network]
    stringr::str_detect(tolower(comments), "legacy sighting") &
      stringr::str_detect(comments, "\\[Orca Network\\]") ~ "Orca Network",
    # Check if comments contains "batch 23" and "Orca Network"
    stringr::str_detect(tolower(comments), "batch 23") &
      stringr::str_detect(comments, "Orca Network") ~ "Orca Network",
    TRUE ~ NA_character_
  )

  # Return the Source Entity pattern if found, otherwise check for Orca Network patterns
  dplyr::case_when(
    !is.na(trimmed) & trimmed != "" ~ trimmed,
    !is.na(orca_network_detected) ~ orca_network_detected,
    TRUE ~ NA_character_
  )
}

## Years for flexible period comparison (configurable)
## Set the years you want to compare (up to 5 years)
comparison_years = c(2024, 2025)  # Can be extended to c(2021, 2022, 2023, 2024, 2025)

## Test user filter - users to exclude from impact metrics
## (to be populated after data exploration)
test_user_ids = c()

####~~~~~~~~~~~~~~~~~~~~~~Color Palette~~~~~~~~~~~~~~~~~~~~~~~####
ocean_wise_palette = c(
  "Sun"      = "#FFCE34",
  "Kelp"     = "#A2B427",
  "Coral"    = "#A8007E",
  "Anemone"  = "#354EB1",
  "Ocean"    = "#005A7C",
  "Tide"     = "#5FCBDA",
  "Black"    = "#000000",
  "White"    = "#FFFFFF",
  "Dolphin"  = "#B1B1B1"
)

## Function to get colors for plotting
get_ocean_wise_colors = function(n) {
  if (n > length(ocean_wise_palette)) {
    warning("Not enough Ocean Wise colors — some colors will be reused.")
  }
  rep(ocean_wise_palette, length.out = n)
}

####~~~~~~~~~~~~~~~~~~~~~~Helper Functions~~~~~~~~~~~~~~~~~~~~~~~####

## Function to extract latitude from MySQL POINT type
extract_latitude = function(point_column) {
  DBI::dbGetQuery(connect, paste0("SELECT ST_Y(", point_column, ") as lat"))$lat
}

## Function to extract longitude from MySQL POINT type
extract_longitude = function(point_column) {
  DBI::dbGetQuery(connect, paste0("SELECT ST_X(", point_column, ") as lon"))$lon
}
