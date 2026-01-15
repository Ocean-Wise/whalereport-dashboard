####~~~~~~~~~~~~~~~~~~~~~~Data Cleaning~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Clean and join all data to create main dataset
## Date written: 2025-10-29

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## This script creates the main dataset with one row per alert_user record
## It includes all relevant information about alerts, sightings, reports, users, and observers
## Should be sourced after config.R and data-import.R

####~~~~~~~~~~~~~~~~~~~~~~Step 1: Identify Primary Report per Sighting~~~~~~~~~~~~~~~~~~~~~~~####

## For each sighting, identify the earliest report by sighting_date
## This will be the "primary" report used for detailed information
primary_reports = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::arrange(sighting_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    sighting_id,
    report_id = id,
    report_created_at = created_at,
    report_sighting_date = sighting_date,
    report_observer_id = observer_id,
    report_species_id = species_id,
    report_latitude = latitude,
    report_longitude = longitude,
    report_count = count,
    report_direction = direction,
    report_location_desc = location_desc,
    report_comments = comments,
    report_source = source,
    report_modality = modality,
    report_source_type = source_type,
    report_source_entity = source_entity,
    report_confidence_id = confidence_id,
    report_count_measure_id = count_measure_id,
    report_sighting_platform_id = sighting_platform_id,
    report_sighting_range_id = sighting_range_id,
    report_ecotype_id = ecotype_id,
    report_vessel_name = vessel_name,
    report_status = status
  ) 

## Count total reports per sighting
reports_per_sighting = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::summarise(total_reports = dplyr::n())

####~~~~~~~~~~~~~~~~~~~~~~Step 2: Clean Individual Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Clean alert_user table
alert_user_clean = alert_user_raw %>%
  dplyr::select(
    alert_user_id = id,
    alert_user_created_at = created_at,
    alert_user_updated_at = updated_at,
    recipient,
    alert_id,
    user_id,
    status,
    alert_type_id,
    context
  )

## Clean alert table
alert_clean = alert_raw %>%
  dplyr::select(
    alert_id = id,
    alert_created_at = created_at,
    sighting_id
  ) %>%
  dplyr::filter(!is.na(sighting_id))

## Clean sighting table
sighting_clean = sighting_raw %>%
  dplyr::select(
    sighting_id = id,
    sighting_created_at = created_at,
    sighting_name = name,
    sighting_start,
    sighting_finish,
    sighting_species_id = species_id,
    sighting_status = status,
    sighting_code = code,
    sighting_organization_id = organization_id
  )

## Clean user table (recipient)
user_clean = user_raw %>%
  dplyr::select(
    user_id = id,
    user_firstname = firstname,
    user_lastname = lastname,
    user_email = email,
    user_phone = phone,
    user_organization = organization,
    user_auth0_id = auth0_id,
    user_experience = experience_on_water,
    user_type_id,
    user_organization_id = organization_id
  )

## Clean observer table
observer_clean = observer_raw %>%
  dplyr::select(
    observer_id = id,
    observer_user_id = user_id,
    observer_name = name,
    observer_email = email,
    observer_organization = organization,
    observer_phone = phone,
    observer_type_id
  )

## Clean observer type table
observer_type_clean = observer_type_raw %>%
  dplyr::select(
    observer_type_id = id,
    observer_type_name = name
  )

## Clean species table
species_clean = species_raw %>%
  dplyr::select(
    species_id = id,
    species_name = name,
    species_scientific_name = scientific_name,
    species_category_id = category_id,
    species_subcategory_id = subcategory_id
  )

## Clean alert type table
alert_type_clean = alert_type_raw %>%
  dplyr::select(
    alert_type_id = id,
    alert_type_name = name
  )

## Clean dictionary table for all lookups
dictionary_clean = dictionary_raw %>%
  dplyr::select(
    dictionary_id = id,
    dictionary_type_id,
    dictionary_code = code,
    dictionary_name = name,
    dictionary_description = description
  )

## Clean organization table
organization_clean = organization_raw %>%
  dplyr::select(
    organization_id = id,
    organization_name = name
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 3: Build Main Dataset~~~~~~~~~~~~~~~~~~~~~~~####

## Start with alert_user as the base (one row per delivery attempt)
main_dataset = alert_user_clean %>%
  ## Join to alert
  dplyr::left_join(
    alert_clean,
    by = "alert_id"
  ) %>%
  ## Join to sighting
  dplyr::left_join(
    sighting_clean,
    by = "sighting_id"
  ) %>%
  ## Join to primary report
  dplyr::left_join(
    primary_reports,
    by = "sighting_id"
  ) %>%
  ## Join to report count
  dplyr::left_join(
    reports_per_sighting,
    by = "sighting_id"
  ) %>%
  ## Join to user (recipient)
  dplyr::left_join(
    user_clean,
    by = "user_id"
  ) %>%
  ## Join to alert type
  dplyr::left_join(
    alert_type_clean,
    by = "alert_type_id"
  ) %>%
  ## Join to observer
  dplyr::left_join(
    observer_clean,
    by = c("report_observer_id" = "observer_id")
  ) %>%
  ## Join to observer type
  dplyr::left_join(
    observer_type_clean,
    by = "observer_type_id"
  ) %>%
  ## Join to species (from report)
  dplyr::left_join(
    species_clean,
    by = c("report_species_id" = "species_id")
  ) %>%
  ## Join to observer's user info (submitter)
  dplyr::left_join(
    user_clean,
    by = c("observer_user_id" = "user_id"),
    suffix = c("_recipient", "_submitter")
  ) %>%
  ## Join to recipient organization
  dplyr::left_join(
    organization_clean %>% dplyr::rename(recipient_org_name = organization_name),
    by = c("user_organization_id_recipient" = "organization_id")
  ) %>%
  ## Join to submitter organization
  dplyr::left_join(
    organization_clean %>% dplyr::rename(submitter_org_name = organization_name),
    by = c("user_organization_id_submitter" = "organization_id")
  )

## Join dictionary fields for human-readable values
## Confidence
main_dataset = main_dataset %>%
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, confidence_name = dictionary_name),
    by = c("report_confidence_id" = "dictionary_id")
  )

## Count measure
main_dataset = main_dataset %>%
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, count_measure_name = dictionary_name),
    by = c("report_count_measure_id" = "dictionary_id")
  )

## Sighting platform
main_dataset = main_dataset %>%
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, sighting_platform_name = dictionary_name),
    by = c("report_sighting_platform_id" = "dictionary_id")
  )

## Sighting range
main_dataset = main_dataset %>%
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, sighting_range_name = dictionary_name),
    by = c("report_sighting_range_id" = "dictionary_id")
  )

## Ecotype (CRITICAL)
main_dataset = main_dataset %>%
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, ecotype_name = dictionary_name),
    by = c("report_ecotype_id" = "dictionary_id")
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 4: Apply Filters~~~~~~~~~~~~~~~~~~~~~~~####

## Filter by date range
main_dataset = main_dataset %>%
  dplyr::filter(
    alert_user_created_at >= start_date,
    alert_user_created_at <= end_date
  )

## Filter by source entity (if source_filter is defined)
# if (length(source_filter) > 0) {
#   main_dataset = main_dataset %>%
#     dplyr::filter(report_source_entity %in% source_filter | is.na(report_source_entity))
# }

## Filter out excluded sources (e.g., BCHN/SWAG)
if (length(exclude_sources) > 0) {
  main_dataset = main_dataset %>%
    dplyr::filter(!report_source_entity %in% exclude_sources | is.na(report_source_entity))
}

## Filter out test users (if test_user_ids is populated)
if (length(test_user_ids) > 0) {
  main_dataset = main_dataset %>%
    dplyr::filter(!user_id %in% test_user_ids)
}

## Filter out 'push' notifications (not used)
main_dataset = main_dataset %>%
  dplyr::filter(alert_type_name != "push" | is.na(alert_type_name))

## Remove any completely duplicate rows
main_dataset = main_dataset %>%
  dplyr::distinct()

####~~~~~~~~~~~~~~~~~~~~~~Step 5: Add Derived Columns~~~~~~~~~~~~~~~~~~~~~~~####

## Add date/time components for easier analysis
main_dataset = main_dataset %>%
  dplyr::mutate(
    alert_year = lubridate::year(alert_user_created_at),
    alert_month = lubridate::month(alert_user_created_at),
    alert_year_month = zoo::as.yearmon(alert_user_created_at),
    alert_date = lubridate::as_date(alert_user_created_at),
    sighting_year = lubridate::year(sighting_start),
    sighting_month = lubridate::month(sighting_start),
    sighting_year_month = zoo::as.yearmon(sighting_start)
  )

## Create a combined user name for recipient and include vessel name
main_dataset = main_dataset %>%
  dplyr::mutate(
    recipient_full_name = paste(user_firstname_recipient, user_lastname_recipient),
    submitter_full_name = dplyr::case_when(
      !is.na(user_firstname_submitter) ~ paste(user_firstname_submitter, user_lastname_submitter),
      !is.na(observer_name) ~ observer_name,
      TRUE ~ "Unknown"
    ),
    vessel_name = report_vessel_name
  )

## Flag successful deliveries (sent status only)
main_dataset = main_dataset %>%
  dplyr::mutate(
    delivery_successful = status == "sent"
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 6: Aggregate Alert Types per User-Sighting~~~~~~~~~~~~~~~~~~~~~~~####

## Aggregate alert types into one row per sighting-user combination
main_dataset = main_dataset %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(sighting_id, user_id) %>%
  dplyr::mutate(
    ## Aggregate delivery methods
    delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", "),
    num_delivery_methods = dplyr::n_distinct(alert_type_name),
    sms_sent = "sms" %in% alert_type_name,
    email_sent = "email" %in% alert_type_name,
    inapp_sent = "in_app" %in% alert_type_name
  ) %>%
  dplyr::ungroup() %>%
  ## Keep only one row per sighting-user combination (first occurrence)
  dplyr::distinct(sighting_id, user_id, .keep_all = TRUE)

####~~~~~~~~~~~~~~~~~~~~~~Data Summary~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n====== main Dataset Summary ======\n")
cat("Total alert_user records:", nrow(main_dataset), "\n")
cat("Date range:", 
    format(min(main_dataset$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
    format(max(main_dataset$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
cat("Unique sightings:", dplyr::n_distinct(main_dataset$sighting_id, na.rm = TRUE), "\n")
cat("Unique alerts:", dplyr::n_distinct(main_dataset$alert_id, na.rm = TRUE), "\n")
cat("Unique recipients:", dplyr::n_distinct(main_dataset$user_id, na.rm = TRUE), "\n")
cat("Alert types:\n")
print(table(main_dataset$alert_type_name, useNA = "ifany"))
cat("\nAlert status:\n")
print(table(main_dataset$status, useNA = "ifany"))
cat("\nAlert context:\n")
print(table(main_dataset$context, useNA = "ifany"))
cat("\nSource entities:\n")
print(table(main_dataset$report_source_entity, useNA = "ifany"))
cat("=====================================\n")

####~~~~~~~~~~~~~~~~~~~~~~Create Simplified Datasets~~~~~~~~~~~~~~~~~~~~~~~####

## Create a dataset for sightings (deduplicated) - built from raw data, not main_dataset
## This captures ALL sightings, not just those that generated alerts

## Step 1: For reports WITH sighting_id, get the earliest report per sighting
sightings_with_id = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::arrange(sighting_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

# ## Step 2: For reports WITHOUT sighting_id, treat each as an individual sighting
# sightings_without_id = report_raw %>%
#   dplyr::filter(is.na(sighting_id)) %>%
#   dplyr::mutate(sighting_id = as.numeric(paste0("0000000", id)))
# 
# ## Step 3: Combine both sets
# all_sightings_reports = dplyr::bind_rows(sightings_with_id, sightings_without_id)

# ## Step 4: Count reports per sighting (for grouped sightings)
reports_count_all = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::summarise(total_reports = dplyr::n())

## Step 5: Join to get all necessary info
sightings_main = sightings_with_id %>%
  ## Join to sighting table for sighting-level info
  dplyr::left_join(
    sighting_clean,
    by = "sighting_id"
  ) %>%
  ## Join to species
  dplyr::left_join(
    species_clean,
    by = c("species_id" = "species_id")
  ) %>%
  ## Join to observer
  dplyr::left_join(
    observer_clean,
    by = c("observer_id" = "observer_id")
  ) %>%
  ## Join to observer type
  dplyr::left_join(
    observer_type_clean,
    by = "observer_type_id"
  ) %>%
  ## Join to observer's user info
  dplyr::left_join(
    user_clean,
    by = c("observer_user_id" = "user_id")
  ) %>%
  ## TEMPORARY - Recode incorrect count_measure_id to "unknown"
  dplyr::mutate(
    count_measure_id = 
      dplyr::case_when(
        count_measure_id == 1 ~ NA,
        count_measure_id == 2 ~ NA,
        TRUE ~ count_measure_id
      )
  ) %>% 
  ## Join to count_measure_id in dictionary
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, count_measure_name = dictionary_name),
    by = c("count_measure_id" = "dictionary_id")
  ) %>%
  ## Join confidence in dictionary
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, observer_confidence = dictionary_name),
    by = c("confidence_id" = "dictionary_id")
  ) %>%
  ## Join to ecotype dictionary
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, ecotype_name = dictionary_name),
    by = c("ecotype_id" = "dictionary_id")
  ) %>%
  ## Change NA Killer Whale to "Unknown"
  dplyr::mutate(
    ecotype_name = dplyr::case_when(
      species_name == "Killer whale" & is.na(ecotype_name) ~ "Unknown",
      TRUE ~ ecotype_name
    )
  ) %>% 
  ## Join to reports count
  dplyr::left_join(
    reports_count_all,
    by = "sighting_id"
  ) %>%
  ## Join sighting_platform
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, sighting_platform_name = dictionary_name),
    by = c("sighting_platform_id" = "dictionary_id")
  ) %>%
  ## Handle behaviours array
  dplyr::mutate(
    # Extract all numbers from the behaviours array string
    behaviour_ids = stringr::str_extract_all(behaviours, "\\d+")
  ) %>%
  # Unnest to create one row per behaviour ID
  tidyr::unnest(behaviour_ids, keep_empty = TRUE) %>%
  dplyr::mutate(behaviour_ids = as.integer(behaviour_ids)) %>%
  # Join to dictionary for behaviour names
  dplyr::left_join(
    dictionary_clean %>% 
      dplyr::select(dictionary_id, behaviour_name = dictionary_name),
    by = c("behaviour_ids" = "dictionary_id")
  ) %>%
  # Group back by sighting_id and collapse behaviour names
  dplyr::group_by(sighting_id) %>%
  dplyr::mutate(
    behaviour_names = dplyr::if_else(
      all(is.na(behaviour_name)),
      NA,
      paste(na.omit(unique(behaviour_name)), collapse = ", ")
    )
  ) %>%
  dplyr::ungroup() %>%
  # Keep only the first row per sighting_id
  dplyr::distinct(sighting_id, .keep_all = TRUE) %>%
  # Clean up temporary columns
  dplyr::select(-behaviour_ids, -behaviour_name) %>% 
  ## TEMPORARY - recode comments and additional properties to keep comments from either column
  dplyr::mutate(
    # Replace additional_props that start with { with NA
    additional_props = dplyr::if_else(
      stringr::str_detect(additional_props, "^\\{"),
      NA,
      additional_props
    ),
    # Merge additional_props into comments, keeping non-NA values
    comments = dplyr::case_when(
      !is.na(comments) & !is.na(additional_props) ~ paste(comments, additional_props, sep = " | "),
      !is.na(comments) ~ comments,
      !is.na(additional_props) ~ additional_props,
      TRUE ~ NA
    )
  ) %>%
  ## For ungrouped sightings, set total_reports to 1
  dplyr::mutate(
    total_reports = dplyr::if_else(is.na(total_reports), 1L, as.integer(total_reports)),
    ## create one email column that prioritizes the user email (if they're a registered user)... 
    ## but falls back to the observer email (if they're just an observer record).
    observer_email = dplyr::coalesce(user_email, observer_email),
    ## same with organization
    observer_organization = dplyr::coalesce(user_organization, observer_organization)) %>%
  ## Select and rename columns
  dplyr::select(
    sighting_id,
    report_id = id,
    sighting_date,
    sighting_code,
    species_name,
    species_scientific_name,
    ecotype_name,
    report_latitude = latitude,
    report_longitude = longitude,
    report_count = count,
    count_type = count_measure_name,
    observer_confidence,
    comments,
    behaviour = behaviour_names,
    sighting_platform_name,
    report_source_entity = source_entity,
    report_source_type = source_type,
    report_modality = modality,
    total_reports,
    observer_name,
    observer_email,
    observer_organization,
    observer_type_name
    # submitter_user_email = user_email,
  ) %>%
  ## Add date components
  dplyr::mutate(
    sighting_year = lubridate::year(sighting_date),
    sighting_month = lubridate::month(sighting_date),
    sighting_year_month = zoo::as.yearmon(sighting_date)
  ) %>%
  ## Add OW as report source
  dplyr::mutate(
    report_source_entity = tidyr::replace_na(report_source_entity, "Ocean Wise Conservation Association")
  ) %>%
  dplyr::filter(., !report_source_entity %in% exclude_sources | is.na(report_source_entity)) %>% 
  ## Add condensed source entity categorization
  # ## Apply filters
  # dplyr::filter(
  #   sighting_date >= start_date,
  #   sighting_date <= end_date
  # ) %>%
  # ## Apply source filter if defined
  # {if (length(source_filter) > 0)
  #   dplyr::filter(., report_source_entity %in% source_filter | is.na(report_source_entity))
  #   else .} %>%
  dplyr::distinct()



cat("\n====== Simplified Datasets Created ======\n")
cat("sightings_main records:", nrow(sightings_main), "\n")
cat("alerts_main records (unique sighting-user):", nrow(alerts_main), "\n")
cat("==========================================\n")

####~~~~~~~~~~~~~~~~~~~~~~Create Reporting Breakdowns~~~~~~~~~~~~~~~~~~~~~~~####

## Breakdown of sightings by source entity
sightings_by_source = sightings_main %>%
  dplyr::group_by(
    year = sighting_year,
    month = sighting_month,
    source = report_source_entity
  ) %>%
  dplyr::summarise(
    sightings_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year, month, source)

## Breakdown of unique notifications (email OR SMS) by condensed source entity
## A "unique notification" means one notification per user per sighting
## regardless of whether they received it via email, SMS, or both
notifications_by_source = alerts_main %>%
  dplyr::filter(email_sent | sms_sent) %>%
  dplyr::group_by(
    year = alert_year,
    # month = alert_month,
    source = report_source_entity
  ) %>%
  dplyr::summarise(
    unique_notifications = dplyr::n(),
    email_notifications = sum(email_sent),
    sms_notifications = sum(sms_sent),
    both_email_and_sms = sum(email_sent & sms_sent),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year,  source)


##### SANDBOX


# ## Create a dataset for unique alerts (one per sighting-user combination)
# alerts_main = main_dataset %>%
#   dplyr::filter(delivery_successful == TRUE) %>%
#   dplyr::group_by(sighting_id, user_id) %>%
#   dplyr::summarise(
#     alert_id = dplyr::first(alert_id),
#     alert_created_at = dplyr::first(alert_created_at),
#     alert_user_created_at = dplyr::first(alert_user_created_at),
#     sighting_start = dplyr::first(sighting_start),
#     species_name = dplyr::first(species_name),
#     report_source_entity = dplyr::first(report_source_entity),
#     report_latitude = dplyr::first(report_latitude),
#     report_longitude = dplyr::first(report_longitude),
#     context = dplyr::first(context),
#     alert_year = dplyr::first(alert_year),
#     alert_month = dplyr::first(alert_month),
#     alert_year_month = dplyr::first(alert_year_month),
#     .groups = "drop"
#   )
