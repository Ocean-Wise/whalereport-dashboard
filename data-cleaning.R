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
    report_status = status,
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
## Clean alert table
alert_clean = alert_raw %>%
  dplyr::select(
    alert_id = id,
    alert_created_at = created_at,
    sighting_id
  ) %>%
  dplyr::filter(!is.na(sighting_id))

## Clean alert_user table (only alerts with valid sightings)
alert_user_clean = alert_user_raw %>%
  dplyr::filter(alert_id %in% alert_clean$alert_id) %>%
  dplyr::select(
    alert_user_id = id,
    alert_user_created_at = created_at,
    alert_user_updated_at = updated_at,
    recipient,
    alert_id,
    user_id,
    status,
    alert_type_id,
    context,
    triggering_location = triggering_location_wkt
  )



####~~~~~~~~~~~~~~~~~~~~~~Handle Orphaned Alerts~~~~~~~~~~~~~~~~~~~~~~~####

## Create separate dataframe for alerts without valid sightings
## These are tracked for debugging but filtered from main analysis
orphaned_alerts = alert_user_raw %>%
  dplyr::filter(!alert_id %in% alert_clean$alert_id) %>%
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

## Print summary
cat("\n=== Orphaned Alerts (no sighting_id) ===\n")
cat("Total orphaned alert deliveries:", nrow(orphaned_alerts), "\n")
if (nrow(orphaned_alerts) > 0) {
  cat("Date range:",
      format(min(orphaned_alerts$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "to",
      format(max(orphaned_alerts$alert_user_created_at, na.rm = TRUE), "%Y-%m-%d"), "\n")
  cat("Unique alert_ids:", dplyr::n_distinct(orphaned_alerts$alert_id), "\n")
  cat("Unique users affected:", dplyr::n_distinct(orphaned_alerts$user_id), "\n")
}
cat("=========================================\n")

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

## Note: Date filtering removed - keeping all historical data
## If you need to filter by date in the future, uncomment the variables in config.R

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

## Remove any completely duplicate rows
main_dataset = main_dataset %>%
  dplyr::distinct()

#######~~~~~~~~~~~~~~~~~~~~Step 4a: Add Derived Columns~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

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

# ## Flag successful deliveries (sent status only)
main_dataset = main_dataset %>%
  dplyr::mutate(
    delivery_successful = status == "sent"
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 4b: Deduplicate Alert Delivery Methods~~~~~~~~~~~~~~~~~~~~~~~####
# 
# ## Pivot alert delivery methods from rows to columns
# ## One user can receive same alert via email AND sms (2 rows) → combine into 1 row
# main_dataset = main_dataset %>%
#   # dplyr::filter(delivery_successful == TRUE) %>%
#   dplyr::group_by(sighting_id, user_id) %>%
#   dplyr::summarise(
#     # Keep first occurrence values for single-value fields
#     alert_id = dplyr::first(alert_id),
#     alert_created_at = min(alert_created_at, na.rm = TRUE),
#     alert_user_created_at = min(alert_user_created_at, na.rm = TRUE),
#     alert_user_updated_at = dplyr::first(alert_user_updated_at),
#     recipient = dplyr::first(recipient),
#     status = dplyr::first(status),
#     context = dplyr::first(context),
# 
#     # Sighting fields
#     sighting_created_at = dplyr::first(sighting_created_at),
#     sighting_name = dplyr::first(sighting_name),
#     sighting_start = dplyr::first(sighting_start),
#     sighting_finish = dplyr::first(sighting_finish),
#     sighting_species_id = dplyr::first(sighting_species_id),
#     sighting_status = dplyr::first(sighting_status),
#     sighting_code = dplyr::first(sighting_code),
#     sighting_organization_id = dplyr::first(sighting_organization_id),
# 
#     # Report fields (from primary report)
#     report_id = dplyr::first(report_id),
#     report_created_at = dplyr::first(report_created_at),
#     report_sighting_date = dplyr::first(report_sighting_date),
#     report_observer_id = dplyr::first(report_observer_id),
#     report_species_id = dplyr::first(report_species_id),
#     report_latitude = dplyr::first(report_latitude),
#     report_longitude = dplyr::first(report_longitude),
#     report_count = dplyr::first(report_count),
#     report_direction = dplyr::first(report_direction),
#     report_location_desc = dplyr::first(report_location_desc),
#     report_comments = dplyr::first(report_comments),
#     report_source = dplyr::first(report_source),
#     report_modality = dplyr::first(report_modality),
#     report_source_type = dplyr::first(report_source_type),
#     report_source_entity = dplyr::first(report_source_entity),
#     report_confidence_id = dplyr::first(report_confidence_id),
#     report_count_measure_id = dplyr::first(report_count_measure_id),
#     report_sighting_platform_id = dplyr::first(report_sighting_platform_id),
#     report_sighting_range_id = dplyr::first(report_sighting_range_id),
#     report_ecotype_id = dplyr::first(report_ecotype_id),
#     report_vessel_name = dplyr::first(report_vessel_name),
#     report_status = dplyr::first(report_status),
#     total_reports = dplyr::first(total_reports),
# 
#     # User fields (recipient)
#     user_firstname_recipient = dplyr::first(user_firstname_recipient),
#     user_lastname_recipient = dplyr::first(user_lastname_recipient),
#     user_email_recipient = dplyr::first(user_email_recipient),
#     user_phone_recipient = dplyr::first(user_phone_recipient),
#     user_organization_recipient = dplyr::first(user_organization_recipient),
#     user_auth0_id_recipient = dplyr::first(user_auth0_id_recipient),
#     user_experience_recipient = dplyr::first(user_experience_recipient),
#     user_type_id_recipient = dplyr::first(user_type_id_recipient),
#     user_organization_id_recipient = dplyr::first(user_organization_id_recipient),
# 
#     # Observer fields
#     observer_user_id = dplyr::first(observer_user_id),
#     observer_name = dplyr::first(observer_name),
#     observer_email = dplyr::first(observer_email),
#     observer_organization = dplyr::first(observer_organization),
#     observer_phone = dplyr::first(observer_phone),
#     observer_type_id = dplyr::first(observer_type_id),
#     observer_type_name = dplyr::first(observer_type_name),
# 
#     # Species fields
#     species_name = dplyr::first(species_name),
#     species_scientific_name = dplyr::first(species_scientific_name),
#     species_category_id = dplyr::first(species_category_id),
#     species_subcategory_id = dplyr::first(species_subcategory_id),
# 
#     # User fields (submitter)
#     user_firstname_submitter = dplyr::first(user_firstname_submitter),
#     user_lastname_submitter = dplyr::first(user_lastname_submitter),
#     user_email_submitter = dplyr::first(user_email_submitter),
#     user_phone_submitter = dplyr::first(user_phone_submitter),
#     user_organization_submitter = dplyr::first(user_organization_submitter),
#     user_auth0_id_submitter = dplyr::first(user_auth0_id_submitter),
#     user_experience_submitter = dplyr::first(user_experience_submitter),
#     user_type_id_submitter = dplyr::first(user_type_id_submitter),
#     user_organization_id_submitter = dplyr::first(user_organization_id_submitter),
# 
#     # Organization fields
#     recipient_org_name = dplyr::first(recipient_org_name),
#     submitter_org_name = dplyr::first(submitter_org_name),
# 
    # # Dictionary lookups
    # confidence_name = dplyr::first(confidence_name),
    # count_measure_name = dplyr::first(count_measure_name),
    # sighting_platform_name = dplyr::first(sighting_platform_name),
    # sighting_range_name = dplyr::first(sighting_range_name),
    # ecotype_name = dplyr::first(ecotype_name),
# 
#     # PIVOT: Aggregate delivery methods into columns
#     delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", "),
#     num_delivery_methods = dplyr::n_distinct(alert_type_name),
#     email_sent = "email" %in% alert_type_name,
#     sms_sent = "sms" %in% alert_type_name,
#     inapp_sent = "in_app" %in% alert_type_name,
# 
#     .groups = "drop"
#   )


####~~~~~~~~~~~~~~~~~~~~~~Step 5: Aggregate Alert Types per User-Sighting~~~~~~~~~~~~~~~~~~~~~~~####

## Aggregate alert types into one row per sighting-user combination
main_dataset = main_dataset %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(sighting_id, user_id) %>%
  dplyr::mutate(
    ## Aggregate delivery methods
    delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", "),
    num_delivery_methods = dplyr::n_distinct(alert_type_name),
    sms_sent = "sms" %in% alert_type_name,
    email_sent = "email" %in% alert_type_name
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
    report_status = status,
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
    observer_type_name,
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

## Create a dataset for unique alerts (one per sighting-user combination)
alerts_main = main_dataset %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(sighting_id, user_id) %>%
  dplyr::summarise(
    alert_id = dplyr::first(alert_id),
    alert_created_at = dplyr::first(alert_created_at),
    alert_user_created_at = dplyr::first(alert_user_created_at),
    sighting_start = dplyr::first(sighting_start),
    species_name = dplyr::first(species_name),
    report_source_entity = dplyr::first(report_source_entity),
    report_latitude = dplyr::first(report_latitude),
    report_longitude = dplyr::first(report_longitude),
    context = dplyr::first(context),
    triggering_location = dplyr::first(triggering_location),
    alert_year = dplyr::first(alert_year),
    alert_month = dplyr::first(alert_month),
    alert_year_month = dplyr::first(alert_year_month),
    .groups = "drop"
  )



cat("\n====== Simplified Datasets Created ======\n")
cat("sightings_main records:", nrow(sightings_main), "\n")
cat("alerts_main records (unique sighting-user):", nrow(alerts_main), "\n")
cat("Note: alerts_main and main_dataset are now identical (both deduplicated)\n")
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
  dplyr::arrange(year, 
                 month,
                 source) 

## Breakdown of unique notifications (email OR SMS) by condensed source entity
## A "unique notification" means one notification per user per sighting
## regardless of whether they received it via email, SMS, or both
notifications_by_source = main_dataset %>%
  dplyr::filter(email_sent | sms_sent) %>%
  dplyr::group_by(
    year = alert_year,
    month = alert_month,
    source = report_source_entity
    # recipient_full_name
  ) %>%
  dplyr::summarise(
    unique_notifications = dplyr::n(),
    email_notifications = sum(email_sent),
    sms_notifications = sum(sms_sent),
    both_email_and_sms = sum(email_sent & sms_sent),
    .groups = "drop"
  ) %>%
  dplyr::arrange(year,  source)

###NOTE - we need to bring back push notifs.

##### SANDBOX

cat("\n====== Reporting Breakdowns Created ======\n")
cat("Sightings by source records:", nrow(sightings_by_source), "\n")
cat("Notifications by source records:", nrow(notifications_by_source), "\n")
cat("==========================================\n")

####~~~~~~~~~~~~~~~~~~~~~~Data Quality Validation~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n=== DATA QUALITY CHECKS ===\n")

## Check for alerts without sightings
alerts_without_sightings = sum(is.na(main_dataset$sighting_id))
cat("Alert records without sightings:", alerts_without_sightings, "\n")
if (alerts_without_sightings > 0) {
  warning("Found ", alerts_without_sightings, " alert records without valid sightings")
}

## Check for missing coordinates
missing_coords = sum(is.na(main_dataset$report_latitude) | is.na(main_dataset$report_longitude))
cat("Sightings missing coordinates:", missing_coords, "of", nrow(main_dataset), "\n")

## Delivery success rate
delivery_rate = mean(main_dataset$delivery_successful, na.rm = TRUE) * 100
cat("Delivery success rate:", round(delivery_rate, 1), "%\n")

## Check for duplicate alert_user_id
duplicate_alert_users = sum(duplicated(main_dataset$alert_user_id))
cat("Duplicate alert_user_id records:", duplicate_alert_users, "\n")

## Compare dataset sizes
cat("\n=== DATASET SIZE COMPARISON ===\n")
cat("main_dataset (all delivery attempts):", nrow(main_dataset), "\n")
cat("alerts_main (deduplicated):", nrow(alerts_main), "\n")
cat("sightings_main (all sightings):", nrow(sightings_main), "\n")
cat("Unique sightings in main_dataset:", dplyr::n_distinct(main_dataset$sighting_id, na.rm=TRUE), "\n")

cat("=====================================\n")

