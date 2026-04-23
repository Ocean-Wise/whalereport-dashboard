####~~~~~~~~~~~~~~~~~~~~~~Data Cleaning~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Clean and join all data to create main dataset
## Date written: 2025-10-29
##
## Architecture: sighting-centric
##   main_dataset  — single source of truth (ALL sightings + alert context where applicable)
##   sightings_main — derived view: one row per sighting, no alert context
##   alerts_main    — derived view: one row per sighting-user, alert-triggered records only
##
## Pipeline:
##   Step 1    Primary reports      — one row per sighting, includes behaviours + additional_props
##   Step 1b   Sighting dedup       — drop exact duplicate sighting events (same lat/lon/datetime/species)
##   Step 2    Table cleaning       — rename and select columns on all lookup tables
##   Step 2b   Behaviour lookup     — pre-compute collapsed behaviour names per sighting
##   Step 2c   Alert pre-tidy       — pivot delivery methods on alert tables alone (fast, narrow)
##   Orphaned alerts                — separated out for debugging
##   Step 3    Build main_dataset   — primary_reports base → sighting transforms → LEFT JOIN alerts
##                                    (pure left-joins only, no groupby/summarise needed)
##   Step 4    Filters              — exclude sources, test users, exact duplicates
##   Step 4b   Date components      — add year/month columns for both sighting and alert dates
##   Step 5    sightings_main       — derived: distinct(sighting_id), sighting-focused columns
##   Step 6    alerts_main          — derived: filter(!is.na(alert_id)), alert-focused columns

## Time script run length
t_script_start = proc.time()

####~~~~~~~~~~~~~~~~~~~~~~Step 1: Identify Primary Report per Sighting~~~~~~~~~~~~~~~~~~~~~~~####

## For each sighting, pick the earliest report as the "primary" report.
## behaviours and additional_props are included here for sighting-level transforms in Step 3.
primary_reports = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::arrange(sighting_date) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(
    sighting_id,
    report_id            = id,
    report_created_at    = created_at,
    report_sighting_date = sighting_date,
    report_status        = status,
    report_observer_id   = observer_id,
    report_species_id    = species_id,
    report_latitude      = latitude,
    report_longitude     = longitude,
    report_count         = count,
    report_direction     = direction,
    report_location_desc = location_desc,
    report_comments      = comments,
    report_source        = source,
    report_modality      = modality,
    report_source_type   = source_type,
    report_source_entity = source_entity,
    report_confidence_id          = confidence_id,
    report_count_measure_id       = count_measure_id,
    report_sighting_platform_id   = sighting_platform_id,
    report_sighting_range_id      = sighting_range_id,
    report_ecotype_id             = ecotype_id,
    report_vessel_name            = vessel_name,
    behaviours,
    additional_props
  )

## Count total reports per sighting
reports_per_sighting = report_raw %>%
  dplyr::filter(!is.na(sighting_id)) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::summarise(total_reports = dplyr::n())

####~~~~~~~~~~~~~~~~~~~~~~Step 1b: Deduplicate Sightings~~~~~~~~~~~~~~~~~~~~~~~####

## Identical (lat, lon, datetime, species) → same real-world event submitted more than once.
## Keep the lowest sighting_id (the canonical/earliest record) per group.
## Rows missing any key field are kept as-is — cannot safely determine if duplicate.
##
## NOTE: alerts linked to dropped sighting_ids will not appear in main_dataset.
##       The count is tracked in duplicate_sighting_ids for debugging.

.keyed = primary_reports %>%
  dplyr::filter(
    !is.na(report_latitude),
    !is.na(report_longitude),
    !is.na(report_sighting_date),
    !is.na(report_species_id)
  )

.unkeyed = primary_reports %>%
  dplyr::filter(
    is.na(report_latitude) | is.na(report_longitude) |
    is.na(report_sighting_date) | is.na(report_species_id)
  )

.keyed_deduped = .keyed %>%
  dplyr::group_by(report_latitude, report_longitude, report_sighting_date, report_species_id) %>%
  dplyr::slice_min(sighting_id, n = 1, with_ties = FALSE) %>%
  dplyr::ungroup()

## Track dropped sighting_ids for downstream debugging
duplicate_sighting_ids = dplyr::setdiff(.keyed$sighting_id, .keyed_deduped$sighting_id)

primary_reports = .keyed_deduped

rm(.keyed, .unkeyed, .keyed_deduped)

####~~~~~~~~~~~~~~~~~~~~~~Step 2: Clean Individual Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Clean alert table
alert_clean = alert_raw %>%
  dplyr::select(
    alert_id         = id,
    alert_created_at = created_at,
    sighting_id
  ) %>%
  dplyr::filter(!is.na(sighting_id))

## Clean alert_user table (only alerts with valid sightings)
alert_user_clean = alert_user_raw %>%
  dplyr::filter(alert_id %in% alert_clean$alert_id) %>%
  dplyr::select(
    alert_user_id         = id,
    alert_user_created_at = created_at,
    alert_user_updated_at = updated_at,
    recipient,
    target_address,
    alert_id,
    user_id,
    status,
    alert_type_id,
    context,
    triggering_location = triggering_location_wkt
  )

## Clean sighting table
sighting_clean = sighting_raw %>%
  dplyr::select(
    sighting_id              = id,
    sighting_created_at      = created_at,
    sighting_name            = name,
    sighting_start,
    sighting_finish,
    sighting_species_id      = species_id,
    sighting_status          = status,
    sighting_code            = code,
    sighting_organization_id = organization_id
  )

## Clean user table
user_clean = user_raw %>%
  dplyr::select(
    user_id              = id,
    user_firstname       = firstname,
    user_lastname        = lastname,
    user_email           = email,
    user_phone           = phone,
    user_organization    = organization,
    user_auth0_id        = auth0_id,
    user_experience      = experience_on_water,
    user_type_id,
    user_organization_id = organization_id
  )

## Clean observer table
observer_clean = observer_raw %>%
  dplyr::select(
    observer_id           = id,
    observer_user_id      = user_id,
    observer_name         = name,
    observer_email        = email,
    observer_organization = organization,
    observer_phone        = phone,
    observer_type_id
  )

## Clean observer type table
observer_type_clean = observer_type_raw %>%
  dplyr::select(
    observer_type_id   = id,
    observer_type_name = name
  )

## Clean species table
species_clean = species_raw %>%
  dplyr::select(
    species_id              = id,
    species_name            = name,
    species_scientific_name = scientific_name,
    species_category_id     = category_id,
    species_subcategory_id  = subcategory_id
  )

## Clean alert type table
alert_type_clean = alert_type_raw %>%
  dplyr::select(
    alert_type_id   = id,
    alert_type_name = name
  )

## Clean dictionary table
dictionary_clean = dictionary_raw %>%
  dplyr::select(
    dictionary_id          = id,
    dictionary_type_id,
    dictionary_code        = code,
    dictionary_name        = name,
    dictionary_description = description
  )

## Clean organization table
organization_clean = organization_raw %>%
  dplyr::select(
    organization_id   = id,
    organization_name = name
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 2b: Pre-compute Behaviour Names per Sighting~~~~~~~~~~~~~~~~~~~~~~~####

## Unnest behaviour ID arrays → join to dictionary → re-collapse to one string per sighting.
## Done as a separate lookup so the main pipeline stays clean.
sighting_behaviours = primary_reports %>%
  dplyr::select(sighting_id, behaviours) %>%
  dplyr::mutate(behaviour_ids = stringr::str_extract_all(behaviours, "\\d+")) %>%
  tidyr::unnest(behaviour_ids, keep_empty = TRUE) %>%
  dplyr::mutate(behaviour_ids = as.integer(behaviour_ids)) %>%
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, behaviour_name = dictionary_name),
    by = c("behaviour_ids" = "dictionary_id")
  ) %>%
  dplyr::group_by(sighting_id) %>%
  dplyr::summarise(
    behaviour = dplyr::if_else(
      all(is.na(behaviour_name)),
      NA_character_,
      paste(na.omit(unique(behaviour_name)), collapse = ", ")
    ),
    .groups = "drop"
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 2c: Pre-tidy Alert-User Table~~~~~~~~~~~~~~~~~~~~~~~####

## Collapse delivery method rows BEFORE joining to sightings.
## alert_user_clean can have 2 rows per (alert_id, user_id) when a recipient
## receives both email AND SMS for the same alert. We pivot those into boolean
## columns here on the small alert tables alone, producing one row per
## (sighting_id, user_id). The main pipeline then needs no groupby/summarise.
alert_user_deduped = alert_user_clean %>%
  dplyr::left_join(alert_type_clean, by = "alert_type_id") %>%
  dplyr::left_join(alert_clean,      by = "alert_id") %>%
  dplyr::group_by(sighting_id, user_id) %>%
  dplyr::summarise(
    alert_id              = dplyr::first(alert_id),
    alert_user_id              = dplyr::first(alert_user_id),
    alert_created_at      = dplyr::first(alert_created_at),
    alert_user_created_at = min(alert_user_created_at, na.rm = TRUE),
    alert_user_updated_at = dplyr::first(alert_user_updated_at),
    recipient             = dplyr::first(recipient),
    target_address_email  = dplyr::first(target_address[alert_type_name == "email"]),
    target_address_sms    = dplyr::first(target_address[alert_type_name == "sms"]),
    status                = dplyr::first(status),
    context               = dplyr::first(context),
    triggering_location   = dplyr::first(triggering_location),
    delivery_methods      = dplyr::if_else(
      all(is.na(alert_type_name)),
      NA_character_,
      paste(sort(unique(na.omit(alert_type_name))), collapse = ", ")
    ),
    num_delivery_methods  = dplyr::if_else(
      all(is.na(alert_type_name)),
      0L,
      as.integer(dplyr::n_distinct(alert_type_name, na.rm = TRUE))
    ),
    email_sent = "email"  %in% alert_type_name,
    sms_sent   = "sms"    %in% alert_type_name,
    push_sent = "push" %in% alert_type_name,
    .groups = "drop"
  )

####~~~~~~~~~~~~~~~~~~~~~~Handle Orphaned Alerts~~~~~~~~~~~~~~~~~~~~~~~####

## Alerts with no valid sighting — excluded from analysis, kept for debugging.
orphaned_alerts = alert_user_raw %>%
  dplyr::filter(!alert_id %in% alert_clean$alert_id) %>%
  dplyr::select(
    alert_user_id         = id,
    alert_user_created_at = created_at,
    alert_user_updated_at = updated_at,
    recipient,
    alert_id,
    user_id,
    status,
    alert_type_id,
    context
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 3: Build main_dataset~~~~~~~~~~~~~~~~~~~~~~~####

## Base: primary_reports (one row per sighting).
## Sighting-level transforms are applied first (once per sighting, cheaply).
## alert_user_deduped is already one row per (sighting_id, user_id), so the
## left-join produces the final grain directly — no collapse step needed after.

main_dataset = primary_reports %>%

  ## --- Sighting-level transforms (run once per sighting, before alert expansion) ---

  ## Recode erroneous count_measure_id values to NA
  dplyr::mutate(
    report_count_measure_id = dplyr::case_when(
      report_count_measure_id %in% c(1, 2) ~ NA,
      TRUE ~ report_count_measure_id
    )
  ) %>%

  ## Merge additional_props into report_comments (discard raw JSON blobs)
  dplyr::mutate(
    additional_props = dplyr::if_else(
      stringr::str_detect(additional_props, "^\\{"), NA_character_, additional_props
    ),
    report_comments = dplyr::case_when(
      !is.na(report_comments) & !is.na(additional_props) ~ paste(report_comments, additional_props, sep = " | "),
      !is.na(report_comments)                            ~ report_comments,
      !is.na(additional_props)                           ~ additional_props,
      TRUE                                               ~ NA_character_
    )
  ) %>%
  dplyr::select(-behaviours, -additional_props) %>%

  ## Fill NA source entity with Ocean Wise
  dplyr::mutate(
    report_source_entity = tidyr::replace_na(report_source_entity, "Ocean Wise Conservation Association")
  ) %>%

  ## --- Sighting-level joins ---
  dplyr::left_join(sighting_clean,       by = "sighting_id") %>%
  dplyr::left_join(reports_per_sighting, by = "sighting_id") %>%
  dplyr::left_join(sighting_behaviours,  by = "sighting_id") %>%

  ## --- Alert join (already one row per sighting-user, no further collapse needed) ---
  ## LEFT JOIN: sightings without alerts retain one row with NA alert columns.
  ## Sightings with alerts expand to one row per recipient — delivery methods already pivoted.
  dplyr::left_join(alert_user_deduped, by = "sighting_id") %>%

  ## --- Alert-context joins ---
  dplyr::left_join(user_clean, by = "user_id") %>%

  ## --- Sighting observer joins ---
  dplyr::left_join(observer_clean,      by = c("report_observer_id" = "observer_id")) %>%
  dplyr::left_join(observer_type_clean, by = "observer_type_id") %>%
  dplyr::left_join(species_clean,       by = c("report_species_id" = "species_id")) %>%

  ## Join submitter (the observer's linked user account)
  ## Suffix distinguishes alert recipient (_recipient) from sighting submitter (_submitter)
  dplyr::left_join(
    user_clean,
    by     = c("observer_user_id" = "user_id"),
    suffix = c("_recipient", "_submitter")
  ) %>%

  ## --- Organization joins ---
  dplyr::left_join(
    organization_clean %>% dplyr::rename(recipient_org_name = organization_name),
    by = c("user_organization_id_recipient" = "organization_id")
  ) %>%
  dplyr::left_join(
    organization_clean %>% dplyr::rename(submitter_org_name = organization_name),
    by = c("user_organization_id_submitter" = "organization_id")
  ) %>%

  ## --- Dictionary lookups ---
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, confidence_name = dictionary_name),
    by = c("report_confidence_id" = "dictionary_id")
  ) %>%
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, count_measure_name = dictionary_name),
    by = c("report_count_measure_id" = "dictionary_id")
  ) %>%
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, sighting_platform_name = dictionary_name),
    by = c("report_sighting_platform_id" = "dictionary_id")
  ) %>%
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, sighting_range_name = dictionary_name),
    by = c("report_sighting_range_id" = "dictionary_id")
  ) %>%
  dplyr::left_join(
    dictionary_clean %>% dplyr::select(dictionary_id, ecotype_name = dictionary_name),
    by = c("report_ecotype_id" = "dictionary_id")
  ) %>%

  ## Fill unknown ecotype for killer whales
  dplyr::mutate(
    ecotype_name = dplyr::case_when(
      species_name == "Killer whale" & is.na(ecotype_name) ~ "Unknown",
      TRUE ~ ecotype_name
    )
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 4: Apply Filters~~~~~~~~~~~~~~~~~~~~~~~####

## Filter out excluded sources (e.g. BCHN/SWAG)
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

####~~~~~~~~~~~~~~~~~~~~~~Step 4b: Add Date Components~~~~~~~~~~~~~~~~~~~~~~~####

main_dataset = main_dataset %>%
  dplyr::mutate(
    # Alert date components (NA for sightings with no alert)
    alert_year       = lubridate::year(alert_user_created_at),
    alert_month      = lubridate::month(alert_user_created_at),
    alert_year_month = zoo::as.yearmon(alert_user_created_at),
    # Sighting date components
    sighting_year       = lubridate::year(report_sighting_date),
    sighting_month      = lubridate::month(report_sighting_date),
    sighting_year_month = zoo::as.yearmon(report_sighting_date)
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 5: Derive sightings_main~~~~~~~~~~~~~~~~~~~~~~~####

## One row per sighting — all sightings including those that never triggered an alert.
## Sighting-focused column selection; alert recipient columns excluded.
## observer_email and observer_organization coalesce to the submitter's registered user
## account where available, falling back to the raw observer record.
sightings_main = main_dataset %>%
  dplyr::distinct(sighting_id, .keep_all = TRUE) %>%
  dplyr::mutate(
    observer_email        = dplyr::coalesce(user_email_submitter, observer_email),
    observer_organization = dplyr::coalesce(user_organization_submitter, observer_organization)
  ) %>%
  dplyr::select(
    sighting_id,
    report_id,
    report_status,
    sighting_date        = report_sighting_date,
    sighting_code,
    species_name,
    species_scientific_name,
    ecotype_name,
    report_latitude,
    report_longitude,
    report_count,
    count_type           = count_measure_name,
    direction            = report_direction,
    observer_confidence  = confidence_name,
    comments             = report_comments,
    behaviour,
    sighting_platform_name,
    report_source_entity,
    report_source_type,
    report_modality,
    total_reports,
    observer_name,
    observer_email,
    observer_organization,
    observer_type_name,
    sighting_year,
    sighting_month,
    sighting_year_month
  )

####~~~~~~~~~~~~~~~~~~~~~~Step 6: Derive alerts_main~~~~~~~~~~~~~~~~~~~~~~~####

## One row per sighting-user combination — only records where an alert was triggered.
## Alert-focused column selection; detailed sighting columns excluded.
alerts_main = main_dataset %>%
  dplyr::filter(!is.na(alert_id)) %>%
  dplyr::select(
    sighting_id,
    user_id,
    alert_id,
    alert_user_id,
    alert_created_at,
    alert_user_created_at,
    alert_year,
    alert_month,
    alert_year_month,
    sighting_start,
    species_name,
    report_source_entity,
    report_latitude,
    report_longitude,
    context,
    triggering_location,
    delivery_methods,
    email_sent,
    sms_sent,
    push_sent,
    target_address_email,
    target_address_sms
  )

## Time how long script took to run - for improving runtime efficiency  
elapsed = (proc.time() - t_script_start)[["elapsed"]]
cat(sprintf("\ndata-cleaning.R completed in %.1fs (%.1f min)\n", elapsed, elapsed / 60))

