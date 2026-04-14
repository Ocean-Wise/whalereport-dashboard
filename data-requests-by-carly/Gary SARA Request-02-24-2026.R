##Gary Data Request on Skana Sightings and Alerts for SARA report 2025##
##Author: Carly Green
##Date: Feb 24 2026

##Gary is looking for all sightings from March 1 2025 until end of Feb 27 2026 
##Just looking for sightings and alerts via Gary, Mike, Ashley 

#Step 1 run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

#Step 2 update start date and end date 
# start_date = lubridate::as_date("2025-03-01")
# end_date = lubridate::as_date("2026-02-15")

#UPDATED step 2: use only dates Gary requested as he had encounters on these days specifically on skana
allowed_dates = c(
  lubridate::as_date(c("2025-03-01", "2025-03-06", "2025-03-13", "2025-03-18", "2025-03-25", "2025-04-11",
                                       "2025-08-19", "2025-08-20", "2025-08-21",
                       "2025-09-03", "2025-09-04", "2025-09-15", "2025-11-28", "2025-12-29", "2026-01-16")),
  seq(
    lubridate::as_date("2025-08-25"),
    lubridate::as_date("2025-08-31"),
  ),
  seq(
    lubridate::as_date("2026-01-20"),
    lubridate::as_date("2026-02-07"),
  )
)

###~~~create the data table~~~###

skana = sightings_main %>% 
  dplyr::mutate(
    sighting_date = lubridate::as_date(sighting_date)
  ) %>% 
  dplyr::filter(
    sighting_date %in% allowed_dates) %>%
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    ),
    observer_email = stringr::str_to_lower(observer_email)) %>% 
  dplyr::filter(observer_email %in% c(
  "gary.sutton@ocean.org",
  "michael.judson@ocean.org",
  "ashleybachert@hotmail.com",
  "chloe.robinson@ocean.org",
  "hannah.trotman@ocean.org",
  "hcrichards00@gmail.com",
  "olivia.heintzman@ocean.org"
)) %>% 
  dplyr::filter(report_longitude <= -115) #don't want ashley gibraltor sightings if any
  # dplyr::filter(sighting_platform_name %in% c("Motor Vessel 25-60 ft", "Motor Vessel <25 ft"), #don't want non skana sightings
  # but I can't actually use this filter yet because our data has some mapping issues. 
               

#Step 3 Ensure Ocean Wise Conservation Association is the Organization not just "Ocean Wise"
skana = skana %>% 
  dplyr::mutate(
    observer_organization = "Ocean Wise Conservation Association")

#Step 4 Ensure no duplicate values exist 
unique(skana$sighting_id)

skana %>%
  dplyr::distinct(sighting_id) %>%
  nrow() #why does the duplicates in report_lat and report_lon have diff sighting ids? makes it hard to find duplicates off this. 

skana %>%
  dplyr::count(report_latitude, report_longitude) %>%
  dplyr::filter(n > 1) #there are 9 duplicate sightings

duplicates = skana %>% 
  dplyr::group_by(report_latitude, report_longitude) %>% 
  dplyr::filter(dplyr::n() >1) %>% 
  dplyr::ungroup()

writexl::write_xlsx(duplicates, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/skana_members_duplicates.xlsx")

#step 5 keep only the unique sightings which is 145
skana_clean = skana %>%
  dplyr::distinct(report_latitude, report_longitude, .keep_all = TRUE)

#step 6 create skana_alerts by doing a filter of main_dataset for all the sighting ids in skana_clean
#GARY WANTS ALL SIGHTINGS REGARDLESS OF ALERT OR NOT
skana_alerts = main_dataset %>% dplyr::filter(sighting_id %in% skana_clean$sighting_id)

skana_alerts = main_dataset %>% dplyr::filter(sighting_id %in% unique(skana_clean$sighting_id))

#SKIP because this step is for only sightings that caused alerts
# skana_clean_1 = skana_clean %>%
# dplyr::filter(sighting_id %in% skana_alerts$sighting_id)

##~~~~~~~ALERTS~~~~~~~##

##alerts SENT by skana individuals (AKA how many Alerts did SKANA generate)##

#step 1 check if this number of unique ids matches skana_clean which is 145
skana_clean %>%
  dplyr::summarise(
    unique_sighting_ids = dplyr::n_distinct(sighting_id)
  )

#step 2 make tables for sightings and alerts 
table_1 = skana_clean %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(count = dplyr::n())

table_2 = skana_alerts %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(count = dplyr::n())

##if using sightings that caused alerts##
# table_3 = skana_clean_1 %>% 
#   dplyr::group_by(species_name, ecotype_name) %>% 
#   dplyr::summarise(count = dplyr::n())

#step 3 join the tables and change the ecotype that was not mapped properly.  
final_table = dplyr::left_join(
  table_1,
  table_2, 
  by = dplyr::join_by(species_name, ecotype_name)) %>% 
  dplyr::mutate(
    ecotype_name = dplyr::if_else(
      species_name == "Killer whale" &
        is.na(ecotype_name),"Unknown", ecotype_name))
  
  #trying to fix some of the mapping issues but Gary manually confirmed which were SRKW and which were not.  
  final_table = dplyr::left_join(
    table_1,
    table_2,
    by = dplyr::join_by(species_name, ecotype_name)
  ) %>%
  dplyr::mutate(
    ecotype_name = dplyr::case_when(
      species_name == "Killer whale" & (is.na(ecotype_name) | ecotype_name == "Unknown") ~ "Southern Resident",
      TRUE ~ ecotype_name
    )
    )
  
  ##if using the table_3 table which is sightings that caused alerts 
  # final_table1 = dplyr::left_join(
  #   table_3,
  #   table_2,
  #   by = dplyr::join_by(species_name, ecotype_name))

#step 4 clean up the table, fix the column names and combine the srkw rows. 
final_table = final_table %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(
    sightings = sum(count.x, na.rm = TRUE),
    alerts = sum(count.y, na.rm = TRUE),
    .groups = "drop"
  )

##save the files when QA is complete. 
##531 alerts were sent from Skana. 
##totals sightings were 145 and 105 of these caused alerts. 

install.packages("writexl")
writexl::write_xlsx(
  list(
    "Skana Sightings & Alerts" = final_table
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Skana_SARA_Request.xlsx")

##Gary to clean up and review the sightings pulled.
writexl::write_xlsx(
  list(
    "Skana Sightings & Alerts" = skana_clean
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Skana_all_the_sightings.xlsx")
