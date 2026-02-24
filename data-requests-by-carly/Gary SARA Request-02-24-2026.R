##Gary Data Request on Skana Sightings and Alerts for SARA report 2025##
##Author: Carly Green
##Date: Feb 24 2026

##Gary is looking for all sightings from March 1 2025 until end of Feb 27 2026 
##Just looking for sightings and alerts via Gary, Mike, Ashley 

#Step 1 run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

#Step 2 update start date and end date 
start_date = lubridate::as_date("2025-03-01")
end_date = lubridate::as_date("2026-02-24")

View(sightings_main %>% 
       dplyr:: filter(sighting_date >= start_date, sighting_date <= end_date))

skana = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::filter(observer_email %in% c(
  "gary.sutton@ocean.org",
  "michael.judson@ocean.org",
  "ashleybachert@hotmail.com"
)) 

#Step 3 Ensure Ocean Wise Conservation Association is the Organization not just "Ocean Wise"
skana = skana %>% 
  dplyr::mutate(
    observer_organization = "Ocean Wise Conservation Association")

#Step 4 Ensure no duplicate values exist 
unique(skana$sighting_date)

skana %>%
  dplyr::distinct(sighting_date) %>%
  nrow() #this is 260 and the full table is 273

skana %>%
  dplyr::count(sighting_date) %>%
  dplyr::filter(n > 1) #there are 13 duplicate sightings

#step 5 keep only the unique sightings 
skana_clean = skana %>%
  dplyr::distinct(sighting_date, .keep_all = TRUE)

##~~~~~~~~~~~~~~##

##alerts SENT by skana individuals (In essence, how many Alerts did Gary, Ashley, Mike generate) ##


skana_alerts = main_dataset %>%
  dplyr::filter(
    report_sighting_date >= start_date,
    report_sighting_date <= end_date,
    delivery_successful == TRUE) %>%
  dplyr::semi_join(
    skana_clean,
    by = c("report_sighting_date" = "sighting_date") ##couldn't go off of vessel_name because sometimes it is NA 
  )
##this looks like 801 alerts were sent based off of the 260 sighting reports. 

##save the files when QA is complete. 

