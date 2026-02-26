##Gary Data Request on Skana Sightings and Alerts for SARA report 2025##
##Author: Carly Green
##Date: Feb 24 2026

##Gary is looking for all sightings from March 1 2025 until end of Feb 27 2026 
##Just looking for sightings and alerts via Gary, Mike, Ashley 

#Step 1 run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

#Step 2 update start date and end date 
start_date = lubridate::as_date("2025-03-01")
end_date = lubridate::as_date("2026-02-26")


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

#step 6 only want to keep the sightings that caused alerts 
skana_clean_1 = skana_clean %>%
  dplyr::filter(sighting_id %in% skana_alerts$sighting_id) #157 sightings caused the alerts 

##~~~~~~~ALERTS~~~~~~~##

##alerts SENT by skana individuals (AKA how many Alerts did Gary, Ashley, Mike generate)##

#step 1 filter sightings main for all the unique sighting ids in skana_clean
skana_alerts = main_dataset %>% dplyr::filter(sighting_id %in% unique(skana_clean$sighting_id))

#step 2 check if this number of unique ids matches skana_clean_1
skana_alerts %>%
  dplyr::summarise(
    unique_sighting_ids = dplyr::n_distinct(sighting_id)
  )

#step 3 make tables for sightings and alerts 
table_1 = skana_clean_1 %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(count = dplyr::n())

table_2 = skana_alerts %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(count = dplyr::n())

#step 4 join the tables and change the ecotype to always be Unknown for humpback. 
final_table = dplyr::left_join(
  table_1,
  table_2,
  by = dplyr::join_by(species_name, ecotype_name)) %>% 
  dplyr::mutate(
    ecotype_name = dplyr::if_else(
      species_name == "Humpback whale" & 
        is.na(ecotype_name),"Unknown", ecotype_name)
    )

#step 5 clean up the table, fix the column names and combine the humpback rows to one row.
final_table = final_table %>% 
  dplyr::group_by(species_name, ecotype_name) %>% 
  dplyr::summarise(
    sightings = sum(count.x, na.rm = TRUE),
    alerts = sum(count.y, na.rm = TRUE),
    .groups = "drop"
  )

##save the files when QA is complete. 
##781 alerts were sent from skana 
#this was caused by 157 sightings

install.packages("writexl")
writexl::write_xlsx(
  list(
    "Skana Sightings & Alerts" = final_table
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Skana_SARA_Request.xlsx")


##SANDBOX


# impactreport= main_dataset %>% 
#   dplyr:: filter(alert_year == 2025, delivery_successful == TRUE) %>% 
#   dplyr::summarise(
#     total_alerts = dplyr::n(),
#     unique_sightings = dplyr::n_distinct(sighting_id),
#     email_only = sum(email_sent & !sms_sent),
#     sms_only = sum(sms_sent & !email_sent),
#     both = sum(email_sent & sms_sent))
