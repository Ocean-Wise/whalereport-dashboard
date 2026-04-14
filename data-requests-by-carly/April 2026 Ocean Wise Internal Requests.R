####~~~~~~~~~~~~~~~~~~~~~~April 2026 Ocean Wise Internal Requests~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Date written: 2026-04-09


####~~~~~~~~~Metrics for Dalal April 9 2026~~~~~~~~~~~~~~###
##seeking cumulative WRAS detections and sightings 

###~~~~~Cumulative WRAS Detections (automated detections)~~~~###

cumulative_detect = sightings_main %>% 
  # dplyr::filter(report_source_type== "Autonomous") %>% 
  dplyr::filter(report_source_entity != "Ocean Wise Conservation Association")


cumulative_detect %>%
  dplyr::distinct(sighting_id) %>%
  nrow() ##3798 cumulative automated detections 


##just the total automated detections by year
detect_table = cumulative_detect %>%
  dplyr::group_by(sighting_year, sighting_month) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


###~~~~~Cumulative OWCA Sighting Reports~~~~###
cumulative_sightings = sightings_main %>% 
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") 

cumulative_sightings %>%
  dplyr::distinct(sighting_id) %>%
  nrow() #166,395

sightings_table = cumulative_sightings %>%
  dplyr::group_by(sighting_year, sighting_month) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


writexl::write_xlsx(
  list(
    "Detections" = detect_table,
    "Sightings" = sightings_table
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/04092026_Metrics_Request1.xlsx")


####~~~~~~Chloe wants to update dashboard with 2026 metrics~~~~~~~~~###
##Seeking WRAS Alerts (unique notifications) for Jan 1 - March 31 2026
##seeking number of new WRAS users Jan 1 - March 31 2026 (which is from salesforce)

##dates 
start_date = lubridate::as_date("2026-01-01")
end_date = lubridate::as_date("2026-03-31")

jan_to_march = main_dataset %>% 
  dplyr:: filter(alert_created_at >= start_date, alert_created_at <= end_date)

#OR 

unique_jan_to_march = alerts_main %>% 
  dplyr::filter(sighting_id %in% jan_to_march$sighting_id)

##question - both of these get the same correct? Either filtering main_dataset for the dates I want or filtering alerts_main to the sighting id's?

##both of these get 15,461 unique alerts 

##context filtering to establish that alerts may appear inflated due to ZOI notifications. 
summary_alert_type = unique_jan_to_march %>%
  dplyr::filter(
    context %in% c("current_location", "preferred_area")
  ) %>% 
  dplyr::mutate(
    context_label = dplyr::case_when(
      context == "current_location" ~ "Proximity",
      context == "preferred_area" ~ "Zone of Interest"
    )
  ) %>% 
  dplyr::group_by(year = alert_year, context_label) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

##context filtering by alert notification type (sms, email) 
summary_jan_to_march= main_dataset %>%
  dplyr:: filter(alert_created_at >= start_date, alert_created_at <= end_date) %>% 
  dplyr:: filter(delivery_successful == TRUE) %>%
  dplyr::summarise(
    total_alerts = dplyr::n(),
    unique_sightings = dplyr::n_distinct(sighting_id),
    email_only = sum(email_sent & !sms_sent),
    sms_only = sum(sms_sent & !email_sent),
    both = sum(email_sent & sms_sent))

##note we filtered push notifications out previously in the code but we should bring it back no?? 

writexl::write_xlsx(
  list(
    "Unique Notifications" = jan_to_march,
    "Alert Type" = summary_alert_type,
    "Notification Type" = summary_jan_to_march
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/04142026_Dashboard_Request.xlsx")
