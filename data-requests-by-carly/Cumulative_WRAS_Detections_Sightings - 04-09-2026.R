####Metrics for Dalal April 9 2026###

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


