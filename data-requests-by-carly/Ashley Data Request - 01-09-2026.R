##Ashley Data Request for HB Sightings##

##I have ran Config R, Data-import R, Data-cleaning R
##Using the sightings_main table as it contains all sightings

##Update the dates I need 

selected_dates <- lubridate::as_date(c(
  "2025-08-03",
  "2025-08-19",
  "2025-08-20",
  "2025-08-24",
  "2025-08-25",
  "2025-08-27",
  "2025-08-29",
  "2025-09-02",
  "2025-09-04"
))

sightings_main <- sightings_main %>%
  dplyr::mutate(sighting_date = as.Date(sighting_date))

ashley_sightings = sightings_main %>% 
  dplyr::filter(sighting_date %in% selected_dates) %>% 
  dplyr::filter(species_name == "Humpback whale") %>% 
  dplyr::filter(!observer_type_name== "external-org") %>% 
  dplyr::filter(!report_source_entity== "Whale Alert Alaska") %>% 
  dplyr::rename(confidence = observer_confidence) %>% 
  # dplyr::select(-c(dplyr::contains("observer"),
                   # "report_modality",
                   # "total_reports")) %>% 
  dplyr::mutate(report_source_entity = "Ocean Wise Conservation Association")

unique(ashley_sightings$report_source_entity)

##Save as csv file
write.csv(ashley_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/AB_Data_Request_Humpback_Sightings.csv", row.names = FALSE)
