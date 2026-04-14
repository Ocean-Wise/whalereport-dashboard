##### Washington State Ferries Monthly Data Pull ####

##Author: Carly Green
##Date: Feb 4 2026 

##need to provide tables of all WS Ferry users sightings for the month
##this is a monthly request from WS Ferries


##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##update the start date as needed every month
start_date = lubridate::as_date("2026-03-01")
end_date = lubridate::as_date("2026-03-31")

View(sightings_main %>% 
       dplyr:: filter(sighting_date >= start_date, sighting_date <= end_date))
wsferries = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date)


wsferries = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date,
    stringr::str_detect(observer_organization, 
                        stringr::regex("washington state ferries|wsf", ignore_case = TRUE)
    ) | stringr::str_detect(observer_email, "wsdot") #including any ws ferries organization name OR emails
  )  

###ensuring unique categories are what we want
unique(wsferries$observer_organization) 
unique(wsferries$report_source_entity) #Just OWCA is good. 

#changing all org names to be consistent
wsferries = wsferries %>% 
  dplyr::mutate(
    observer_organization = "Washington State Ferries"
  ) %>% ##remove columns we don't want 
  dplyr::select(-c(total_reports, observer_type_name, report_modality, sighting_year_month))


##save it 
writexl::write_xlsx(wsferries, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/wsferries_mar_2026.xlsx")

