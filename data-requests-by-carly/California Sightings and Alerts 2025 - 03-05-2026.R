####~~~~~~~~~~~~~~~~~~~~~~Chloe California 2025 sightings request~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for Wyndward Maritime Ltd. Seeking marine mammal sightings data in Salish Sea for Oct 2025.
## Date written: 2026-03-05

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts


##load the bounding box 
cali = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Chloe_requests/calimap.geojson")

##set the start date and end date 
start_date = lubridate::as_date("2025-01-01")
end_date = lubridate::as_date("2025-12-31")

##~~~~Testing~~~~##
##map the boundary to ensure it looks correct
leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = cali, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2) 

##convert sightings main to spatial data
sightings_sf = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

##Testing the boundary to see if sightings exist before any date restrictions are placed 
sightings_within = sightings_sf %>% 
  dplyr::filter(sf::st_within(., cali, sparse = FALSE))

##~~~~~Sightings within California in 2025~~~~~##
cali_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
  dplyr::distinct(sighting_date, .keep_all = TRUE) %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(cali) %>% 
  sf::st_drop_geometry() 
  

##no duplicate, just 21 sightings in 2025 and this is ALL report_source_entities 
 cali_sightings %>%
  dplyr::summarise(
    unique_sighting_ids = dplyr::n_distinct(sighting_id)
  )

##prepare to map it
cali_sf = cali_sightings %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = cali, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2) %>%
  leaflet::addCircleMarkers(
    data = cali_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)
  
##~~~~~~~~Alerts in California 2025~~~~~~~##

##filter main_dataset for latitude and longitude within the cali box 
cali_alerts = main_dataset %>%
  dplyr::filter(
    report_created_at >= start_date,
    report_created_at <= end_date) %>%  
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(cali)  ##we can see 2 alerts, one for gary, one for jess. 

##which of the sightings caused alerts 
cali_sightings_caused_alert = cali_sightings %>% 
  dplyr::filter(sighting_id %in% cali_alerts$sighting_id) ##these 2 alerts were caused by chloe

##remaining question - what are the California WRAS accounts 
