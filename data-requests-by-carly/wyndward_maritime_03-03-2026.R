####~~~~~~~~~~~~~~~~~~~~~~Wyndward Maritime Data Request March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for Wyndward Maritime Ltd. Seeking marine mammal sightings data in Salish Sea for Oct 2025.
## Date written: 2026-03-03

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##set the start date and end date 
start_date = lubridate::as_date("2025-10-01")
end_date = lubridate::as_date("2025-10-31")

##load the bounding box 
boundary = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Wyndward2026.geojson")

##map the boundary to ensure it looks correct
leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = boundary, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)

##convert sightings main to spatial data
sightings_sf = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

##Testing the boundary 
sightings_within = sightings_sf %>% 
  dplyr::filter(sf::st_within(., boundary, sparse = FALSE))


##filter sightings_main to the sightings within the boundary, only using OWCA data 
wyndward = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(!observer_type_name== "external-org") %>% ##remove external orgs (and whale alerta alaska and orca network)
  dplyr::filter(!report_source_entity== "Whale Alert Alaska" & !report_source_entity =="Orca Network via Conserve.io app") %>% 
  dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(boundary) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(-c(dplyr::contains("observer"), ##remove columns that are not for public 
                   "report_modality",
                   "report_id",
                   "total_reports",
                   "sighting_year_month",))

##ensure no duplicates of sighting_id 
wyndward %>%
  dplyr::distinct(sighting_id) %>%
  nrow()

##~~~~~map it~~~~~##

#create as spatial data again
wyndward_sf = wyndward %>%
   sf::st_as_sf(
     coords = c("report_longitude", "report_latitude"),
     crs = 4326,
     remove = FALSE
   )

 
#map it
leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
  leaflet::addPolygons(
    data = boundary,
    color = "blue",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = wyndward_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

##Save the table 
writexl::write_xlsx(wyndward, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/wyndward_sightings_03-04-2026.xlsx")


