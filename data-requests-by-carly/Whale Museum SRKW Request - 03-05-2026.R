####~~~~~~~~~~~~~~~~~~~~~~Whale Museum SRKW Data Request March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for Whale Museum, seeking SRKW sightings data for Jan 1 to Dec 31 2025 in the Salish Sea to update their annual Orca Master report
## Date written: 2026-03-05

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##step 1 create start and end dates -- only doing Sept 1 - Dec 31 as Dylan provided Lapis data for Jan 1 - Aug 31
start_date = lubridate::as_date("2025-09-01")
end_date = lubridate::as_date("2025-12-31")


##step 2 load the salish sea geojson file 
salish = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Whale Museum/salishsea.geojson")

##step 3 map the polygon just to see if it looks correct
leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = salish, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)

##step 4 filter the data 
srkw_whale_museum = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
  dplyr::group_by(sighting_date, report_latitude, report_longitude, observer_email) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  dplyr::filter(ecotype_name == "Southern Resident") %>% 
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(!report_status== "rejected") %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(salish) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(sighting_date, observer_email) %>%
  dplyr::rename(confidence = observer_confidence) %>% 
  dplyr::select(-c("report_modality",
                   "total_reports",
                   "behaviour",
                   "report_source_type",
                   "sighting_year_month",
                   "date",
                   "is_duplicate"))

##checking for duplicates
srkw_whale_museum %>%
  dplyr::distinct(sighting_date) %>%
  nrow()

srkw_whale_museum %>% 
  dplyr::distinct(report_latitude) %>% 
  nrow()

##~~~~upload lapis data and join lapis data to my existing table~~~~##

lapis_srkw = readxl::read_excel(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Whale Museum/lapis_srkw_2025.xlsx")

##rename confidence column and filter to the salish sea box 
lapis_srkw = lapis_srkw %>% 
  dplyr::rename(confidence = observer_confidence) %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(salish) %>% 
  sf::st_drop_geometry()

##make sighting_id column same structure for both before joining with bind_rows
lapis_srkw$sighting_id = as.character(lapis_srkw$sighting_id)
srkw_whale_museum$sighting_id = as.character(srkw_whale_museum$sighting_id)

##bind and remove more columns 
srkw_clean = dplyr::bind_rows(lapis_srkw, srkw_whale_museum) %>% 
  dplyr::select(-c("report_id",
                   "report_status",
                   "sighting_code",
                   "comments",
                   "observer_type_name"))
  

##try to map it
srkw_clean_sf = srkw_clean %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
  leaflet::addPolygons(
    data = salish,
    color = "blue",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = srkw_clean_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

##save it
writexl::write_xlsx(srkw_clean, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Whale Museum/03182026_whale_museum_srkw.xlsx")

