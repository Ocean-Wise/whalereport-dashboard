####~~~~~~~~~~~~~~~~~~~~~~Ashley North Coast cetacean Request March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for Prince Rupert Port Authority Report due March 31 2026. Request is for all cetacean data for 2025 in North Coast.
## Request is for Sightings and Alerts.
## Date written: 2026-03-05

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##load the North Coast shapefile
northcoast = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/North Coast Requests/North Coast Data Pull.shp")

sf::st_crs(northcoast) ##no need to transform 

#step 1 create start and end dates 
start_date = lubridate::as_date("2025-01-01")
end_date = lubridate::as_date("2025-12-31")


##filter sightings data
nc_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::group_by(sighting_date, report_latitude, report_longitude, observer_email) %>%
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(northcoast) %>% 
  dplyr::mutate(observer_email_clean = tolower(observer_email)) %>% 
  dplyr::group_by(report_longitude, observer_email_clean) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry()


##check for duplicates 
nc_sightings %>%
  dplyr::distinct(report_longitude) %>%
  nrow()

duplicates = nc_sightings %>%
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)

##~~summarize species data~~##
summary_sightings = nc_sightings %>%
  dplyr::group_by(sighting_year, species_name) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


##map sightings

##make spatial data 
nc_sightings_sf <- nc_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

##color palette
mycolors = leaflet::colorFactor(
  palette = c(ocean_wise_palette),
  domain = nc_sightings$species_name
)

##map it 
leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  # leaflet::addPolygons(data = northcoast,
  #                      color = "red", 
  #                      fill = FALSE, 
  #                      weight = 2) %>% 
  leaflet::addCircleMarkers(
    data = nc_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~mycolors(species_name),
    fillColor = ~mycolors(species_name),
    fillOpacity = 1
    ) %>% 
  leaflet::addLegend(
    "bottomleft",
    pal = mycolors,
    values = nc_sightings_sf$species_name,
    title = "Species",
  )

##~~~~~~~~~~~~~Alerts~~~~~~~~~~~~~~~~##

nc_alerts = main_dataset %>%
  dplyr::filter(
    report_created_at >= start_date,
    report_created_at <= end_date) %>%  
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(northcoast)

nc_alerts_unique = alerts_main %>% 
  dplyr::filter(
    alert_created_at >= start_date,
    alert_created_at <= end_date,
    !is.na(report_longitude))%>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(northcoast)

##in the northcoast, 2356 alerts sent - but we need context on ZOI based alerts 
##QUESTION - why is nc_alerts the same alerts as from nc_alerts_unique isn't alerts_main set for unique alerts? main_dataset isn't? thinking about emails vs sms.

##context filtering to establish that alerts may appear inflated in ashley's map. This is due to ZOI notifications. 
summary = nc_alerts_unique %>%
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
  
##map the alerts in nc 

leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
  # leaflet::addPolygons(
  #   data = northcoast,
  #   color = "#A8007E",
  #   weight = 2,
  #   fill = FALSE
  # ) %>% 
  leaflet::addCircleMarkers(
    data = nc_alerts_unique,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)


