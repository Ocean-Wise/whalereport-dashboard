####~~~~~~~~~~~~~~~~~~~~~~Quiet Sound Sept 2025 to March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Date written: 2026-04-14

##Data Request: all WA state sightings from OWCA, Orca Newtork, Whale Alert, and the Whale Spotter camera in WA state. 
##also was Gonzalo looking for sept 2025 - march 2026 or just march (and April eventually) 2026?

## Step 1 load shapefile
westcoast_srkw = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - api-resources/mmr/westcoast-us-srkw-critical-habitat/WhaleKiller_SouthernResidentDPS_20210802.shp")

## Step 2 set the start date and end date 
start_date = lubridate::as_date("2025-09-14")
end_date = lubridate::as_date("2026-03-31")


## Step 3 filter and clean sightings
wmb_sightings = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(westcoast_srkw) %>%
  sf::st_drop_geometry() %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date
  ) %>% 
  dplyr::filter(report_latitude > 46.2679660) %>% ##ensuring we are just looking at WA state 
  dplyr::arrange(sighting_date) %>% 
  dplyr::filter(!report_status== "rejected") %>%
  # dplyr::group_by(report_latitude, report_longitude) %>% 
  # dplyr::mutate(
  #   is_duplicate = dplyr::n() > 1
  # ) %>% 
  # dplyr::slice(1) %>% 
  dplyr::ungroup()

## Step 4 check report_source_entity names 
unique(wmb_sightings$report_source_entity) 
##QUESTION Do we need to remove any because in the end its only capturing sightings that are in US waters right?


## Step 5 check for duplicates
duplicates = wmb_sightings %>% ##majority of these duplicates are because the cameras don't move. But worth noting. 
  dplyr::group_by(report_longitude, report_latitude) %>%
  dplyr::filter(dplyr::n() > 1)



###~~~~~~~~~~~~mapping sightings for confirmation all looks good~~~~~~~~~~~~~~~~~~~~###
#create as spatial data
wmb_sightings_sf = wmb_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

#map it
leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
  leaflet::addCircleMarkers(
    data = wmb_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

###~~~~~~~~~~~~~~~~~~~~~~~~ALERTS~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
## Step 1 filter alerts_main?
wmb_alerts_unique = alerts_main %>% 
  dplyr::filter(sighting_id %in% wmb_sightings$sighting_id) %>% 
  dplyr::mutate(latitude = report_latitude,
                longitude = report_longitude)

## Step 2 filter main_dataset (this works too?)
wmb_alerts = main_dataset %>% 
  dplyr::filter(sighting_id %in% wmb_sightings$sighting_id)


## Step 3 if we are specifying the ZOI vs proximity. summarize context of alert types  --- for this it relies on step 1
summary = wmb_alerts_unique %>%
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


