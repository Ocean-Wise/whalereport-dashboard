####~~~~~~~~~~~~~~~~~~~~~~Ashley North Coast cetacean Request March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for Prince Rupert Port Authority Report due March 31 2026. Request is for all cetacean data for 2025 in North Coast.
## Request is for Sightings and Alerts. OWSN sightings only, but all generated alerts. 
## Date written: 2026-03-05

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts


###### VISUALS REQUIRED
# All sightings, aggregated from all sources [/]
# All alerts, aggregated from all sources [/]
# Species breakdown, all sources [/]
# Map of sightings - OWSN and Whale Alert Alaska only [/]
# Map of alerts sent - all sources, based on where alert was sent [/] <<<< BASED ON SIGHTING AS NO DATA FROM 2024 ON ALERT LOCATION
# Breakdown of alert types (% of proximity vs zone of interest) [/]
#######


##load the North Coast shapefile
northcoast = sf::st_read("/Users/alexmitchell/Downloads/nc-area/North Coast Data Pull.shp")

northcoast = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/North Coast Requests/North Coast Data Pull.shp")


sf::st_crs(northcoast) ##no need to transform 

# start / end filter with PST
start_date = lubridate::ymd_hms("2025-01-01 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("2025-12-31 23:59:59", tz = "America/Los_Angeles")

# filter and clean sightings
nc_sightings = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(northcoast) %>%
  sf::st_drop_geometry() %>% 
  
  dplyr::mutate(
    # Convert everything to PST/PDT consistently
    sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "15 mins"),
  ) %>%
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date
  ) %>% 
  dplyr::arrange(sighting_date) %>% 
  dplyr::filter(!report_status== "rejected") %>% 
  # REMOVED observer_email from grouping to catch NA mismatches
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup()


##step 3 check for duplicates 
nc_sightings %>%
  dplyr::distinct(report_longitude) %>%
  nrow() # slight difference but ignore.

duplicates = nc_sightings %>% ##inspecting duplicate longitudes and they are seemingly all different sightings 
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)

##~~summarize species data~~##
nc_sightings %>%
  dplyr::group_by(sighting_year, species_name) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

##~~summarize source~~##
nc_sightings %>%
  dplyr::group_by(report_source_entity) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")



##map sightings

##make spatial data 
nc_sightings_sf = nc_sightings %>%
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
  dplyr::filter(sighting_id %in% nc_sightings$sighting_id)


nc_alerts_unique = alerts_main %>% 
  dplyr::filter(sighting_id %in% nc_sightings$sighting_id) %>% 
  dplyr::mutate(latitude = report_latitude,
                longitude = report_longitude)

##in the northcoast, 2,230 alerts sent - but we need context on ZOI based alerts 
##QUESTION - why is nc_alerts the same alerts as from nc_alerts_unique isn't alerts_main set for unique alerts only? main_dataset is just a total (inflated) number?
## ANSWER ---- FILTER USING THE SIGHTING_ID FROM YOUR SIGHTINGS LIST TO HAVE ONE SOURCE OF TRUTH.

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
  

##total of 1,252 were based on proximity and 978 were sent based on zone of interest. 

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

##save spreadsheet of sightings
# writexl::write_xlsx(nc_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Ashley Bachert/Ashley Bachert - North Coast/PRPA March 2026 Request/2026-03-23-NCsightings1.xlsx")
##Question: how can we combat this without manually going through? or do we need to manually go through..
##Question: Are we only to provide 'approved' sightings in this case? There were many summer sightings left 'waiting review' because we were waiting for databse cleaning to happen before processing all of these duplicates. 
## ANSWER - CONTEXT DEPEDANT. IN THIS CASE PRPA IS INTERESTED IN DATA IN + DATA OUT, NO QA'ED SIGHTINGS FOR A PAPER. ALL REPORT STATUS OTHER THAN REJECTED IS FINE. 
