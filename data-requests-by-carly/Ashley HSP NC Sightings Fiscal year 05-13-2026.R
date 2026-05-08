##~~~North Coast HSP Data Request~~##

###### What is REQUIRED
# All sightings, aggregated from all sources [/] #there were 19 sightings from Orca Network I removed because we are mapping)
# All alerts, aggregated from all sources [/]
# Map of sightings - OWSN and Whale Alert Alaska only [/] #confirmed entities are just OWCA and Whale Alert
# Map of alerts sent - all sources, BASED ON SIGHTING AS NO DATA FROM 2024 ON ALERT LOCATION
# Breakdown of alert types (% of proximity vs zone of interest) [/]
#######


## North Coast OWSN sightings April 1 2025 to March 31 2026
northcoast = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/North Coast Requests/North Coast Data Pull.shp")

sf::st_crs(northcoast) ##no need to transform 

leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = northcoast,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)


# start / end filter with PST
start_date = lubridate::ymd_hms("2025-04-01 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("2026-03-31 23:59:59", tz = "America/Los_Angeles")

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
  dplyr::filter(!report_source_entity == "Orca Network via Conserve.io app") %>%  ##added
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() 
  # dplyr::group_by(lat_rnd, long_rnd) %>% ###when I do this without time_bucket I get rid of some more but I'm not pressed for THIS time.
  # dplyr::mutate(
  #   is_duplicate = dplyr::n() > 1
  # ) %>% 
  # dplyr::slice(1) %>% 
  # dplyr::ungroup()


##step 3 check for duplicates 
nc_sightings %>%
  dplyr::distinct(report_longitude) %>%
  nrow() # slight difference but ignore.

duplicates = nc_sightings %>% ##inspecting duplicate longitudes and they are seemingly all different sightings 
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)

##~~summarize species data~~##
species_table = nc_sightings %>%
  dplyr::group_by(sighting_year, species_name) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

##~~summarize source~~##
nc_sightings %>%
  dplyr::group_by(report_source_entity) %>%
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

# 1,918 sightings (but there are seemingly 72 duplicates)

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

##2,165 Alerts 

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


##total of 987 were based on proximity and 1178 were sent based on zone of interest. 

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
writexl::write_xlsx(nc_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Ashley Bachert/Ashley Bachert - North Coast/2026-05-08-NCsightings.xlsx")


writexl::write_xlsx(
  list("HSP Fiscal 25-26" = nc_sightings,
       "Species Table" = species_table),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Ashley Bachert/Ashley Bachert - North Coast/2026-05-08-NCsightings.xlsx")
