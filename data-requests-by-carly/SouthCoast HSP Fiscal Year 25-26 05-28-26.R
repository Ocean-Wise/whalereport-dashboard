#####~~~South Coast HSP Data Request for May 2026~~##
##Author: Carly Green
##Date: 05-24-2026 

###### What is REQUIRED (all within April 1 2025 to March 31 2026)
# All sightings OWSN only [11,629]
# All alerts (unique notifications) [54, 502]
# Number of Species reported, [17 species]
# Heat Map of sightings - OWSN only (done)
# Map of sightings comparing Fiscal Years - OWSN only 
# Breakdown of alert types (% of proximity vs zone of interest) [/] #just commented these numbers in to report for context.
#######


##South Coast OWSN sightings April 1 2025 to March 31 2026
southcoast = sf::st_read("/Users/alexmitchell/Downloads/HSP_SC.geojson")

sf::st_crs(southcoast) ##no need to transform 

leaflet::leaflet() |> 
  leaflet::addTiles() |>  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = southcoast,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)


# start / end filter with PST
start_date = lubridate::ymd_hms("2025-04-01 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("2026-03-31 23:59:59", tz = "America/Los_Angeles")

# filter and clean sightings
sc_sightings = sightings_main |> 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) |>
  sf::st_filter(southcoast) |>
  sf::st_drop_geometry() |> 
  dplyr::mutate(
    # Convert everything to PST/PDT consistently
    # sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "3 mins"),
  ) |>
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date
  ) |> 
  dplyr::arrange(sighting_date) |> 
  dplyr::filter(!report_status== "rejected") |> 
  dplyr::filter(report_source_entity == "Ocean Wise Conservation Association") |>  #just OWCA for now yes? 
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) |> 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup() 


##step 3 check for duplicates 
sc_sightings |>
  dplyr::distinct(report_longitude) |>
  nrow() # slight difference but ignore.just a few hundred.

sc_sightings |> ##inspecting duplicate longitudes and they are seemingly all different sightings 
  dplyr::distinct(report_latitude) |>
  nrow()

##~~summarize species data~~##
species_table = sc_sightings |>
  dplyr::group_by(sighting_year, species_name) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

##~~summarize source~~##
sc_sightings |>
  dplyr::group_by(report_source_entity) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


##~~~~~~~~~~~~~map sightings~~~~~~~~~~~~~~~~##

##make spatial data 
sc_sightings_sf = sc_sightings |>
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

##Heat map 
leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |> 
  leaflet.extras::addHeatmap(
    data = sc_sightings_sf,
    radius = 15,
    blur = 20,
    max = 0.05
  )

##Sightings map 
mycolors = leaflet::colorFactor(
  palette = c(ocean_wise_palette),
  domain = sc_sightings$species_name
)

##map it (same as Ashley's, sighting map with species colored)
leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addCircleMarkers(
    data = sc_sightings_sf,
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
    values = sc_sightings_sf$species_name,
    title = "Species",
  )


##Sightings map comparing fiscal years -- this was requested but looks ugly on a map IMHO)

start_date = lubridate::ymd_hms("2022-04-01 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("2026-03-31 23:59:59", tz = "America/Los_Angeles")

##filter and mutate to have fiscal year column
fiscal_sc_sightings = sightings_main |> 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) |>
  sf::st_filter(southcoast) |>
  
  dplyr::mutate(
    # Convert everything to PST/PDT consistently
    # sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "3 mins"),
  ) |>
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date
  ) |> 
  dplyr::arrange(sighting_date) |> 
  dplyr::filter(!report_status== "rejected") |> 
  dplyr::filter(report_source_entity == "Ocean Wise Conservation Association") |>  #just OWCA for now yes? 
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) |> 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup()

fiscal_sc_sightings = fiscal_sc_sightings |> 
  dplyr::mutate(
    fy_year = lubridate::year(sighting_date),
    fy_year = ifelse(lubridate::month(sighting_date) >= 4,
                     fy_year,
                     fy_year -1
                     ),
    fiscal_year = paste0(fy_year, "-", fy_year +1)
    )

##year color palette
mycolors = leaflet::colorFactor(
  palette = c(ocean_wise_palette),
  domain = fiscal_sc_sightings$fiscal_year
)

leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  leaflet::addCircleMarkers(
    data = fiscal_sc_sightings,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~mycolors(fiscal_year),
    fillColor = ~mycolors(fiscal_year),
    fillOpacity = 1
  ) |> 
  leaflet::addLegend(
    "bottomleft",
    pal = mycolors,
    values = fiscal_sc_sightings$fiscal_year,
    title = "Year"
  )

##~~~~~~~~~~~~~Alerts~~~~~~~~~~~~~~~~##

sc_alerts = main_dataset |> 
  dplyr::filter(sighting_id %in% sc_sightings$sighting_id)


sc_alerts_unique = alerts_main |> 
  dplyr::filter(sighting_id %in% sc_sightings$sighting_id) |> 
  dplyr::mutate(latitude = report_latitude,
                longitude = report_longitude)

##54,502

##context filtering to establish that alerts may appear inflated. This is due to ZOI notifications. 
summary = sc_alerts_unique |>
  dplyr::filter(
    context %in% c("current_location", "preferred_area")
  ) |> 
  dplyr::mutate(
    context_label = dplyr::case_when(
      context == "current_location" ~ "Proximity",
      context == "preferred_area" ~ "Zone of Interest"
    )
  ) |> 
  dplyr::group_by(year = alert_year, context_label) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")


##map the alerts

leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
  # leaflet::addPolygons(
  #   data = northcoast,
  #   color = "#A8007E",
  #   weight = 2,
  #   fill = FALSE
  # ) |> 
  leaflet::addCircleMarkers(
    data = sc_alerts_unique,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "grey",
    fillColor = "#FFCE34",
    fillOpacity = 1)

