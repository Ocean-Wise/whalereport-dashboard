####~~~~~~~~~~~~~~~~~~~~~April 2026 Tsleil-Waututh Nation KW request~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Date written: 2026-05-08

##We need to provide##
##all OWSN Killer Whale Sightings from ALL Years 
##Use Alex's offline database 


####~~~~~~~~~~~~~~Area of Interest~~~~~~~~~~~~~~~~~~~~######
boundary = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/mapBurrardInlet.geojson")


leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = boundary,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)

####~~~~~~~~~~~~~~~~~~~~~~ Filter ~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# 1931-2026 april 30 - OWSN only
sightings_filtered = `sightings-main-new` %>% 
  dplyr::filter(
    report_source_entity == "Ocean Wise Conservation Association",
    species_name == "Killer whale",
    sighting_date >= as.POSIXct("1931-01-01 00:00:00", tz = "UTC"),
    sighting_date <= as.POSIXct("2026-04-30 23:59:59", tz = "UTC")
  )


cat(sprintf("Records after date + source filter: %d\n", nrow(sightings_filtered)))

# Spatial filter: convert sightings to sf and intersect with AOI
sightings_sf = sightings_filtered %>% 
  dplyr::filter(!is.na(report_latitude), !is.na(report_longitude)) %>% 
  dplyr::filter(!report_status == "rejected") %>% 
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs    = 4326,
    remove = FALSE
  )

sightings_area = sightings_sf |>
  sf::st_filter(boundary) |>
  sf::st_drop_geometry()

cat(sprintf("Records after spatial filter (AOI): %d\n", nrow(sightings_area)))


####~~~~~~~~~~~~~~~~~~~~~~ Column Select ~~~~~~~~~~~~~~~~~~~~####

##Do we take out comments?? 

sightings_out = sightings_area %>% 
  dplyr::select(
    sighting_id,
    sighting_date,
    species_name,
    species_scientific_name,
    ecotype_name,
    report_latitude,
    report_longitude,
    report_count,
    count_type,
    direction,
    observer_confidence,
    behaviour,
    sighting_platform_name,
    report_source_entity,
    sighting_year,
    sighting_month,
  )

cat(sprintf("Final output: %d rows x %d columns\n",
            nrow(sightings_out), ncol(sightings_out)))

##Save it
writexl::write_xlsx(sightings_out, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/KillerWhale_OWCA_Sightings.xlsx")

