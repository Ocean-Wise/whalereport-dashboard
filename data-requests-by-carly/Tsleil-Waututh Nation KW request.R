####~~~~~~~~~~~~~~~~~~~~~April 2026 Tsleil-Waututh Nation KW request~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Date written: 2026-05-08

##We need to provide##
##all OWSN Killer Whale Sightings from ALL Years 
##Use Alex's offline database 


####~~~~~~~~~~~~~~Area of Interest~~~~~~~~~~~~~~~~~~~~######
boundary = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/mapBurrardInlet.geojson")


leaflet::leaflet() |>
  leaflet::addTiles() |>  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = boundary,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)

####~~~~~~~~~~~~~~~~~~~~~~ Filter ~~~~~~~~~~~~~~~~~~~~~~~~~~~####

# 1931-2026 april 30 - OWSN only
sightings_filtered = sightings_main_new |> 
  dplyr::filter(
    report_source_entity == "Ocean Wise Conservation Association",
    species_name == "Killer whale",
    sighting_date >= as.POSIXct("1931-01-01 00:00:00", tz = "UTC"),
    sighting_date <= as.POSIXct("2026-04-30 23:59:59", tz = "UTC")
  )


cat(sprintf("Records after date + source filter: %d\n", nrow(sightings_filtered)))

# Spatial filter: convert sightings to sf and intersect with AOI
sightings_sf = sightings_filtered |> 
  dplyr::filter(!is.na(report_latitude), !is.na(report_longitude)) |> 
  dplyr::filter(!report_status == "rejected") |> 
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs    = 4326,
    remove = FALSE
  )

sightings_area = sightings_sf |>
  sf::st_filter(boundary) |>
  sf::st_drop_geometry()

cat(sprintf("Records after spatial filter (AOI): %d\n", nrow(sightings_area)))


remove_fields = c(
  "first_name",
  "last_name",
  "email",
  "processed_by",
  "how_you_heard_other",
  "experience",
  "why_in_water",
  "when_in_water",
  "how_you_heard"
)

pattern = paste0(
  "\\|?\\s*(",
  paste(remove_fields, collapse = "|"),
  ")\\s*:[^|]*"
)

sightings_area = sightings_area |>
  
  dplyr::mutate(
    
    comments_clean = comments |>
      
      # Remove selected metadata fields
      stringr::str_remove_all(
        stringr::regex(
          pattern,
          ignore_case = TRUE
        )
      ) |>
      
      # Remove any remaining email addresses
      stringr::str_remove_all(
        stringr::regex(
          "[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}",
          ignore_case = TRUE
        )
      ) |>
      
      # Remove phone numbers
      stringr::str_remove_all(
        stringr::regex(
          "\\b\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}\\b"
        )
      ) |>
      
      # Clean duplicated separators
      stringr::str_replace_all("\\|\\s*\\|", "|") |>
      
      # Remove leading/trailing separators
      stringr::str_replace_all("^\\s*\\|\\s*", "") |>
      stringr::str_replace_all("\\s*\\|\\s*$", "") |>
      
      # Remove extra line breaks
      stringr::str_replace_all("[\\r\\n]+", " ") |>
      
      # Squish repeated whitespace
      stringr::str_squish()
    
  )



####~~~~~~~~~~~~~~~~~~~~~~ Column Select ~~~~~~~~~~~~~~~~~~~~####

sightings_out = sightings_area |>
  dplyr::select(
    sighting_id,
    sighting_date,
    report_status,
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
    comments_clean,
    sighting_platform_name,
    report_source_entity,
    sighting_year,
    sighting_month,
  )

cat(sprintf("Final output: %d rows x %d columns\n",
            nrow(sightings_out), ncol(sightings_out)))



##Save it
writexl::write_xlsx(sightings_out, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/2026-05-22-Tsleil-Watouth-Nation-KW-Sightings.xlsx")



###map make sf 

sightings_sf_new = sightings_area |>
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

#map it
leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
  leaflet::addPolygons(
    data = boundary,
    color = "blue",
    weight = 2,
    fill = FALSE
  ) |> 
  leaflet::addCircleMarkers(
    data = sightings_sf_new,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)