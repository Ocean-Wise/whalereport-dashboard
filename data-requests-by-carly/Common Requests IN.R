####~~~~~~~~~~~~~~~~~~Common Data Request Super Script~~~~~~~~~~~~~~~~~~####
##Author: Carly Green
##Date: May 24 2026 
##Purpose: This script is for common data requests both external and internal to the program of the Whale Report App/WRAS. 


####~~~~~~~~~~~~~Type 1: Request for Ocean Wise (unless other entities allowed) sightings in a defined area and date range~~~~~~~~~~####

##Step 1 upload the defined area of interest (aoi) 
aoi = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/samplearea.geojson")

##Step 2 map the boundary to ensure it looks correct
leaflet::leaflet() |> 
  leaflet::addTiles() |>  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = aoi, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)
##Step 3 check if you need to transform 
sf::st_crs(aoi)

##Step 4 (if needed) transform data to wgs84
# aoi_wgs84 = sf::st_transform(aoi, 4326)


##Step 5 create start and end dates (do I need the time zone still)
start_date = lubridate::as_date("2023-01-30")
end_date = lubridate::as_date("2026-01-30")


###~~~Step 6.1 filter and clean sightings_main~~~###
##filter and clean sightings within aoi
sightings_filtered = sightings_main |> 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) |>
  sf::st_filter(aoi) |>
  sf::st_drop_geometry() |> 
  
  dplyr::mutate(
    ##Convert everything to PST/PDT consistently and rounding by 15 minutes
    sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "15 mins"),
  ) |>
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date
  ) |> 

###~~~~6.2 optional filters~~~###
  dplyr::filter(!report_status== "rejected") |>
  ##Filter for OWCA only if required, which is required when providing data tables to external parties
  dplyr::filter(report_source_entity == "Ocean Wise Conservation Association") |>  
  # dplyr::filter(!report_source_entity == "Orca Network via Conserve.io app") |> 
  ##for species specific filtering 
  # dplyr::filter(species_name == "Killer whale") |> 
  # dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) |> 

###~~~6.3 round time, clean confidence column name and remove duplicates~~~###
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) |> 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) |> 
  dplyr::slice(1) |> 
  dplyr::ungroup() |> 
  dplyr::rename(confidence = observer_confidence) |> 
  dplyr::arrange(sighting_date) 


#Step 7 check for duplicates and inspect the data table 

##checking for duplicates (can be based on longitude or latitude)
sightings_filtered |>
  dplyr::distinct(report_longitude) |>
  nrow() 

##create a table for inspecting the duplicates if needed
duplicates = sightings_filtered
dplyr::group_by(report_longitude) |>
  dplyr::filter(dplyr::n() > 1)

##~~Depending on the request, you can summarize species data, source, etc.~~##
sightings_filtered |>
  dplyr::group_by(sighting_year, species_name) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

sightings_filtered |>
  dplyr::group_by(report_source_entity) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")



####~~~~~~~~~~Step 8 COMMENT CLEANING~~~~~~####
####Question - do we need to add code in to format the historical sighting comments 


##for Alex's reformatted comments to be cleaned 
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

sightings_out = sightings_filtered |>
  
  dplyr::mutate(
    
    comments = dplyr::case_when(
      
      # If Original Comments exists,
      # keep everything after it
      stringr::str_detect(
        comments,
        stringr::regex(
          "Original Comments:",
          ignore_case = TRUE
        )
      ) ~ stringr::str_replace(
        comments,
        stringr::regex(
          ".*Original Comments:\\s*",
          ignore_case = TRUE
        ),
        ""
      ),
      
      # Otherwise, if Historical Import exists,
      # keep only "Historical Import"
      stringr::str_detect(
        comments,
        stringr::regex(
          "Historical Import",
          ignore_case = TRUE
        )
      ) ~ "Historical Import",
      
      # Otherwise keep original comment
      TRUE ~ comments
      
    ) |>
      
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


###~~~Step 9 column Select~~~###

sightings_out = sightings_out |>
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
    confidence,
    behaviour,
    comments,
    sighting_platform_name,
    report_source_entity,
    report_source_type,
    sighting_year,
    sighting_month,
  )

cat(sprintf("Final output: %d rows x %d columns\n", nrow(sightings_out), ncol(sightings_out)))



#Step 10 if needed, map it (if requested but also is good to check everything looks correct)
#create as spatial data again
sightings_filtered_sf = sightings_filtered |>
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )


#map it
leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
  # leaflet::addPolygons(
  #   data = boundary,
  #   color = "blue",
  #   weight = 2,
  #   fill = FALSE
  # ) |> 
  leaflet::addCircleMarkers(
    data = sightings_filtered_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)



####~~~~~~~~~Type 2: Running total of Sightings and Alerts sent ~~~~~~~~~~####
####~~~~~~~~~~Type 3: Organizational specific sightings data request~~~~~~####

##Or instead of formatting by request type, could format by parameters##

##~~~Date Range~~~##
start_date = lubridate::ymd_hms("yyyy-mm-dd 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("yyyy-mm-dd 23:59:59", tz = "America/Los_Angeles")

##~~~Species Selection~~~##

##~~~Geography~~~##

##~~~Data Source (i.e. organization, OWSN, autoamted sightings)~~~##

##~~~Output (table and/or map)~~~##
