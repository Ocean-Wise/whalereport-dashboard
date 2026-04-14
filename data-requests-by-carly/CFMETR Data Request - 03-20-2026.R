####~~~~~~~~~~~~~~~~~~~~~~Canadian Forces Data Request Melina Sorensen March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for CFMETR to support activities - looking for all OWSN sightings for 2024 and 2025 within their shapefile. 
## Date written: 2026-03-20

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##load the North Coast shapefile
aoi = sf::st_read("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Canadian Forces/CFMETR_AOI/CFMETR_AOI.shp")

sf::st_crs(aoi) ##need to transform

aoi_wgs84 = sf::st_transform(aoi, 4326)

##~~~~~~~~~using Alex's approach (same as PRPA request)~~~~~~~~~##

# start / end filter with PST
start_date = lubridate::ymd_hms("2024-01-01 00:00:00", tz = "America/Los_Angeles")
end_date   = lubridate::ymd_hms("2025-12-31 23:59:59", tz = "America/Los_Angeles")

# filter and clean sightings
cf_sightings = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>%
  sf::st_filter(aoi_wgs84) %>%
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
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(!report_status== "rejected") %>% 
  # REMOVED observer_email from grouping to catch NA mismatches
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup()

##checking for duplicates 
cf_sightings %>%
  dplyr::distinct(report_latitude) %>%
  nrow() # slight difference..

duplicates_lon = cf_sightings %>% ##inspecting duplicate longitudes as there are a few hundred
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)

##NOTE I get a couple hundred more sightings with this approach in comparison to my original approach. But still issues within the data.

##~~~~~~~~~~old code(carly's code)~~~~~~~~~~~##

#create start and end dates 
start_date = lubridate::as_date("2024-01-01")
end_date = lubridate::as_date("2025-12-31")

##filter sightings data
cf_sightings = sightings_main %>% 
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
  sf::st_filter(aoi_wgs84) %>% 
  dplyr::mutate(observer_email_clean = tolower(observer_email)) %>% 
  dplyr::group_by(report_longitude, report_latitude, observer_email_clean) %>% ##sometimes duplicates have different times due to time zone switch so not grouping by date.
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(report_latitude, observer_email_clean) %>% ##grouping it just by duplicate latitude and email (formatted all emails to lower case).
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(report_longitude, observer_email_clean) %>% ##grouping it just by duplicate longitude and email now.
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup() %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(confidence = observer_confidence) %>% 
  dplyr::select(-c(dplyr::contains("observer"),
                   "report_id",
                   "report_status",
                   "sighting_code",
                   "is_duplicate",
                   "total_reports",
                   "report_modality",
                   "report_source_type",
                   "report_source_entity",
                   "sighting_year_month"
  ))

#NOTE: some historical sightings previously with range there is no animal count. leaves the cell blank. Others will have the number but the blank will be in count_type column.
#NOTE: there are still clear duplicates - not sure how my grouping did not get rid of them ? But also some are just duplicates that are close lat/lon but not exact same.
#NOTE: still have to remove the observer specific info from comments in historical sightings if I am leaving the comment column in. 
#NOTE: noticed mapping issues still exist for the overlap period May 2025-Aug 2025. CURIOUS IF WE JUST SEND 2024 DATA FOR NOW. this request was for 2024 and she mentioned 'if possible' 2025 but perhaps I say we can send 2025 in a couple of months time? 


###inspecting for duplicates - I am still suspicious of remaining duplicates even after all that ^.
cf_sightings %>%
  dplyr::distinct(report_longitude) %>%
  nrow()

cf_sightings %>%
  dplyr::distinct(report_latitude) %>%
  nrow()

cf_sightings %>%
  dplyr::distinct(sighting_date) %>%
  nrow()

duplicates_lon = cf_sightings %>% ##inspecting duplicate longitudes and it seems like the ones that remain are all different sightings?
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)

##save the file
writexl::write_xlsx(cf_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Canadian Forces/2026-03-25-CFMETR_sightings_2024_2025.xlsx")
                    

##~~~~~~~EXTRA~~~~~~##

#create as spatial data again
cf_sightings_sf = cf_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

#map it
leaflet::leaflet() %>% 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) %>% 
  leaflet::addPolygons(
    data = aoi_wgs84,
    color = "blue",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = cf_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

#sharepoint link (was having permission issues)
##C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Whales Initiative - Data Requests/Canadian Forces/2026-03-25-CFMETR_sightings_2024_2025.xlsx")
