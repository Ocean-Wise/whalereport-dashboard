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

#step 1 create start and end dates 
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
  dplyr::group_by(report_longitude, observer_email_clean) %>% ##trying to continue to remove duplicates but some still remain?? 
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



cf_sightings %>%
  dplyr::distinct(report_longitude) %>%
  nrow()

duplicates = cf_sightings %>% ##inspecting the 207 duplicate longitudes and it seems like the ones that remain are all different sightings?
  dplyr::group_by(report_longitude) %>%
  dplyr::filter(dplyr::n() > 1)
