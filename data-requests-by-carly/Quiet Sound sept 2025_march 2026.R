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
  dplyr::arrange(sighting_date) %>% 
  dplyr::filter(!report_status== "rejected") %>% 
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) %>% 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) %>% 
  dplyr::slice(1) %>% 
  dplyr::ungroup()

