####~~~~~~~~~~~~~~~~~~~~~~VFPA Killer Whale Sightings Request March 2026~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Data Request for VFPA, they are seeking sightings data for Juan de Fuca and Swiftsure Bank Area June 1 - Oct 31 2025 (Biggs and SRKW)
## Date written: 2026-03-03

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts


##set the start date and end date 
start_date = lubridate::as_date("2025-06-01")
end_date = lubridate::as_date("2025-10-31")

##load the shapefile subregions 2022 (this has all subregions including swiftsure bank)
subregions = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/subregions-2022/subregions-2022-FINAL.shp") %>% 
  sf::st_zm()

sf::st_crs(subregions) ##need to transform 

subregions_wgs84 = sf::st_transform(subregions, 4326) ##transform

swiftsure1 = subregions_wgs84 %>% 
  dplyr::filter(NAME == "Swiftsure Bank")

sjdf = subregions_wgs84 %>% 
  dplyr::filter(NAME == "Juan de Fuca")

##testing mapping
leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = swiftsure1,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)


##load the shapefile 2025 slowdown areas(just has swiftsure and Haro strait slowdowns) 
slowdown_areas = sf::st_read(
  "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/2025 slowdown areas/Slowdown.shp") %>% 
  sf::st_zm()

sf::st_crs(slowdown_areas) ##don't need to transform

swiftsure2 = slowdown_areas %>% 
  dplyr::filter(Name == "Swiftsure")

##testing mapping
leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = swiftsure2,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)


##convert sightings main to spatial data
sightings_sf = sightings_main %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  dplyr::mutate(
    sighting_date = dplyr::case_when(
    stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
    TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::mutate(sighting_date = lubridate::floor_date(sighting_date, unit = "minute")) %>% 
  dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
  dplyr::distinct(sighting_date, .keep_all = TRUE) ##when I do all this date adjusting and removing duplicate sighting dates I go from 190k to 79k sightings..

##create a new column with the polygons listed in subregions 
sightings_with_polygons = sf::st_join(sightings_sf, subregions_wgs84["NAME"])

##filter for killer whales (Biggs and SRKW) and only OWCA data and remove unnecessary columns 
subregions_sightings = sightings_with_polygons %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  # dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) %>% #decided to keep all killer whales 
  dplyr::filter(NAME != "") %>% 
  dplyr::filter(NAME == "Swiftsure Bank" | NAME == "Juan de Fuca") %>% 
  dplyr::select(-c("observer_email",
                   "observer_name",
                   "observer_organization",
                   "observer_type_name",
                   "report_modality",
                   "report_id",
                   "total_reports",
                   "sighting_year_month")) %>% 
  sf::st_drop_geometry()

##remove duplicates (if any - but since adjusting sightings_sf I now see none)
subregions_sightings %>%
  dplyr::count(report_latitude, report_longitude) %>%
  dplyr::filter(n > 1)

subregions_sightings_clean = subregions_sightings %>%
  dplyr::distinct(report_latitude, report_longitude, .keep_all = TRUE)


##create another version/table but for the two slowdown areas 
sightings_with_slowdowns = sf::st_join(sightings_sf, slowdown_areas["Name"])

##filter for killer whales (Biggs and SRKW) and only OWCA data and remove unnecessary columns 
sightings_with_slowdowns = sightings_with_slowdowns %>%
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  # dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) %>% 
  dplyr::filter(Name != "") %>% 
  dplyr::filter(Name == "Swiftsure") %>% 
  dplyr::select(-c("observer_email",
                   "observer_name",
                   "observer_organization",
                   "observer_type_name",
                   "report_modality",
                   "report_id",
                   "total_reports",
                   "sighting_year_month")) %>% 
  sf::st_drop_geometry()

##remove duplicates (if any - but since adjusting sightings_sf I now see none)
sightings_with_slowdowns %>%
  dplyr::count(report_latitude, report_longitude) %>%
  dplyr::filter(n > 1)

sightings_with_slowdowns_clean = sightings_with_slowdowns %>%
  dplyr::distinct(report_latitude, report_longitude, .keep_all = TRUE)

##merge two tables into one
combined = dplyr::bind_rows(sightings_with_slowdowns_clean, subregions_sightings_clean) %>% 
  dplyr::rename("subregion" = "NAME",
                "slowdown" = "Name")
##rename NAME to subregion
##rename Name to slowdown

##Save the table 
writexl::write_xlsx(combined, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/VFPA_KW_Swift_SJDF1.xlsx")


##try to map sighting just for fun and to make sure all sightings are within the expected boundaries. 
combined_sf = combined %>% 
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE)

leaflet::leaflet() %>%
  leaflet::addTiles() %>%  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = swiftsure1,
                       color = "red", 
                       fill = FALSE, 
                       weight = 2) %>% 
  leaflet::addPolygons(data = swiftsure2,
                       color = "blue",
                       fill = FALSE,
                       weight = 2) %>%
  leaflet::addPolygons(data = sjdf,
                       color = "purple",
                       fill = FALSE,
                       weight = 2) %>% 
  leaflet::addCircleMarkers(
    data = combined_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)
