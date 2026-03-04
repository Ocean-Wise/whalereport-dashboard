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
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

##create a new column with the polygons listed in subregions 
sightings_with_polygons = sf::st_join(sightings_sf, subregions_wgs84["NAME"])

##filter for killer whales (Biggs and SRKW) and only OWCA data and remove unnecessary columns 
subregions_sightings_clean = sightings_with_polygons %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) %>% 
  dplyr::filter(NAME != "") %>% 
  dplyr::select(-c(dplyr::contains("observer"), ##remove columns that are not for public 
                 "report_modality",
                 "report_id",
                 "total_reports",
                 "sighting_year_month")) %>% 
  sf::st_drop_geometry()


##create another version/table but for the two slowdown areas 
sightings_with_slowdowns = sf::st_join(sightings_sf, slowdown_areas["Name"])

##filter for killer whales (Biggs and SRKW) and only OWCA data and remove unnecessary columns 
sightings_with_slowdowns_clean = sightings_with_slowdowns %>%
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(report_source_entity== "Ocean Wise Conservation Association") %>% 
  dplyr::filter(species_name == "Killer whale") %>% 
  dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name)) %>% 
  dplyr::filter(Name != "") %>% 
  dplyr::select(-c(dplyr::contains("observer"), ##remove columns that are not for public 
                   "report_modality",
                   "report_id",
                   "total_reports",
                   "sighting_year_month")) %>% 
  sf::st_drop_geometry()
  
##Save the two tables in separate sheets 
writexl::write_xlsx(
  list(
    "Sightings Data subregions" = subregions_sightings_clean,
    "Sightings Data slowdowns" = sightings_with_slowdowns_clean
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/VFPA_KillerWhale_sightings.xlsx")
