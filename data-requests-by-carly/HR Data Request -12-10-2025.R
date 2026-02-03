###Hannah's Data  Request###
##Author: Carly Green
##Date: December 10 2025

##I have ran Config R, Data-import R, Data-cleaning R
##Using the sightings_main table primarily
##I have updated my start and end date to May 1 - Oct 31 2025


##create box polygon based on 4 lat/lon points
library(sf)

box_coords = matrix(
  c(-127.813334, 51.166430,   # Upper Left
    -125.788948, 50.708319,   # Upper Right
    -125.961410, 50.381710,   # Lower Right
    -128.037749, 50.926107,   # Lower Left
    -127.813334, 51.166430    # Close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_poly = sf::st_polygon(list(box_coords)) |>
                            sf::st_sfc(crs = 4326)

##create Hannah's sighting request
hannah_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::filter(species_name == "Humpback whale") %>% 
  dplyr::filter(!observer_type_name== "external-org") %>% 
  dplyr::filter(!report_source_entity== "Whale Alert Alaska") %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(box_poly) %>% 
  sf::st_drop_geometry() %>%
  dplyr::select(-c(dplyr::contains("observer"),
                   "report_modality",
                   "report_count",
                   "ecotype_name")) %>% 
  dplyr::mutate(report_source_entity = "Ocean Wise Conservation Association") %>% 
  dplyr::mutate(
    comments = dplyr::case_when(
      stringr::str_detect(comments, "Alaska") ~ "remove",
      stringr::str_detect(comments, "Historical") ~ NA_character_,
      TRUE ~ comments
    )
  ) %>% 
  dplyr::filter(comments!= "remove"| is.na(comments))

            
Historical Import | Source: WRAS | Original WRAS ID: 2cb98514-7789-4fbf-909d-f39fea08a18d | Sighting Code: H89 | Distance: 300-1000m (328yd/984ft-1093yd/3280ft)
Historical Import | Source: WhaleReport App | Original ID: 07946b4e-c8a3-4e36-934b-33dfdbe8f1d9 | Modality: mobile app | Observer: michael stadnyk | Original Comments: -
  

##save file                          
write.csv(Hannah_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/HR_Data_Humpback.csv", row.names = FALSE)

##try to map 
library(leaflet)
library(sf)

leaflet() %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
  leaflet::addPolygons(
    data = box_poly,
    color = "#A8007E",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = Hannah_sightings,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)


##Another version with bigger box

hannah_box = matrix(
  c(-127.813334, 51.166430,   # Upper Left
    -125.788948, 50.708319,   # Upper Right
    -125.964111, 50.362143,   # Lower Right
    -128.113224, 50.826111,   # Lower Left
    -127.813334, 51.166430    # Close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_polynew = sf::st_polygon(list(hannah_box)) |>
  sf::st_sfc(crs = 4326)


hannah_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::filter(species_name == "Humpback whale") %>% 
  dplyr::filter(!observer_type_name== "external-org") %>% 
  dplyr::filter(!report_source_entity== "Whale Alert Alaska") %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(box_polynew) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(confidence = observer_confidence) %>% 
  dplyr::select(-c(dplyr::contains("observer"),
                   "report_modality",
                   "total_reports",
                   "ecotype_name")) %>% 
  dplyr::mutate(report_source_entity = "Ocean Wise Conservation Association") %>% 
  dplyr::mutate(
    comments = dplyr::case_when(
      stringr::str_detect(comments, "Alaska") ~ "remove",
      stringr::str_detect(comments, "Historical") ~ NA_character_,
      TRUE ~ comments
    )
  ) %>% 
  dplyr::filter(comments!= "remove"| is.na(comments))

###try to map it
##put back geometry
hannah_sightings_sf <- hannah_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

library(leaflet)
library(sf)

leaflet() %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
  leaflet::addPolygons(
    data = box_polynew,
    color = "blue",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = hannah_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)


writexl::write_xlsx(hannah_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/HR_12-12-2025.xlsx")

