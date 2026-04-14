##Caitlin Belz Data Request
##Author: Carly Green
##Date Dec 10 2025


##I have ran Config R, Data-import R, Data-cleaning R
##Using the sightings_main table as it contains all sightings

##Update the start date
start_date = lubridate::as_date("2019-01-01")
end_date = lubridate::as_date("2025-12-01")


##create box polygon based on 4 lat/lon points
library(sf)

box_coord = matrix(
  c(-123.626223, 49.380436,   # Upper Left
    -123.294225, 49.300863,   # Upper Right
    -123.076070, 49.472660,   # Lower Right
    -123.310315, 49.672660,   # Lower Left
    -123.626223, 49.380436    # Close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_poly = sf::st_polygon(list(box_coord)) |>
  sf::st_sfc(crs = 4326)

##create Caitlin's sightings request

##set confidence values
confidence_values = c("High Chance", "Certain", "Low Chance", "Uncertain")
count_values = c("Exact", "Range", "Approximate")

caitlin_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>%
  dplyr::filter(!observer_type_name== "external-org") %>% 
  dplyr::filter(!report_source_entity== "Whale Alert Alaska") %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(box_poly) %>%
  sf::st_drop_geometry() %>%
  dplyr::rename(confidence = observer_confidence) %>% 
  dplyr::select(-c(dplyr::contains("observer"),
                   "report_modality",
                   "total_reports")) %>% 
  dplyr::mutate(report_source_entity = "Ocean Wise Conservation Association") %>% 
  dplyr::mutate(
    confidence = dplyr::if_else(
      is.na(confidence) & sighting_platform_name %in% confidence_values,
      sighting_platform_name,
      confidence
    ),
    sighting_platform_name = dplyr::if_else(
      sighting_platform_name %in% confidence_values,
      NA_character_,
      sighting_platform_name
    )
  ) %>% 
  dplyr::mutate(
    count_type = dplyr::if_else(
      is.na(count_type) & sighting_platform_name %in% count_values,
      sighting_platform_name,
      count_type
    ),
    sighting_platform_name = dplyr::if_else(
      sighting_platform_name %in% count_values,
      NA_character_,
      sighting_platform_name
    )
  ) %>% 
  dplyr::mutate(
    comments = dplyr::case_when(
      stringr::str_detect(comments, "Alaska") ~ "remove",
      stringr::str_detect(comments, "Historical") ~ NA_character_,
      TRUE ~ comments
    )
  ) 

##carly attempting to clean up the sighting platform name
  dplyr::filter(comments!= "remove"| is.na(comments)) %>% 
  dplyr::mutate(
    sighting_platform_name = dplyr::case_when(
      stringr::str_detect(sighting_platform_name, "High Chance") ~ NA_character_,
      stringr::str_detect(sighting_platform_name, "Low Chance") ~ NA_character_,
      stringr::str_detect(sighting_platform_name, "Range") ~ NA_character_,
      stringr::str_detect(sighting_platform_name, "Exact") ~ NA_character_,
      stringr::str_detect(sighting_platform_name, "Uncertain") ~ NA_character_,
      TRUE ~ sighting_platform_name
    )
  )

##checking uniqe source entities 
unique(caitlin_sightings$report_source_entity)

##checking unique sighting platform name
unique(caitlin_sightings$sighting_platform_name)


##save file                          
writexl::write_xlsx(caitlin_sightings, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/CB_Data_12-15-2025.xlsx")

##try to map 

##put back geometry
caitlin_sightings_sf <- caitlin_sightings %>%
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
    data = box_poly,
    color = "#A8007E",
    weight = 2,
    fill = FALSE
  ) %>% 
  leaflet::addCircleMarkers(
    data = caitlin_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

##Try to map it with different species 
  
  species_pal = leaflet::colorFactor(
    palette = ocean_wise_palette,
    domain = caitlin_sightings_sf$species_name
  )

# Build the leaflet map
leaflet() %>% 
  leaflet::addProviderTiles(providers$OpenStreetMap) %>% 
  leaflet::addCircleMarkers(
    data = caitlin_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~species_pal(species_name),      # border color
    fillColor = ~species_pal(species_name),  # fill color
    fillOpacity = 1,
    popup = ~paste0("Species: ", species_name, "<br>",
                    "ID: ", sighting_id, "<br>",
                    "Date: ", sighting_date)
  ) %>% 
  leaflet::addLegend(
    "topright",
    pal = species_pal,
    values = caitlin_sightings_sf$species_name,
    title = "Species",
    opacity = 1
  )


