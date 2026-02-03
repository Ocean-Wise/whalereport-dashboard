##Ashley Bachert Data Request North Coast Humpacks##
##Author: Carly Green
##Date: Jan 26 2026

##I have ran Config R, Data-import R, Data-cleaning R
##Using the sightings_main table as it contains all sightings
##in Data-cleaning R I made sure to keep 'status' column in sightings main. This edit is at row 377 and 529.
##at row 377 I changed step 1 to the following 
## Step 1: For reports WITH sighting_id, get the earliest report per sighting
# sightings_with_id = report_raw %>%
#   dplyr::filter(!is.na(sighting_id)) %>%
#   dplyr::group_by(sighting_id) %>%
#   dplyr::arrange(sighting_date) %>%
#   dplyr::slice(1) %>%
#   dplyr::ungroup() %>% 
#   dplyr::select(
#     sighting_id,
#     sighting_date,
#     status,
#     dplyr::everything()
#   )
##at row 529  dplyr::select I included 'status'

##Request details 
##Jan 1 2025 - Dec 31 2025 but she ideally just wants processed data.
##Coordinates cover box from beyond Gringolox to Bella Bella with Tip of Haida Gwaii as the Western boundary

#step 1 create start and end dates 
start_date = lubridate::as_date("2015-01-01")
end_date = lubridate::as_date("2025-12-31")

#step 2 create box coordinates for area of request 
box_coords <- matrix(
  c(
    -133.5796102, 51.7991842,  # Lower Left
    -133.5796102, 55.1299119,  # Upper Left
    -127.9208957, 55.1299119,  # Upper Right
    -127.9208957, 51.7991842,  # Lower Right
    -133.5796102, 51.7991842   # Close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_polynew = sf::st_polygon(list(box_coords)) |>
  sf::st_sfc(crs = 4326)


#step 3 create the filtered table from sightings main. Forcing time zones in historical imports to adjust. 
hb_sightings = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) %>% 
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::mutate(sighting_date = lubridate::floor_date(sighting_date, unit = "minute")) %>% 
  dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
  dplyr::distinct(sighting_date, .keep_all = TRUE) %>% 
  dplyr::filter(species_name == "Humpback whale") %>% 
  dplyr::filter(!observer_type_name== "external-org") %>%
  dplyr::filter(!report_source_entity== "Whale Alert Alaska") %>%
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(box_polynew) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::rename(confidence = observer_confidence) %>% ##trim and keep usable comments 
  dplyr::mutate(
    comments = dplyr::case_when(
      stringr::str_detect(comments, "Historical Import") &
        stringr::str_detect(comments, "Comments:") ~
        stringr::str_trim(
          stringr::str_extract(
            comments, '(?i)(?<=comments:)\\s*.*')),
      !is.na(comments) &
        stringr::str_detect(comments, "Historical") ~ NA_character_,
      TRUE ~ comments
    ),
    comments = stringr::str_trim(comments),
    comments = dplyr::na_if(comments, ""),
    comments = dplyr::na_if(comments, "-")) %>% #historical comments with just a dash are removed to NA
  dplyr::select(-c("report_modality",
                   "ecotype_name",
                   "sighting_year_month"))

#step 4 view the status of what is not approved or auto approved
View(
  hb_sightings %>% 
    dplyr::filter(!status %in% c("approved", "auto_approved"))
)
##save this portion of the table for ashley
hb_not_processed = hb_sightings %>% 
  dplyr::filter(!status %in% c("approved", "auto_approved"))

  
#checking for unique rows and dates 
  unique(hb_sightings$sighting_date) 

hb_sightings %>%
  dplyr::distinct(sighting_date) %>%
  nrow()
  

##step 5 try to map it

##put back geometry
hb_sightings_sf <- hb_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

##map it

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
    data = hb_sightings_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "#FFCE34",
    fillColor = "#FFCE34",
    fillOpacity = 1)

#step 6 save it

install.packages("writexl")
writexl::write_xlsx(
  list(
    "Humpback Sightings 2025" = hb_sightings, ##all sightings 
    "Not Processed Sightings" = hb_not_processed
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/hb_sightings_not_processed.xlsx")