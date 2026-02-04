##NOAA Humpback Request for Hannah Miller##
##Author: Carly Green
##Date: Feb 3 2026

##need to provide 

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts
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
##at row 529  dplyr::select I included status. 

#step 1 set dates 
#all WRAS years, 2019 to date. Will do 2019 until Jan 31 2026
start_date = lubridate::as_date("2019-01-01")
end_date = lubridate::as_date("2026-01-31")

#step 2 create box
box_coords <- matrix(
  c(
    -126, 47,  # Lower Left
    -126, 49,  # Upper Left
    -122, 49,  # Upper Right
    -122, 47, # Lower Right
    -126, 47   #close polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_polynew = sf::st_polygon(list(box_coords)) |>
  sf::st_sfc(crs = 4326)

plot(box_polynew)

#step 3 filter for humpback sightings within the box. Standardize dates first, then filter. 
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
  dplyr::rename(confidence = observer_confidence) %>%
  dplyr::select(-c(dplyr::contains("observer"),
                   "report_modality",
                   "total_reports",
                   "ecotype_name",
                   "sighting_year_month")) %>% 
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
    comments = stringr::str_remove(comments, stringr::fixed("[Orca Network]")),##trying to figure out how to remove [Orca Network]
    comments = stringr::str_trim(comments),
    comments = dplyr::na_if(comments, ""),
    comments = dplyr::na_if(comments, "-"))


#step 4 view status of what is not approved or approved
View(
  hb_sightings %>% 
    dplyr::filter(!status %in% c("approved", "auto_approved"))
) ##there are 525 not approved 

##save this portion of the table
hb_not_processed = hb_sightings %>% 
  dplyr::filter(!status %in% c("approved", "auto_approved"))
  

  # dplyr::mutate(report_source_entity = "Ocean Wise Conservation Association") 
  # dplyr::mutate(
  #   comments = dplyr::case_when(
  #     stringr::str_detect(comments, "Alaska") ~ "remove",
  #     stringr::str_detect(comments, "Historical") ~ NA_character_,
  #     TRUE ~ comments
  #   )
  # ) %>% 
  # dplyr::filter(comments!= "remove"| is.na(comments))
  
  ##trim and keep usable comments 
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


#step 3 filter for humpback sightings in the box. Ensure to standardize time zones in sighting_date in sightings_main table
 
  
  
  unique(hb_sightings$sighting_date) #are there duplicates

hb_sightings %>%
  dplyr::distinct(sighting_date) %>%
  nrow() 
  
  ###try to map it
  ##put back geometry
  hb_sightings_sf <- hb_sightings %>%
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )

##map it ##hanna doesn't need a map I just wanted to see if I was correct in my polygon

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




  