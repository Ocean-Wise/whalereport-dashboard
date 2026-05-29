####~~~~~~~~~~~~~~~~~~Common Data Request Outputs and Analysis~~~~~~~~~~~~~~~~~~####
##Author: Carly Green
##Date: May 29 2026 
##Common Request Outputs, investigating, visualizations
##Must run Common Requests IN first

### run Common Requests IN (with all proper parameters filled in)
source("Common Requests IN.R")

### ANALYSIS  - outputs from sightings_filtered

### checking for duplicates (can be based on longitude or latitude)
sightings_filtered |>
  dplyr::distinct(report_longitude) |>
  nrow() 

### create a table for inspecting the duplicates if needed
duplicates = sightings_filtered |> 
dplyr::group_by(report_longitude) |>
  dplyr::filter(dplyr::n() > 1)

### Depending on the request, you can summarize species data, source, etc.
species_summary = sightings_filtered |>
  dplyr::group_by(sighting_year, species_name) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

source_summary = sightings_filtered |>
  dplyr::group_by(report_source_entity) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

############### TEMPORARY FOR NOW - comment cleaning 

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

##Column select

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

### SAVE OUTPUTS

### Update to your username 
user = "CarlyGreen"

### Save spreadsheet 
openxlsx::write.xlsx(sightings_out, paste0("C:/Users/", user, "/Downloads/", Sys.Date(),"-sightingsorg.xlsx"))

### Option to save multiple sheets and outputs 
writexl::write_xlsx(
  list(
    "Sightings" = sightings_out,
    "Species" = species_summary,
    "Sources" = source_summary
  ),
  path = paste0("C:/Users/",user,"/Downloads/",Sys.Date(),"-sightingsorg.xlsx"))


### MAPPING SIGHTINGS 

### create as spatial data
sightings_filtered_sf = sightings_filtered |>
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )


### sightings map: general
leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
  # leaflet::addPolygons(
  #   data = request_area,
  #   color = "blue",
  #   weight = 2,
  #   fill = FALSE
  # ) |>
  leaflet::addCircleMarkers(
    data = sightings_filtered_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "grey",
    fillColor = "#FFCE34",
    fillOpacity = 1)

### sightings map: by species 

species_pal = leaflet::colorFactor(
  palette = ocean_wise_palette,
  domain = sightings_filtered_sf$species_name)

leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  # leaflet::addPolygons(data = request_area,
  #                      color = "blue", 
  #                      fill = FALSE, 
  #                      weight = 2) %>% 
  leaflet::addCircleMarkers(
    data = sightings_filtered_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~species_pal(species_name),
    fillColor = ~species_pal(species_name),
    fillOpacity = 1
  ) %>% 
  leaflet::addLegend(
    "bottomleft",
    pal = species_pal,
    values = sightings_filtered_sf$species_name,
    title = "Species",
  )

### sightings map: by source

source_pal <- leaflet::colorFactor(
  palette = ocean_wise_palette,
  domain = sightings_filtered_sf$report_source_entity)

leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  # leaflet::addPolygons(data = request_area,
  #                      color = "blue", 
  #                      fill = FALSE, 
  #                      weight = 2) %>% 
  leaflet::addCircleMarkers(
    data = sightings_filtered_sf,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~source_pal(report_source_entity),
    fillColor = ~source_pal(report_source_entity),
    fillOpacity = 1
  ) %>% 
  leaflet::addLegend(
    "bottomleft",
    pal = source_pal,
    values = sightings_filtered_sf$report_source_entity,
    title = "Source",
  )

####~~~~~~~~~~~~ALERTS~~~~~~~~~~~~~####

### ANALYSIS  - outputs from alerts_filtered


### checking for duplicates QUESTION - by alert user id? do we need to do this?
alerts_filtered |>
  dplyr::distinct(alert_user_id) |>
  nrow() 

### create a table for inspecting the duplicates if needed
duplicates = sightings_filtered |> 
  dplyr::group_by(alert_user_id) |>
  dplyr::filter(dplyr::n() > 1)

### Depending on the request, you can summarize by organization email, context

### summarizing the delivery methods 
notification_types = alerts_filtered |>
  tidyr::pivot_longer(
    cols = c(email_sent, sms_sent, push_sent),
    names_to = "method",
    values_to = "sent"
  ) |>
  dplyr::filter(sent == TRUE)

notification_types = notification_types |>
  dplyr::mutate(
    method_label = dplyr::case_when(
      method == "email_sent" ~ "Email",
      method == "sms_sent" ~ "SMS",
      method == "push_sent" ~ "Push"
    )
  )
### summary table for delivery methods
notification_summary = notification_types |>
  dplyr::group_by(alert_year, method_label) |>
  dplyr::summarise(
    count = dplyr::n(),
    .groups = "drop"
  )

### context of proximity vs ZOI
notification_context = alerts_filtered |> 
  dplyr::filter(
    context %in% c("current_location", "preferred_area")
  ) |> 
  dplyr::mutate(
    context_label= dplyr::case_when(
      context == "current_location" ~ "Proximity",
      context== "preferred_area" ~ "Zone of Interest")
  ) |> 
  dplyr::group_by(year = alert_year, context_label) |> 
  dplyr::summarize(count = dplyr:: n(), .groups = "drop")

### summary of alerts sent to specific organiztion 
org_notifications = 


####~~~~~~~~~~~~~Type 1: Request for Ocean Wise (unless other entities allowed) sightings in a defined area and date range~~~~~~~~~~###
####~~~~~~~~~Type 2: Running total of Sightings and Alerts sent ~~~~~~~~~~####
####~~~~~~~~~~Type 3: Organizational specific sightings data request~~~~~~####
####Fiscal Year comparison 
####Yearly comparison calendar year 
####Organization specific sightings and Alerts 



