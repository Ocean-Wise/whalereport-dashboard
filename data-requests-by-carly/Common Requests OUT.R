####~~~~~~~~~~~~~~~~~~Common Data Request Outputs and Analysis~~~~~~~~~~~~~~~~~~####
##Author: Carly Green
##Date: May 29 2026 
##Common Request Outputs, investigating, visualizations
##Must run Common Requests IN first

### run Common Requests IN (with all proper parameters filled in or as is if no filters)
# source("Common Requests IN.R")

####~~~~~~~~~~~~~~~~~~Sightings Outputs and Analysis~~~~~~~~~~~~~~~~~~####

### checking for duplicates (can be based on longitude or latitude)
sightings_filtered |>
  dplyr::distinct(report_longitude) |>
  nrow() 

### create a table for inspecting the duplicates if needed
##QUESTION - how are there still rows marked 'as duplicate' TRUE if we removed them in Common Requests IN.R???
duplicates = sightings_filtered |> 
dplyr::group_by(report_longitude, report_latitude) |>
  dplyr::filter(dplyr::n() > 1)

### Depending on the request, you can summarize species data, source, etc.
species_summary = sightings_filtered |>
  dplyr::group_by(sighting_year, species_name) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

source_summary = sightings_filtered |>
  dplyr::group_by(report_source_entity) |>
  dplyr::summarise(count = dplyr::n(), .groups = "drop")

############### TEMPORARY FOR NOW - comment cleaning 
## QUESTION - orca network commments always specify a name, but the likelyhood of us sharing orca network data is low anyways so it's ok right?

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

### Column select

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

####~~~~~~~~~~~~~~~~~Organization Specific Sightings~~~~~~~~~~~~~~~~~~~####

### Sightings from a specific organization (string detect org name, and/or use the email ending i.e. wsdot)
## note common regex statements provided in sandbox. 
# org_sightings = sightings_filtered |>
#   dplyr::filter(
#     sighting_date >= start_date,
#     sighting_date <= end_date,
#     stringr::str_detect(observer_organization,
#                         stringr::regex("washington state ferries |wsf", ignore_case = TRUE)
#   ) | stringr::str_detect(observer_email, "wsdot") #including any ws ferries organization name OR emails
# )

### need to column clean org_sightings
# 
# org_sightings = org_sightings |>
#   dplyr::select(
#     sighting_id,
#     report_id,
#     sighting_date,
#     report_status,
#     sighting_code,
#     species_name,
#     species_scientific_name,
#     ecotype_name,
#     report_latitude,
#     report_longitude,
#     report_count,
#     count_type,
#     direction,
#     confidence,
#     behaviour,
#     comments,
#     sighting_platform_name,
#     report_source_entity,
#     report_source_type,
#     observer_name,
#     observer_email,
#     observer_organization,
#     sighting_year,
#     sighting_month,
#   )

### clean up organization name for consistency - change to name kept in Salesforce (i.e. Washington State Ferries)
# org_sightings = org_sightings |> 
#   dplyr::mutate(
#     observer_organization = "ORGANIZATION NAME HERE")

# ### save organization specific spreadsheet. 
# openxlsx::write.xlsx(org_sightings, paste0("C:/Users/", user, "/Downloads/", Sys.Date(),"-organization_sightings.xlsx"))


###~~~~~~~~~~~~~~~~~~Mapping Sightings~~~~~~~~~~~~~~~~~~~~~###

### create as spatial data
sightings_filtered_sf = sightings_filtered |>
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"),
    crs = 4326,
    remove = FALSE
  )


### sightings map: general

## QUESTION/note we need to remove/reject the test sightings because when I map alllll sightings I am seeing whales in Africa, Greenland etc.
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

source_pal = leaflet::colorFactor(
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

###~~~~~~~~~~~~~~~~~~Alert Outputs and Analysis~~~~~~~~~~~~~~~~~~~~~###

### checking for duplicates 
##QUESTION - dplyr::distinct by alert user id? or what else would we use? also this may not be a necessary step 
alerts_filtered |>
  dplyr::distinct(alert_user_id) |>
  nrow() 

### create a table for inspecting the duplicates if needed
duplicates_alerts = alerts_filtered |> 
  dplyr::group_by(alert_user_id) |>
  dplyr::filter(dplyr::n() > 1)

### Depending on the request, you can summarize by delivery method, context, organization
##COMMENT - I Feel like there may be a better way to do this then how I did this.. 

### Summary of unique alerts by year 
unique_alert_summary = alerts_filtered |> 
  dplyr::group_by(alert_year) |> 
  dplyr::summarize(
    unique_alerts = dplyr::n_distinct(alert_id),
    .groups = "drop"
  )

### How many people are receiving alerts (notifications) no matter what their delivery method is
recipient_summary = alerts_filtered |>
  dplyr::group_by(alert_year) |>
  dplyr::summarise(
    unique_recipients = dplyr::n_distinct(alert_user_id),
    .groups = "drop"
  )

### how many notifications being sent (one person can receive up to three: sms, push, email)
notifications_sent = alerts_filtered |> 
  dplyr::group_by(alert_year) |>
  dplyr::summarise(
    total_email_sends = sum(email_sent == TRUE, na.rm = TRUE),
    total_sms_sends = sum(sms_sent == TRUE, na.rm = TRUE),
    total_push_sends = sum(push_sent == TRUE, na.rm = TRUE),
    total_notification_sends =
      total_email_sends +
      total_sms_sends +
      total_push_sends,
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

### summary of alerts sent to specific organization --- NEED SUPPORT ON THIS PIECE --------
##COMMENT/QUESTION this was done with main_dataset over alerts_main because we are pulling user_email_recipient. I think this means we can't use alerts_filtered?
##COMMENT alerts_filtered does have target_address_email but only if that person is receiving email notifs. SO I still think main_dataset is best yes?  
org_alerts = main_dataset |>
  dplyr::filter(
    stringr::str_detect(
      user_organization_recipient,
      stringr::regex("washington state ferries|wsf", ignore_case = TRUE) ##example regex
    ) |
      stringr::str_detect(observer_email, "wsdot"),
    alert_year == 2025
  ) |>
  dplyr::group_by(user_email_recipient) |>
  dplyr::summarise(
    email_only = sum(email_sent & !sms_sent & !push_sent, na.rm = TRUE),
    sms_only   = sum(sms_sent & !email_sent & !push_sent, na.rm = TRUE),
    push_only  = sum(push_sent & !email_sent & !sms_sent, na.rm = TRUE),
    
    email_sms  = sum(email_sent & sms_sent & !push_sent, na.rm = TRUE),
    email_push = sum(email_sent & push_sent & !sms_sent, na.rm = TRUE),
    sms_push   = sum(sms_sent & push_sent & !email_sent, na.rm = TRUE),
    
    all_three  = sum(email_sent & sms_sent & push_sent, na.rm = TRUE),
    total_alerts = dplyr::n(),
    .groups = "drop"
  ) |>
  dplyr::arrange(desc(total_alerts))

##########Another Option for this^  would be to create a function?################


##still need to create
####Fiscal Year comparison 
####Yearly comparison with calendar year 
####Organization specific alert -- need support 

###~~~~~~~~~~~~~~~~~~Alert Mapping~~~~~~~~~~~~~~~~~~~~~###
###general, heat, source? any other type of alert map?species? 

## General Alert Map
leaflet::leaflet() |> 
  leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |> 
  # leaflet::addPolygons(
  #   data = request_area,
  #   color = "#A8007E",
  #   weight = 2,
  #   fill = FALSE
  # ) |> 
  leaflet::addCircleMarkers(
    data = alerts_filtered,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = "grey",
    fillColor = "#FFCE34",
    fillOpacity = 1)

## Alert Heat Map 
leaflet::leaflet() |>
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |> 
  leaflet.extras::addHeatmap(
    data = alerts_filtered,
    radius = 15,
    blur = 20,
    max = 0.05
  )

## Alerts from sources
#### QUESTIONS -Do we ever need this one? Considering we show sightings by source? Also should this be based on triggering location now?? 
source_pal1 = leaflet::colorFactor(
  palette = ocean_wise_palette,
  domain = alerts_filtered$report_source_entity)

leaflet::leaflet() %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>% 
  # leaflet::addPolygons(data = request_area,
  #                      color = "blue", 
  #                      fill = FALSE, 
  #                      weight = 2) %>% 
  leaflet::addCircleMarkers(
    data = alerts_filtered,
    radius = 4, 
    stroke = TRUE,
    weight = 1,
    color = ~source_pal1(report_source_entity),
    fillColor = ~source_pal1(report_source_entity),
    fillOpacity = 1
  ) %>% 
  leaflet::addLegend(
    "bottomleft",
    pal = source_pal,
    values = alerts_filtered$report_source_entity,
    title = "Source",
  )

####~~~~~~~~~~~~~~~~~~~~~~~~~~SANDBOX~~~~~~~~~~~~~~~~~~~~~~~~~###

## Washington State Ferries regex statement 
stringr::str_detect(observer_organization,
                    stringr::regex("washington state ferries |wsf", ignore_case = TRUE)
                    ) | stringr::str_detect(observer_email, "wsdot") #including any ws ferries organization name OR emails

## BC Ferries regex statement
stringr::str_detect(observer_organization, 
                    stringr::regex("bc\\s*fer", ignore_case = TRUE)
                    ) | stringr::str_detect(observer_email, "bcferries") #including any bc ferries organizations OR bcferries emails


## List of specific emails to get sightings from (if provided) such as for Skana people
  dplyr::filter(observer_email %in% c(
    "gary.sutton@ocean.org",
    "michael.judson@ocean.org",
    "ashleybachert@hotmail.com",
    "chloe.robinson@ocean.org",
    "hannah.trotman@ocean.org",
    "hcrichards00@gmail.com",
    "olivia.heintzman@ocean.org"
  ))