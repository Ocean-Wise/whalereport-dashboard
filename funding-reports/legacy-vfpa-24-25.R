####~~~~~~~~~~~~~~~~~~~~~~VFPA Reporting~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the production of metrics for VFPA reporting.
## Date written: 2024-07-05
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project
##      3. Run the "monthly-dashboard-numbers.R" script first

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

## source the data for the visuals. 
source("./monthly-dashboard-numbers.R")

## this is a test commit

## color pallette
ocean_wise_palette <- c(
  "Sun"      = "#FFCE34",
  "Kelp"     = "#A2B427",
  "Coral"    = "#A8007E",
  "Anemone"  = "#354EB1",
  "Ocean"    = "#005A7C",
  "Tide"     = "#5FCBDA",
  "Black"    = "#000000",
  "White"    = "#FFFFFF",
  "Dolphin"  = "#B1B1B1"
)

##
get_ocean_wise_colors <- function(n) {
  if (n > length(ocean_wise_palette)) {
    warning("Not enough Ocean Wise colors — some colors will be reused.")
  }
  rep(ocean_wise_palette, length.out = n)
}

## Set pallete 
source_colors = setNames(
  get_ocean_wise_colors(length(unique(detections_pre$source_entity))), 
  unique(detections$source_entity)
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####~~~ datasets for visualizations ~~~####
## How many sightings in reporting period Feb -...?

## Creating a simple clean detections data set for grant period
period_detections = detections_pre %>% 
  dplyr::filter(zoo::as.yearmon(sighted_at) < zoo::as.yearmon(Sys.Date())) %>%
  # dplyr::filter(dplyr::between(sighted_at, 
  #                              lubridate::as_date("2024-01-01"),
  #                              lubridate::rollback(lubridate::floor_date(Sys.Date(), unit = "month")))) %>% 
  # dplyr::filter(lubridate::year(sighted_at) == 2025) %>% 
  dplyr::select(id, sighted_at, species, source_entity, latitude, longitude)
  


## and the same for alerts
period_alerts = alerts_detections %>% 
  dplyr::filter(zoo::as.yearmon(sighted_at) < zoo::as.yearmon(Sys.Date())) %>% 
  # dplyr::filter(dplyr::between(sent_at, 
  #                              lubridate::as_date("2024-01-01"),
  #                              lubridate::rollback(lubridate::floor_date(Sys.Date(), unit = "month")))) %>% 
  dplyr::select(c(id, sighting_id, sent_at, sighted_at, species, source_entity, latitude, longitude))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
####~~~ Plots ~~~####

##~~ Data ~~####
# 
# jasco_events = period_detections %>%
#   dplyr::filter(source_entity == "JASCO") %>%
#   dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
#   dplyr::arrange(sighted_at) %>%
#   dplyr::mutate(
#     time_diff = as.numeric(sighted_at - dplyr::lag(sighted_at), units = "mins"),
#     new_event = dplyr::if_else(is.na(time_diff) | time_diff >= 30, 1, 0),
#     event_id = cumsum(new_event)
#   )

detections = period_detections %>% 
  # dplyr::filter(source_entity %in% c("JASCO", "SMRU")) %>% 
  dplyr::group_by(source_entity, 
                  year_month = zoo::as.yearmon(sighted_at)
                  ) %>% 
  dplyr::summarize(detections = dplyr::n_distinct(id)) %>% 
  rbind(data.frame(
    source_entity = rep("JASCO", 2),
    year_month = zoo::as.yearmon(c("Feb 2025", "Mar 2025"), "%b %Y"),
    detections = c(0, 0)
  )) 
  

  
alerts  = period_alerts %>% 
  # dplyr::filter(source_entity %in% c("JASCO", "SMRU")) %>% 
  dplyr::group_by(source_entity, 
                  year_month = zoo::as.yearmon(sent_at)
                  ) %>%
  dplyr::summarize(alerts = dplyr::n()) %>% 
  rbind(data.frame(
    source_entity = rep("JASCO", 2),
    year_month = zoo::as.yearmon(c("Feb 2025", "Mar 2025"), "%b %Y"),
    alerts = c(0, 0)
  ))


##~~ Line graph of alerts and detections ~~####

# Define a function to make graphs for data sources
make_lines_function = function(source){
  
  # make data
  plot_data = dplyr::left_join(detections, alerts) %>% 
    dplyr::mutate(month = factor(base::month.abb[as.integer(lubridate::month(year_month))], levels = base::month.abb, ordered = TRUE),
                  year = lubridate::year(year_month)) %>% 
    dplyr::select(-year_month) %>% 
    tidyr::pivot_wider(names_from = year, values_from = c(detections, alerts)) %>% 
    # dplyr::mutate(dplyr::across(dplyr::everything(), ~tidyr::replace_na(.x, 0))) %>% 
    dplyr::filter(source_entity == source) %>% 
    # dplyr::select(c(1,2,dplyr::ends_with("24"),dplyr::ends_with("25"))) %>% 
    dplyr::mutate(month = factor(month,
                                 levels = month.abb,  # c("Jan", "Feb", ..., "Dec")
                                 ordered = TRUE)) %>% 
    dplyr::arrange(month) %>% 
    dplyr::mutate(
      month = factor(
        month,
        levels = lubridate::month(1:12, label = TRUE, abbr = TRUE),
        ordered = TRUE
      ),
      current_month = factor(
        lubridate::month(Sys.Date(), label = TRUE, abbr = TRUE),
        levels = lubridate::month(1:12, label = TRUE, abbr = TRUE),
        ordered = TRUE
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("detections_") | dplyr::starts_with("alerts_"),
      .fns = ~ dplyr::if_else(month < current_month & is.na(.x), 0, .x)
    ))
  
  # make plot
  plotly::plot_ly(data = plot_data,
                  y = ~detections_2025,
                  x = ~month,
                  name = "Detections 2025",
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "#800080",
                              dash = "solid")) %>% 
    plotly::add_trace(y = ~detections_2024,
                      name = "Detections 2024",
                      # mode = "lines",
                      line = list(color = "#D8BFD8",
                                  dash  = "solid")) %>% 
    plotly::add_trace(y = ~alerts_2025,
                      name = "Alerts 2025",
                      # mode = "lines",
                      line = list(color = "#800080",
                                  dash  = "dash")) %>% 
    plotly::add_trace(y = ~alerts_2024,
                      name = "Alerts 2024",
                      # mode = "lines",
                      line = list(color = "#D8BFD8",
                                  dash  = "dash")) %>% 
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = ""),
      legend = list(title = list(text = "<b>Source</b>"))
      )
  
}


# JASCO
make_lines_function("JASCO")

# SMRU
make_lines_function("SMRU") %>% 
  plotly::layout(
    shapes = list(
      list(type = "line",
           y0 = 0,
           y1 = 1,
           yref = "paper",
           x0 = "Jun",
           x1 = "Jun",
           line = list(color = "B1B1B1", dash="dot"))
    )
  ) %>% 
  plotly::add_text(showlegend = FALSE, 
                   y = 105,
                   x = factor("May", levels = month.abb, ordered = TRUE),
                   text = "SMRU Integrated \n Jun 24")


# Whale Spotter

make_lines_function("WhaleSpotter") %>% 
  plotly::layout(
    shapes = list(
      list(type = "line",
           y0 = 0,
           y1 = 1,
           yref = "paper",
           x0 = "Apr",
           x1 = "Apr",
           line = list(color = "B1B1B1", dash="dot"))
    )
  ) %>% 
  plotly::add_text(showlegend = FALSE, 
                   y = 80,
                   x = factor("Mar", levels = month.abb, ordered = TRUE),
                   text = "WhaleSpotter Integrated \n Apr 24")



##~~ Day vs Night ~~####

day_vs_night_function = function(year, source){

    day_night_detections = period_detections %>% 
      dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
      dplyr::filter(source_entity %in% c("Ocean Wise", "Orca Network", "Whale Alert")) %>% 
      dplyr::mutate(date = lubridate::as_date(sighted_at)) %>% 
      dplyr::distinct(sighted_at, date, latitude, longitude) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        sun_info = list(suncalc::getSunlightTimes(
          date = date,
          lat = latitude,
          lon = longitude,
          keep = c("dawn", "dusk"),
          tz = "America/Los_Angeles"
        ))
      ) %>%
      tidyr::unnest(cols = c(sun_info), names_sep = "_") %>%
      dplyr::select(sighted_at, latitude, longitude, dawn = sun_info_dawn, dusk = sun_info_dusk)
    
    full_months = tidyr::expand_grid(
      month = seq.Date(from = as.Date(paste0(2024,"-01-01")), to = as.Date(paste0(2025,"-12-01")), by = "month"),
      time_of_day = c("day", "night")
    )
    
    
    x = period_detections %>% 
      dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
      dplyr::filter(source_entity %in% c("Ocean Wise", "Orca Network", "Whale Alert")) %>% 
      dplyr::left_join(day_night_detections, by = c("sighted_at", "latitude", "longitude")) %>% 
      dplyr::mutate(sighted_at = lubridate::force_tz(sighted_at, tzone = "America/Los_Angeles")) %>%
      dplyr::mutate(
        time_of_day = dplyr::case_when(
          sighted_at >= dawn & sighted_at <= dusk ~ "day",
          TRUE ~ "night"
        )
      ) %>% 
      dplyr::mutate(
        month = lubridate::floor_date(sighted_at, unit = "month")
      ) %>%
      dplyr::group_by(month, time_of_day) %>%
      dplyr::summarise(count = dplyr::n(), .groups = "drop") %>% 
      dplyr::full_join(full_months, bar_data, by = c("month", "time_of_day")) %>%
      dplyr::mutate(count = tidyr::replace_na(count, 0),
                    month_label = factor(format(month, "%b"),levels = month.abb)) %>% 
      plotly::plot_ly(
        x = ~month_label,
        y = ~count,
        color = ~time_of_day,
        colors = c("day" = "#FDB813", "night" = "#2C3E50"),
        type = "bar") %>% 
      plotly::layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = "Detections"),
        legend = list(title = list(text = "<b>Time of Day</b>"))
      )
}

day_vs_night_function(2025, "Ocean Wise")
day_vs_night_function(2025, "Orca Network")
day_vs_night_function(2025, "WhaleSpotter")
day_vs_night_function(2025, "JASCO")
day_vs_night_function(2025, "SMRU")

day_vs_night_function(2025, c("Ocean Wise", "Orca Network", "Whale Alert"))
day_vs_night_function(2025, c("WhaleSpotter", "JASCO", "SMRU"))





##~~ Maps and spatial work ~~####
# detection map

alert_map = period_detections %>%
  ## EDIT THIS FOR REPORTING PERIOD
  dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
  dplyr::mutate(col_palette =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A8007E",
                    stringr::str_detect(source_entity, "Orca Network|Alert") == T ~ "#FFCE34",
                    # stringr::str_detect(source_entity, "Alaska") == T ~ "#27AE60",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#A2B427",
                    stringr::str_detect(source_entity, "JASCO|SMRU") == T ~ "#005A7C"
                  )) %>%
  dplyr::mutate(detection_method =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "Infrared camera",
                    stringr::str_detect(source_entity, "Orca Network|Alert") == T ~ "Partner sightings network",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Whale report app",
                    stringr::str_detect(source_entity, "JASCO|SMRU") == T ~ "Hydrophone"
                  )) %>%
  dplyr::mutate(
    popup_content =
      paste("<b>Species:</b> ", species,
            "<b><br>Source:</b> ", source_entity,
            "<b><br>Detection method:</b>", detection_method,
            "<b><br>Date:</b>", as.Date(sighted_at)
      ))


## Point Map
alert_map %>%
  leaflet::leaflet() %>%
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet::addTiles(
    urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
    attribution = 'Map data: &copy; <a href="https://www.openseamap.org">OpenSeaMap</a> contributors',
    group = "OpenSeaMap"
  ) %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    color = ~col_palette,
    fillOpacity = 0.6,
    opacity = 0.6,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(alert_map$col_palette)),
    labels = c(unique(alert_map$detection_method)),
    opacity = 0.8
  ) %>% 
  leaflet::addMiniMap(toggleDisplay = TRUE) %>% 
  htmlwidgets::saveWidget(., paste0("C:/Users/",
                                         user,
                                         "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
                                         "interactive-sightings-map-",
                                         Sys.Date(),
                                         ".html"),
                          selfcontained = TRUE)
  

## stacked bar graph of detections
detections %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(lubridate::year(year_month) == 2025) %>% 
  tidyr::complete(
    source_entity,
    year_month = zoo::as.yearmon(paste(month.abb, 2025))
  ) %>%
  dplyr::mutate(detections = tidyr::replace_na(detections, 0)) %>% 
  dplyr::mutate(month = format(year_month, "%b")) %>%
  dplyr::mutate(month = factor(month, levels = month.abb)) %>%
  plotly::plot_ly(
    data = .,
    x = ~month,
    y = ~detections,
    color = ~source_entity,
    colors = source_colors,
    type = "bar"
  ) %>%
  plotly::layout(
    barmode = "stack",
    xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
    yaxis = list(title = "Detections"),
    legend = list(orientation = "h", x = 0.1, y = -0.2)
  )

## total quarterly detections
detections %>% 
  dplyr::filter(lubridate::year(year_month) == 2025) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year_month) %>%
  dplyr::summarise(total_detections = sum(detections, na.rm = TRUE))


## stacked bar graph of alerts
alerts %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(lubridate::year(year_month) == 2025) %>% 
  tidyr::complete(
    source_entity,
    year_month = zoo::as.yearmon(paste(month.abb, 2025))
  ) %>%
  dplyr::mutate(alerts = tidyr::replace_na(alerts, 0)) %>% 
  dplyr::mutate(month = format(year_month, "%b")) %>%
  dplyr::mutate(month = factor(month, levels = month.abb)) %>%
  plotly::plot_ly(
    data = .,
    x = ~month,
    y = ~alerts,
    color = ~source_entity,
    colors = source_colors,
    type = "bar"
  ) %>%
  plotly::layout(
    barmode = "stack",
    xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
    yaxis = list(title = "Alerts"),
    legend = list(orientation = "h", x = 0.1, y = -0.2)
  )

## total quarterly alerts
alerts %>% 
  dplyr::filter(lubridate::year(year_month) == 2025) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(year_month) %>%
  dplyr::summarise(total_detections = sum(alerts, na.rm = TRUE))


##~~ WhaleSpotter table ~~####

whalespotter_species = detections_pre %>%
  dplyr::filter(source_entity == "WhaleSpotter") %>% 
  dplyr::group_by(year_mon = zoo::as.yearmon(sighted_at), source_entity, species) %>%
  dplyr::summarise(n = dplyr::n())

whalespotter_sightings_alerts = detections_pre %>%
  dplyr::filter(source_entity == "WhaleSpotter") %>% 
  dplyr::group_by(year_mon = zoo::as.yearmon(sighted_at), source_entity) %>%
  dplyr::filter(id %in% alert_clean$sighting_id) %>% 
  dplyr::summarise(n = dplyr::n())

rm(list = "whalespotter_sightings_alerts", "whalespotter_species")

##~~ Regional data ~~####
#load shapefile
shapefiles = sf::st_read(
  paste0("C:/Users/", user, "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/Shapefiles/ECHO slowdown - for data filter/echo-slowdown.shp")
) %>% 
  dplyr::mutate(
    region = dplyr::case_when(
      grepl("swiftsure", Name, ignore.case = TRUE) ~ "Swiftsure Bank",
      TRUE ~ Name  # Keep original name for others
    )
  ) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  sf::st_as_sf()

# cleaned dataset of sightings, source, and filtered by region
area_detections = period_detections %>% 
  # dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  # Assuming WGS84
  sf::st_join(shapefiles, left = TRUE) %>% 
  dplyr::filter(is.na(region) == F) 

area_alerts = period_alerts %>% 
  # dplyr::filter(lubridate::year(sent_at) == 2025) %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%  # Assuming WGS84
  sf::st_join(shapefiles, left = TRUE) %>% 
  dplyr::filter(is.na(region) == F) 


##~~ Areas ~~#### 

map_maker_function = function(area){
  
  area_data = area_detections %>% 
    dplyr::filter(region == area & lubridate::year(sighted_at) == 2025) %>% 
    dplyr::mutate(species = 
                    dplyr::case_when(
                      stringr::str_detect(species, "dolphin") ~ "Dolphin/Porpoise species",
                      stringr::str_detect(species, "porpoise") ~ "Dolphin/Porpoise species",
                      stringr::str_detect(species, "turtle") ~ "Potential Turtle species",
                      stringr::str_detect(species, "False") ~ "Dolphin/Porpoise species",
                      stringr::str_detect(species, "Sei") ~ "Unidentified whale",
                      .default = as.character(species)
                    )) %>% 
    dplyr::mutate(col_palette =
                    dplyr::case_when(
                      species == "Minke whale" ~ "#A569BD",
                      species == "Killer whale" ~ "#17202A",
                      species == "Humpback whale" ~ "#E74C3C",
                      species == "Fin whale" ~ "#F4D03F",
                      species == "Dolphin/Porpoise species" ~ "#566573",
                      species == "Grey whale" ~ "#AAB7B8",
                      species == "Dolphin species" ~ "#1ABC9C",
                      species == "Sperm whale" ~ "blue",
                      species == "Unidentified whale" ~ "#B7950B",
                      species == "Potential Turtle species" ~ "darkgreen" 
                    ))
  
   
  leaflet::leaflet(data = area_data) %>%
        leaflet::addProviderTiles("CartoDB.Positron") %>%
        leaflet::addTiles(
          urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
          attribution = 'Map data: &copy; <a href="https://www.openseamap.org">OpenSeaMap</a> contributors',
          group = "OpenSeaMap"
        ) %>%
    leaflet::addCircleMarkers(
      group = ~species,
      color = ~col_palette,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.8
    ) %>%
    leaflet::addLegend(
      "bottomright",
      colors = c(unique(area_data$col_palette)),
      labels = c(unique(area_data$species)),
      opacity = 0.8
    )  %>%
    leaflet::setView(lng = -123.5, lat = 48.5, zoom = 8)
}


## Monthly detections and alerts

make_regional_lines_function = function(area){
  
  detections = area_detections %>% 
    dplyr::filter(region == area) %>% 
    sf::st_drop_geometry() %>% 
    # dplyr::filter(source_entity %in% c("JASCO", "SMRU")) %>% 
    dplyr::group_by(year_month = zoo::as.yearmon(sighted_at)) %>% 
    dplyr::summarize(detections = dplyr::n_distinct(id))
  
  
  
  alerts  = area_alerts %>% 
    dplyr::filter(region == area) %>% 
    sf::st_drop_geometry() %>% 
    # dplyr::filter(source_entity %in% c("JASCO", "SMRU")) %>% 
    dplyr::group_by(year_month = zoo::as.yearmon(sent_at)) %>% 
    dplyr::summarize(alerts = dplyr::n())
  
  # make data
  plot_data = dplyr::left_join(detections, alerts) %>% 
    dplyr::mutate(month = factor(base::month.abb[as.integer(lubridate::month(year_month))], levels = base::month.abb, ordered = TRUE),
                  year = lubridate::year(year_month)) %>% 
    dplyr::select(-year_month) %>% 
    tidyr::pivot_wider(names_from = year, values_from = c(detections, alerts)) %>% 
    dplyr::arrange(month) %>% 
    dplyr::mutate(
      month = factor(
        month,
        levels = lubridate::month(1:12, label = TRUE, abbr = TRUE),
        ordered = TRUE
      ),
      current_month = factor(
        lubridate::month(Sys.Date(), label = TRUE, abbr = TRUE),
        levels = lubridate::month(1:12, label = TRUE, abbr = TRUE),
        ordered = TRUE
      )
    ) %>%
    dplyr::mutate(dplyr::across(
      .cols = dplyr::starts_with("detections_") | dplyr::starts_with("alerts_"),
      .fns = ~ dplyr::if_else(month < current_month & is.na(.x), 0, .x)
    ))
  
  # make plot
  plotly::plot_ly(data = plot_data,
                  y = ~detections_2025,
                  x = ~month,
                  name = "Detections 2025",
                  type = "scatter",
                  mode = "lines",
                  line = list(color = "#800080",
                              dash = "solid")) %>% 
    plotly::add_trace(y = ~detections_2024,
                      name = "Detections 2024",
                      # mode = "lines",
                      line = list(color = "#D8BFD8",
                                  dash  = "solid")) %>% 
    plotly::add_trace(y = ~alerts_2025,
                      name = "Alerts 2025",
                      # mode = "lines",
                      line = list(color = "#800080",
                                  dash  = "dash")) %>% 
    plotly::add_trace(y = ~alerts_2024,
                      name = "Alerts 2024",
                      # mode = "lines",
                      line = list(color = "#D8BFD8",
                                  dash  = "dash")) %>% 
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = ""),
      legend = list(title = list(text = "<b>Source</b>"))
    )
  
}


## stacked bar graph of detections

area_bar_maker_function = function(area){
  
  area_detections %>% 
    sf::st_drop_geometry() %>%
    dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
    dplyr::mutate(year_month = zoo::as.yearmon(sighted_at)) %>% 
    dplyr::filter(region == area) %>% 
    dplyr::group_by(year_month, source_entity) %>% 
    dplyr::summarise(detections = dplyr::n()) %>% 
    dplyr::ungroup() %>% 
    tidyr::complete(
      source_entity,
      year_month = zoo::as.yearmon(paste("2025", month.abb), format = "%Y %b")) %>% 
    dplyr::mutate(detections = tidyr::replace_na(detections, 0)) %>% 
    dplyr::mutate(month = format(year_month, "%b")) %>%
    dplyr::mutate(month = factor(month, levels = month.abb)) %>%
    plotly::plot_ly(
      data = .,
      x = ~month,
      y = ~detections,
      color = ~source_entity,
      type = "bar"
    ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
      yaxis = list(title = "Detections")
      # legend = list(orientation = "h", x = 0.1, y = -0.2)
    )
}



# Day vs Nigh detections

day_vs_night_region_function = function(area){
  
  day_night_detections = area_detections %>% 
    sf::st_drop_geometry() %>%
    dplyr::filter(lubridate::year(sighted_at) == 2025) %>%
    dplyr::filter(region == area) %>% 
    dplyr::mutate(date = lubridate::as_date(sighted_at)) %>% 
    dplyr::distinct(sighted_at, date, id) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sun_info = list(suncalc::getSunlightTimes(
        date = date,
        lat = 48.53876,
        lon = -123.25061,
        keep = c("dawn", "dusk"),
        tz = "America/Los_Angeles"
      ))
    ) %>%
    tidyr::unnest(cols = c(sun_info), names_sep = "_") %>%
    dplyr::select(id, sighted_at, dawn = sun_info_dawn, dusk = sun_info_dusk)
  
  full_months = tidyr::expand_grid(
    month = seq.Date(from = as.Date(paste0(2025,"-01-01")), to = as.Date(paste0(2025,"-12-01")), by = "month"),
    time_of_day = c("day", "night")
  )
  
  
  area_detections %>% 
    sf::st_drop_geometry() %>% 
    dplyr::filter(lubridate::year(sighted_at) == 2025) %>% 
    dplyr::filter(region == area) %>% 
    dplyr::left_join(day_night_detections, by = c("id", "sighted_at")) %>% 
    dplyr::mutate(sighted_at = lubridate::force_tz(sighted_at, tzone = "America/Los_Angeles")) %>%
    dplyr::mutate(
      time_of_day = dplyr::case_when(
        sighted_at >= dawn & sighted_at <= dusk ~ "day",
        TRUE ~ "night"
      )
    ) %>% 
    dplyr::mutate(
      month = lubridate::floor_date(sighted_at, unit = "month")
    ) %>%
    dplyr::group_by(month, time_of_day) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>% 
    dplyr::full_join(full_months, bar_data, by = c("month", "time_of_day")) %>%
    dplyr::mutate(count = tidyr::replace_na(count, 0),
                  month_label = factor(format(month, "%b"),levels = month.abb)) %>% 
    plotly::plot_ly(
      x = ~month_label,
      y = ~count,
      color = ~time_of_day,
      colors = c("day" = "#FDB813", "night" = "#2C3E50"),
      type = "bar") %>% 
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = ""),
      yaxis = list(title = "Detections"),
      legend = list(title = list(text = "<b>Time of Day</b>"))
    )
}

## UPDATE THESE TO RUN THE FUNCTION TO CREATE VISUALS WITH EITHER BOUNDARY PASS, HARO STRAIT, AND SWIFTSURE BANK

map_maker_function("Swiftsure Bank")
area_bar_maker_function("Swiftsure Bank")
make_regional_lines_function("Swiftsure Bank")
day_vs_night_region_function("Swiftsure Bank")

##~~~~~~~~~ sandbox ~~~~~~~~~~~####

# 
# heat_map_maker_function = function(area){
#   area_detections %>% 
#     dplyr::filter(region == area) %>% 
#     leaflet::leaflet() %>%
#     leaflet::addProviderTiles("CartoDB.Positron") %>%
#     leaflet::addTiles(
#       urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
#       attribution = 'Map data: &copy; <a href="https://www.openseamap.org">OpenSeaMap</a> contributors',
#       group = "OpenSeaMap"
#     ) %>%
#     leaflet.extras::addHeatmap(
#       blur = 5,
#       max = 0.4,
#       radius = 20) %>% 
#     leaflet::addControl(
#       html = "<div style='background:white;padding:8px;border-radius:5px;box-shadow:0 0 5px rgba(0,0,0,0.3);'>
#                   <b>Heatmap Intensity</b><br>
#                   <span style='color:#2CAED8;'>&#9632;</span> Low<br>
#                   <span style='color:#FFEDA0;'>&#9632;</span> Medium<br>
#                   <span style='color:#F03B20;'>&#9632;</span> High
#                 </div>",
#       position = "bottomright")
# }
                