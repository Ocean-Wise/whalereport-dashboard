####~~~~~~~~~~~~~~~~~~~~~~Simple Calendar Year Visualizations~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Create simple year-on-year comparison visualizations
## Date written: 2024-12-24

####~~~~~~~~~~~~~~~~~~~~~~Prerequisites~~~~~~~~~~~~~~~~~~~~~~~####
## Run these first:
##   source("./config.R")
##   source("./data-import.R")
##   source("./data-cleaning.R")

library(magrittr)
library(plotly)
library(leaflet)

####~~~~~~~~~~~~~~~~~~~~~~Configuration~~~~~~~~~~~~~~~~~~~~~~~####
## Set which years to compare (from config.R or override here)
viz_years = comparison_years  # e.g., c(2023, 2024, 2025)

####~~~~~~~~~~~~~~~~~~~~~~Data Prep~~~~~~~~~~~~~~~~~~~~~~~####

## Filter data to comparison years
sightings_viz = sightings_main %>%
  dplyr::filter(sighting_year %in% viz_years)

alerts_viz = alerts_main %>%
  dplyr::filter(alert_year %in% viz_years)

main_viz = main_dataset %>%
  dplyr::filter(alert_year %in% viz_years)

####~~~~~~~~~~~~~~~~~~~~~~1. Map by Year~~~~~~~~~~~~~~~~~~~~~~~####

viz_1_map = function() {
  # Prepare data with valid coordinates
  map_data = sightings_viz %>%
    dplyr::filter(
      !is.na(report_latitude),
      !is.na(report_longitude)
    ) %>%
    dplyr::mutate(
      year_label = as.character(sighting_year),
      # Color palette for years
      col_palette = dplyr::case_when(
        sighting_year == viz_years[1] ~ "#A8007E",
        sighting_year == viz_years[2] ~ "#5FCBDA",
        length(viz_years) >= 3 & sighting_year == viz_years[3] ~ "#A2B427",
        length(viz_years) >= 4 & sighting_year == viz_years[4] ~ "#354EB1",
        length(viz_years) >= 5 & sighting_year == viz_years[5] ~ "#FFCE34",
        TRUE ~ "#B1B1B1"
      ),
      popup_content = paste0(
        "<b>Year:</b> ", sighting_year, "<br>",
        "<b>Species:</b> ", species_name, "<br>",
        "<b>Source:</b> ", report_source_entity, "<br>",
        "<b>Date:</b> ", as.Date(sighting_date)
      )
    )

  # Create map
  map = leaflet::leaflet(data = map_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addTiles(
      urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
      attribution = 'Map data: &copy; <a href="https://www.openseamap.org">OpenSeaMap</a> contributors',
      group = "OpenSeaMap"
    ) %>%
    leaflet::addCircleMarkers(
      lng = ~report_longitude,
      lat = ~report_latitude,
      radius = 3,
      color = ~col_palette,
      fillOpacity = 0.6,
      opacity = 0.6,
      popup = ~popup_content
    ) %>%
    leaflet::addLegend(
      "bottomright",
      colors = unique(map_data$col_palette),
      labels = unique(map_data$year_label),
      opacity = 0.8
    ) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE)

  return(map)
}

####~~~~~~~~~~~~~~~~~~~~~~2. Detections by Year (Line)~~~~~~~~~~~~~~~~~~~~~~~####

viz_2_detections_line = function() {
  # Monthly detections by year
  data = sightings_viz %>%
    dplyr::group_by(year = sighting_year, month = sighting_month) %>%
    dplyr::summarise(detections = dplyr::n(), .groups = "drop")

  # Colors for years - newest (last) is purple, oldest (first) is light blue
  year_colors = setNames(
    c("#5FCBDA", "#A8007E", "#354EB1","#FFCE34")[1:length(viz_years)],
    as.character(viz_years)
  )

  # Create plot
  p = plotly::plot_ly()

  for (year in viz_years) {
    year_data = data %>% dplyr::filter(year == !!year)

    p = p %>%
      plotly::add_trace(
        data = year_data,
        x = ~month,
        y = ~detections,
        name = as.character(year),
        type = "scatter",
        mode = "lines",
        line = list(color = year_colors[as.character(year)], width = 3)
      )
  }

  p = p %>%
    plotly::layout(
      xaxis = list(
        title = list(text = "", standoff = 20),
        ticktext = paste0("<i>", month.abb, "</i>"),
        tickvals = 1:12,
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Detections", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "#000000",
        zerolinewidth = 2,
        rangemode = "tozero",
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "x unified"
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~3. Unique Sighters by Month (Grouped Bar)~~~~~~~~~~~~~~~~~~~~~~~####

viz_3_sighters_bar = function() {
  # Count unique observers per month by year using report_observer_id from main_dataset
  data = main_viz %>%
    dplyr::filter(!is.na(report_observer_id)) %>%
    dplyr::group_by(year = alert_year, month = alert_month) %>%
    dplyr::summarise(unique_sighters = dplyr::n_distinct(report_observer_id), .groups = "drop")

  # Colors: purple for 2025, blue for other year
  year_colors = sapply(viz_years, function(y) {
    if (y == 2025) "#A8007E" else "#5FCBDA"
  })
  names(year_colors) = as.character(viz_years)

  # Create grouped bar chart
  p = plotly::plot_ly()

  for (year in viz_years) {
    year_data = data %>% dplyr::filter(year == !!year)

    p = p %>%
      plotly::add_trace(
        data = year_data,
        x = ~month,
        y = ~unique_sighters,
        name = as.character(year),
        type = "bar",
        marker = list(color = year_colors[as.character(year)])
      )
  }

  p = p %>%
    plotly::layout(
      barmode = "group",
      xaxis = list(
        title = list(text = "", standoff = 20),
        ticktext = paste0("<i>", month.abb, "</i>"),
        tickvals = 1:12,
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Unique Sighters", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "#000000",
        zerolinewidth = 2,
        rangemode = "tozero",
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~4. Detections by Source (Continuous Stacked Bar)~~~~~~~~~~~~~~~~~~~~~~~####

viz_4_source_stacked = function() {
  # Monthly detections by source across all years
  data = sightings_viz %>%
    dplyr::mutate(year_month = zoo::as.yearmon(sighting_date)) %>%
    dplyr::group_by(year_month, source = report_source_entity) %>%
    dplyr::summarise(detections = dplyr::n(), .groups = "drop")

  # Source colors
  unique_sources = unique(data$source)
  source_colors = setNames(get_ocean_wise_colors(length(unique_sources)), unique_sources)

  p = plotly::plot_ly(
    data = data,
    x = ~year_month,
    y = ~detections,
    color = ~source,
    colors = source_colors,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = list(text = "", standoff = 20),
        showgrid = FALSE,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Detections", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "darkgray",
        zerolinewidth = 1,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2)
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~5. Notifications by Year (Line)~~~~~~~~~~~~~~~~~~~~~~~####

viz_5_notifications_line = function() {
  # Monthly unique notifications by year (one per user per sighting)
  data = alerts_viz %>%
    dplyr::group_by(year = alert_year, month = alert_month) %>%
    dplyr::summarise(notifications = dplyr::n(), .groups = "drop")

  # Colors - newest (last) is purple, oldest (first) is light blue
  year_colors = setNames(
    c("#5FCBDA","#A8007E", "#354EB1", "#FFCE34", "#A8007E")[1:length(viz_years)],
    as.character(viz_years)
  )

  # Create plot
  p = plotly::plot_ly()

  for (year in viz_years) {
    year_data = data %>% dplyr::filter(year == !!year)

    p = p %>%
      plotly::add_trace(
        data = year_data,
        x = ~month,
        y = ~notifications,
        name = as.character(year),
        type = "scatter",
        mode = "lines",
        line = list(color = year_colors[as.character(year)], width = 3)
      )
  }

  p = p %>%
    plotly::layout(
      xaxis = list(
        title = list(text = "", standoff = 20),
        ticktext = paste0("<i>", month.abb, "</i>"),
        tickvals = 1:12,
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Unique Notifications", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "#000000",
        zerolinewidth = 2,
        rangemode = "tozero",
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      hovermode = "x unified"
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~6. Unique Users by Month (Line)~~~~~~~~~~~~~~~~~~~~~~~####

viz_6_users_bar = function() {
  # Count unique users receiving alerts per month by year
  # Count unique observers per month by year using user_id from main_dataset
  data = main_viz %>%
    dplyr::filter(!is.na(user_id)) %>%
    dplyr::group_by(year = alert_year, month = alert_month) %>%
    dplyr::summarise(unique_recipients = dplyr::n_distinct(user_id), .groups = "drop")
  
  # Colors: purple for 2025, blue for other year
  year_colors = sapply(viz_years, function(y) {
    if (y == 2025) "#A8007E" else "#5FCBDA"
  })
  names(year_colors) = as.character(viz_years)
  
  # Create grouped bar chart
  p = plotly::plot_ly()
  
  for (year in viz_years) {
    year_data = data %>% dplyr::filter(year == !!year)
    
    p = p %>%
      plotly::add_trace(
        data = year_data,
        x = ~month,
        y = ~unique_recipients,
        name = as.character(year),
        type = "bar",
        marker = list(color = year_colors[as.character(year)])
      )
  }
  
  p = p %>%
    plotly::layout(
      barmode = "group",
      xaxis = list(
        title = list(text = "", standoff = 20),
        ticktext = paste0("<i>", month.abb, "</i>"),
        tickvals = 1:12,
        showgrid = FALSE,
        zeroline = FALSE,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Unique Alert Recipients", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "#000000",
        zerolinewidth = 2,
        rangemode = "tozero",
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white"
    )
  
  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~7. Notification Type: Proximity vs Zone~~~~~~~~~~~~~~~~~~~~~~~####

viz_7_notification_type = function() {
  # Count by context type
  data = alerts_viz %>%
    dplyr::filter(
      context %in% c("current_location", "preferred_area")
    ) %>%
    dplyr::mutate(
      context_label = dplyr::case_when(
        context == "current_location" ~ "Proximity",
        context == "preferred_area" ~ "Zone of Interest"
      )
    ) %>%
    dplyr::group_by(year = alert_year, context_label) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  p = plotly::plot_ly(
    data = data,
    x = ~factor(year),
    y = ~count,
    color = ~context_label,
    colors = c("Proximity" = "#A8007E", "Zone of Interest" = "#5FCBDA"),
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "group",
      xaxis = list(
        title = list(text = "Year", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      yaxis = list(
        title = list(text = "Notifications", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "darkgray",
        zerolinewidth = 1,
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(title = list(text = "Type"))
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~8. Delivery Method: SMS vs Email~~~~~~~~~~~~~~~~~~~~~~~####

viz_8_delivery_method = function() {
  # Count by delivery method
  data = main_viz %>%
    dplyr::filter(
      delivery_successful == TRUE,
      alert_type_name %in% c("sms", "email")
    ) %>%
    dplyr::mutate(
      method_label = dplyr::case_when(
        alert_type_name == "sms" ~ "SMS",
        alert_type_name == "email" ~ "Email"
      )
    ) %>%
    dplyr::group_by(year = alert_year, method_label) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop")

  p = plotly::plot_ly(
    data = data,
    y = ~factor(year),
    x = ~count,
    color = ~method_label,
    colors = c("Email" = "#354EB1", "SMS" = "#A2B427"),
    type = "bar",
    orientation = "h"
  ) %>%
    plotly::layout(
      barmode = "group",
      yaxis = list(
        title = list(text = "Year", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      xaxis = list(
        title = list(text = "Notifications Sent", standoff = 20, font = list(size = 14, family = "Arial", weight = "bold")),
        showgrid = TRUE,
        gridcolor = "lightgray",
        zeroline = TRUE,
        zerolinecolor = "#000000",
        zerolinewidth = 2,
        rangemode = "tozero",
        tickfont = list(size = 14, family = "Arial", weight = "bold")
      ),
      plot_bgcolor = "white",
      paper_bgcolor = "white",
      legend = list(title = list(text = "Method"))
    )

  return(p)
}

####~~~~~~~~~~~~~~~~~~~~~~Generate All Visualizations~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n=== Generating Visualizations ===\n")
cat("Comparison years:", paste(viz_years, collapse = ", "), "\n\n")

# Generate all visualizations
map_viz = viz_1_map()
detections_line = viz_2_detections_line()
sighters_bar = viz_3_sighters_bar()
source_stacked = viz_4_source_stacked()
notifications_line = viz_5_notifications_line()
users_bar = viz_6_users_bar()
notification_type = viz_7_notification_type()
delivery_method = viz_8_delivery_method()

cat("✓ All visualizations created\n")
cat("==================================\n\n")

cat("View visualizations:\n")
cat("  map_viz              - Map of detections by year\n")
cat("  detections_line      - Detections over time (line)\n")
cat("  sighters_bar         - Unique sighters by month (line)\n")
cat("  source_stacked       - Detections by source (stacked)\n")
cat("  notifications_line   - Notifications over time (line)\n")
cat("  users_bar            - Unique WRAS users by month (line)\n")
cat("  notification_type    - Proximity vs Zone of Interest\n")
cat("  delivery_method      - SMS vs Email (horizontal)\n")
