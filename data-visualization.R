####~~~~~~~~~~~~~~~~~~~~~~VFPA Reporting (New Structure)~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Generate VFPA reporting visualizations using sighting-based data structure
## Date written: 2025-12-11
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####

####~~~~~~~~~~~~~~~~~~~~~~Prerequisites~~~~~~~~~~~~~~~~~~~~~~~####
## Run these first:
##   1. source("./config.R")
##   2. source("./data-import.R")
##   3. source("./data-cleaning.R")

library(magrittr)
library(plotly)
library(leaflet)
library(suncalc)
library(sf)

####~~~~~~~~~~~~~~~~~~~~~~Configuration~~~~~~~~~~~~~~~~~~~~~~~####
## UPDATE THESE FOR YOUR REPORTING PERIOD

# Comparison periods
period_1_start = lubridate::as_date("2024-01-01")
period_1_end = lubridate::as_date("2024-12-31")
period_1_label = "2024"

period_2_start = lubridate::as_date("2025-01-01")
period_2_end = lubridate::as_date("2025-10-31")
period_2_label = "2025"

####~~~~~~~~~~~~~~~~~~~~~~Data Preparation~~~~~~~~~~~~~~~~~~~~~~~####

## Filter main dataset for both periods combined
analysis_data = main_dataset %>%
  dplyr::filter(
    sighting_date >= period_1_start,
    sighting_date <= period_2_end
  )


## Create period labels
analysis_data = analysis_data %>%
  dplyr::mutate(
    period = dplyr::case_when(
      sighting_date >= period_1_start & sighting_date <= period_1_end ~ period_1_label,
      sighting_date >= period_2_start & sighting_date <= period_2_end ~ period_2_label,
      TRUE ~ NA_character_
    )
  )


## Sightings dataset (one row per sighting)
period_sightings = analysis_data %>%
  dplyr::distinct(sighting_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(period)) %>%
  dplyr::select(sighting_id, sighting_date, sighting_year_month, period, 
                species_name, report_source_entity, 
                report_latitude, report_longitude)

## Alerts dataset (only successful deliveries)
period_alerts = analysis_data %>%
  dplyr::filter(delivery_successful == TRUE, !is.na(period)) %>%
  dplyr::distinct(sighting_id, user_id, .keep_all = TRUE) %>%
  dplyr::select(sighting_id, alert_id, user_id, alert_user_created_at,
                sighting_date, sighting_year_month, period,
                species_name, report_source_entity,
                report_latitude, report_longitude, context)

####~~~~~~~~~~~~~~~~~~~~~~Helper Functions~~~~~~~~~~~~~~~~~~~~~~~####

## Ensure months with zero counts are included
complete_months = function(data, period_col = "period") {
  # Get all month combinations for both periods
  all_months = tidyr::expand_grid(
    year_month = zoo::as.yearmon(seq.Date(
      from = period_1_start,
      to = max(period_2_end, lubridate::today()),
      by = "month"
    )),
    report_source_entity = unique(data$report_source_entity)
  ) %>%
    dplyr::mutate(
      period = dplyr::case_when(
        year_month >= zoo::as.yearmon(period_1_start) & 
          year_month <= zoo::as.yearmon(period_1_end) ~ period_1_label,
        year_month >= zoo::as.yearmon(period_2_start) & 
          year_month <= zoo::as.yearmon(period_2_end) ~ period_2_label,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::filter(!is.na(period))
  
  return(all_months)
}

####~~~~~~~~~~~~~~~~~~~~~~Sightings & Alerts by Source~~~~~~~~~~~~~~~~~~~~~~~####

## Aggregate sightings by month and source
sightings_by_month = period_sightings %>%
  dplyr::group_by(period, year_month = sighting_year_month, report_source_entity) %>%
  dplyr::summarise(sightings = dplyr::n_distinct(sighting_id), .groups = "drop") %>%
  # Add missing months with 0 counts
  dplyr::right_join(
    complete_months(period_sightings),
    by = c("year_month", "report_source_entity", "period")
  ) %>%
  dplyr::mutate(sightings = tidyr::replace_na(sightings, 0)) %>% 
  dplyr::arrange(year_month)

## Aggregate alerts by month and source
alerts_by_month = period_alerts %>%
  dplyr::group_by(period, year_month = sighting_year_month, report_source_entity) %>%
  dplyr::summarise(alerts = dplyr::n(), .groups = "drop") %>%
  # Add missing months with 0 counts
  dplyr::right_join(
    complete_months(period_alerts),
    by = c("year_month", "report_source_entity", "period")
  ) %>%
  dplyr::mutate(alerts = tidyr::replace_na(alerts, 0)) %>% 
  dplyr::arrange(year_month)

## Joined sighting + alert data

combined_data = sightings_by_month %>% 
  dplyr::left_join(alerts_by_month,
                   by = c("year_month", "report_source_entity", "period"))
  


####~~~~~~~~~~~~~~~~~~~~~~Line Graph Function~~~~~~~~~~~~~~~~~~~~~~~####

make_comparison_lines = function(source) {
  # Filter for the specific source
  plot_data = combined_data %>%
    dplyr::filter(stringr::str_detect(report_source_entity, source)) %>%
    dplyr::mutate(
      month = factor(format(year_month, "%b"), levels = month.abb, ordered = TRUE)
    )
  
  # Separate by period
  data_2024 = plot_data %>% dplyr::filter(period == "2024")
  data_2025 = plot_data %>% dplyr::filter(period == "2025")
  
  # Create plot
  p = plotly::plot_ly() %>%
    # 2025 sightings (solid, coral)
    plotly::add_trace(
      data = data_2025,
      x = ~month,
      y = ~sightings,
      name = "Sightings 2025",
      type = "scatter",
      mode = "lines",
      line = list(color = ocean_wise_palette["Coral"], dash = "solid", width = 2)
    ) %>%
    # 2024 sightings (solid, tide)
    plotly::add_trace(
      data = data_2024,
      x = ~month,
      y = ~sightings,
      name = "Sightings 2024",
      type = "scatter",
      mode = "lines",
      line = list(color = ocean_wise_palette["Tide"], dash = "solid", width = 2)
    ) %>%
    # 2025 alerts (dashed, coral)
    plotly::add_trace(
      data = data_2025,
      x = ~month,
      y = ~alerts,
      name = "Alerts 2025",
      type = "scatter",
      mode = "lines",
      line = list(color = ocean_wise_palette["Coral"], dash = "dash", width = 2)
    ) %>%
    # 2024 alerts (dashed, tide)
    plotly::add_trace(
      data = data_2024,
      x = ~month,
      y = ~alerts,
      name = "Alerts 2024",
      type = "scatter",
      mode = "lines",
      line = list(color = ocean_wise_palette["Tide"], dash = "dash", width = 2)
    ) %>%
    # Layout
    plotly::layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        categoryorder = "array",
        categoryarray = month.abb
      ),
      yaxis = list(
        title = "",
        showgrid = TRUE,
        gridcolor = "rgba(200, 200, 200, 0.3)"
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      ),
      hovermode = "x unified"
    )
  
  return(p)
}
## Generate line graphs for each source
make_comparison_lines("JASCO")
make_comparison_lines("SMRU")
make_comparison_lines("WhaleSpotter")
make_comparison_lines("Ocean Wise")
make_comparison_lines("Orca Network")
make_comparison_lines("Whale Alert")

####~~~~~~~~~~~~~~~~~~~~~~Day vs Night Analysis~~~~~~~~~~~~~~~~~~~~~~~####

day_vs_night_analysis = function(sources, year) {
  # Filter data
  data = period_sightings %>%
    dplyr::filter(
      report_source_entity %in% sources,
      lubridate::year(sighting_date) == year
    ) %>%
    dplyr::mutate(date = sighting_date) %>%
    dplyr::distinct(sighting_id, date, report_latitude, report_longitude)
  
  # Calculate sunrise/sunset for each sighting
  day_night = data %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      sun_info = list(suncalc::getSunlightTimes(
        date = date,
        lat = report_latitude,
        lon = report_longitude,
        keep = c("dawn", "dusk"),
        tz = "America/Los_Angeles"
      ))
    ) %>%
    tidyr::unnest(cols = c(sun_info), names_sep = "_") %>%
    dplyr::select(sighting_id, date, dawn = sun_info_dawn, dusk = sun_info_dusk)
  
  # Join back and classify
  result = data %>%
    dplyr::left_join(day_night, by = c("sighting_id", "date")) %>%
    dplyr::left_join(
      period_sightings %>% dplyr::select(sighting_id, sighting_date),
      by = "sighting_id"
    ) %>%
    dplyr::mutate(
      sighting_datetime = lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      time_of_day = dplyr::case_when(
        sighting_datetime >= dawn & sighting_datetime <= dusk ~ "day",
        TRUE ~ "night"
      ),
      month = lubridate::floor_date(sighting_date, unit = "month")
    )
  
  # Aggregate by month
  monthly_counts = result %>%
    dplyr::group_by(month, time_of_day) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    # Ensure all months present
    tidyr::complete(
      month = seq.Date(
        from = lubridate::as_date(paste0(year, "-01-01")),
        to = lubridate::as_date(paste0(year, "-12-01")),
        by = "month"
      ),
      time_of_day = c("day", "night"),
      fill = list(count = 0)
    ) %>%
    dplyr::mutate(month_label = factor(format(month, "%b"), levels = month.abb))
  
  # Create plot
  p = plotly::plot_ly(
    data = monthly_counts,
    x = ~month_label,
    y = ~count,
    color = ~time_of_day,
    colors = c("day" = ocean_wise_palette["Sun"], "night" = ocean_wise_palette["Anemone"]),
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600")
      ),
      yaxis = list(
        title = "Sightings",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600")
      ),
      legend = list(
        title = list(text = "<b>Time of Day</b>"),
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      )
    )
  
  return(p)
}

# Example usage:
# day_vs_night_analysis(c("Ocean Wise", "Orca Network", "Whale Alert"), 2025)
# day_vs_night_analysis(c("WhaleSpotter"), 2025)
# day_vs_night_analysis(c("JASCO", "SMRU"), 2025)

####~~~~~~~~~~~~~~~~~~~~~~Stacked Bar Charts~~~~~~~~~~~~~~~~~~~~~~~####

## Sightings stacked bar by source
create_stacked_bar_sightings = function(year) {
  data = sightings_by_month %>%
    dplyr::filter(period == as.character(year)) %>%
    dplyr::mutate(
      month = factor(format(year_month, "%b"), levels = month.abb)
    )
  
  p = plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~sightings,
    color = ~report_source_entity,
    colors = ocean_wise_palette,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600")
      ),
      yaxis = list(
        title = "Sightings",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600")
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    )
  
  return(p)
}

## Alerts stacked bar by source
create_stacked_bar_alerts = function(year) {
  data = alerts_by_month %>%
    dplyr::filter(period == as.character(year)) %>%
    dplyr::mutate(
      month = factor(format(year_month, "%b"), levels = month.abb)
    )
  
  p = plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~alerts,
    color = ~report_source_entity,
    colors = ocean_wise_palette,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600")
      ),
      yaxis = list(
        title = "Alerts",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600")
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    )
  
  return(p)
}

# Example usage:
# create_stacked_bar_sightings(2025)
# create_stacked_bar_alerts(2025)

####~~~~~~~~~~~~~~~~~~~~~~Summary Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Total quarterly sightings
quarterly_sightings = period_sightings %>%
  dplyr::group_by(period, year_month = sighting_year_month) %>%
  dplyr::summarise(total_sightings = dplyr::n(), .groups = "drop")

## Total quarterly alerts
quarterly_alerts = period_alerts %>%
  dplyr::group_by(period, year_month = sighting_year_month) %>%
  dplyr::summarise(total_alerts = dplyr::n(), .groups = "drop")

####~~~~~~~~~~~~~~~~~~~~~~Maps~~~~~~~~~~~~~~~~~~~~~~~####

create_sightings_map = function(year, include_non_alerting = FALSE) {
  # Filter sightings
  map_data = period_sightings %>%
    dplyr::filter(period == as.character(year))
  
  # If we want to highlight non-alerting sightings
  if (include_non_alerting) {
    map_data = map_data %>%
      dplyr::left_join(
        analysis_data %>% 
          dplyr::distinct(sighting_id, has_alert),
        by = "sighting_id"
      ) %>%
      dplyr::mutate(
        has_alert = tidyr::replace_na(has_alert, FALSE)
      )
  }
  
  # Color mapping
  map_data = map_data %>%
    dplyr::mutate(
      col_palette = dplyr::case_when(
        report_source_entity == "WhaleSpotter" ~ ocean_wise_palette["Coral"],
        report_source_entity %in% c("Orca Network", "Whale Alert") ~ ocean_wise_palette["Sun"],
        report_source_entity == "Ocean Wise" ~ ocean_wise_palette["Kelp"],
        report_source_entity %in% c("JASCO", "SMRU") ~ ocean_wise_palette["Ocean"],
        TRUE ~ ocean_wise_palette["Dolphin"]
      ),
      detection_method = dplyr::case_when(
        report_source_entity == "WhaleSpotter" ~ "Infrared camera",
        report_source_entity %in% c("Orca Network", "Whale Alert") ~ "Partner sightings network",
        report_source_entity == "Ocean Wise" ~ "Whale report app",
        report_source_entity %in% c("JASCO", "SMRU") ~ "Hydrophone",
        TRUE ~ "Other"
      ),
      popup_content = paste(
        "<b>Species:</b>", species_name,
        "<br><b>Source:</b>", report_source_entity,
        "<br><b>Detection method:</b>", detection_method,
        "<br><b>Date:</b>", as.Date(sighting_date)
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
      labels = unique(map_data$detection_method),
      opacity = 0.8
    ) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE)
  
  return(map)
}

# Example usage:
# create_sightings_map(2025)
# create_sightings_map(2025, include_non_alerting = TRUE)

####~~~~~~~~~~~~~~~~~~~~~~Regional Analysis~~~~~~~~~~~~~~~~~~~~~~~####
## Load in shapefiles - 
sharepoint = Microsoft365R::get_sharepoint_site(
  "https://vamsc.sharepoint.com/sites/MMRP"
)

site = Microsoft365R::get_sharepoint_site(
  "https://vamsc.sharepoint.com/sites/MMRP",
  auth_type = "device_code"
)

sharepoint$list_drives()

# regional_analysis_function = function(shapefile_path, region_name) {
#   # Load shapefile
#   shapefiles = sf::st_read(shapefile_path) %>%
#     dplyr::mutate(
#       region = dplyr::case_when(
#         grepl("swiftsure", Name, ignore.case = TRUE) ~ "Swiftsure Bank",
#         TRUE ~ Name
#       )
#     ) %>%
#     dplyr::group_by(region) %>%
#     dplyr::summarise(geometry = sf::st_union(geometry)) %>%
#     sf::st_as_sf()
#   
#   # Filter sightings to region
#   area_sightings = period_sightings %>%
#     sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326) %>%
#     sf::st_join(shapefiles, left = TRUE) %>%
#     dplyr::filter(region == region_name)
#   
#   # Similar logic for alerts, maps, etc.
#   # ... (to be implemented based on specific needs)
# }

####~~~~~~~~~~~~~~~~~~~~~~Export Functions~~~~~~~~~~~~~~~~~~~~~~~####

## Save visualizations
save_vfpa_visuals = function(output_dir = "/mnt/user-data/outputs") {
  # Ensure output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Generate and save plots
  # (Add your specific visualizations here)
  
  message("VFPA visualizations saved to: ", output_dir)
}



#####~~~~~~~~~~~~~~~~~~~~~~SANDBOX~~~~~~~~~~~~~~~~~~~~~~~~~~~~#####


x = sightings_main %>% 
  dplyr::filter(stringr::str_detect(report_source_entity, stringr::regex("orca", ignore_case = TRUE))) %>% 
  dplyr::group_by(sighting_year_month) %>% 
  dplyr::summarise(new_db_count = dplyr::n()) %>% 
  dplyr::full_join(detections_clean %>% 
                     dplyr::filter(stringr::str_detect(source_entity, stringr::regex("orca", ignore_case = TRUE))) %>% 
                     dplyr::group_by(sighting_year_month = zoo::as.yearmon(sighted_at)) %>% 
                     dplyr::summarise(legacy_db_count = dplyr::n())) %>% 
  dplyr::mutate(difference = new_db_count - legacy_db_count)


detections_clean %>% 
  dplyr::filter(source_entity == "JASCO") %>% 
  dplyr::group_by(yearmon = zoo::as.yearmon(sighted_at)) %>% 
  dplyr::summarise(legacy_db_count = dplyr::n())



