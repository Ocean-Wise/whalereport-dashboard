####~~~~~~~~~~~~~~~~~~~~~~VFPA Reporting (New Structure)~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Generate VFPA reporting visualizations using sighting-based data structure
## Date written: 2025-12-11
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## This script uses the NEW sighting-based main_dataset structure
## See DATA_STRUCTURE_CHANGES.md for details on the new approach
##
## Key differences:
## - All sightings preserved (whether they generated alerts or not)
## - More accurate conversion rate analysis
## - Better coverage gap identification


####~~~~~~~~~~~~~~~~~~~~~~Configuration~~~~~~~~~~~~~~~~~~~~~~~####
## UPDATE THESE FOR YOUR REPORTING PERIOD

# Comparison periods
period_1_start = lubridate::as_date("2024-01-01")
period_1_end = lubridate::as_date("2024-12-31")
period_1_label = "2024"

period_2_start = lubridate::as_date("2025-01-01")
period_2_end = lubridate::as_date("2025-12-31")
period_2_label = "2025"

####~~~~~~~~~~~~~~~~~~~~~~Data Preparation~~~~~~~~~~~~~~~~~~~~~~~####

## Filter main dataset for both periods combined
analysis_data = main_dataset %>%
  dplyr::filter(
    report_sighting_date >= period_1_start,
    report_sighting_date <= period_2_end
  )

## Create period labels
analysis_data = analysis_data %>%
  dplyr::mutate(
    period = dplyr::case_when(
      report_sighting_date >= period_1_start & report_sighting_date <= period_1_end ~ period_1_label,
      report_sighting_date >= period_2_start & report_sighting_date <= period_2_end ~ period_2_label,
      TRUE ~ NA_character_
    )
  )

## Sightings dataset (one row per sighting)
period_sightings = sightings_main %>%
  dplyr::mutate(
    period = dplyr::case_when(
      sighting_date >= period_1_start & sighting_date <= period_1_end ~ period_1_label,
      sighting_date >= period_2_start & sighting_date <= period_2_end ~ period_2_label,
      TRUE ~ NA_character_
    )
  ) %>% 
  # dplyr::distinct(sighting_id, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(period)) %>%
  dplyr::select(sighting_id, sighting_date, sighting_year_month, period, 
                species_name, comments, report_source_entity, 
                report_latitude, report_longitude) %>% 
  # dplyr::filter(report_source_entity == "WhaleSpotter") %>% 
  dplyr::distinct()
  

## Alerts dataset
period_alerts = alerts_main %>%
  dplyr::mutate(
    period = dplyr::case_when(
      sighting_start >= period_1_start & sighting_start <= period_1_end ~ period_1_label,
      sighting_start >= period_2_start & sighting_start <= period_2_end ~ period_2_label,
      TRUE ~ NA_character_
    )
  ) %>% 
  dplyr::filter(!is.na(period)) %>%
  # dplyr::filter(delivery_successful == TRUE, !is.na(period)) %>%
  dplyr::distinct(alert_id, user_id, .keep_all = TRUE) %>%
  dplyr::select(sighting_id, user_id, alert_id,
                alert_created_at, alert_year_month, period,
                species_name, report_source_entity,
                report_latitude, report_longitude, context)

####~~~~~~~~~~~~~~~~~~~~~~Helper Functions~~~~~~~~~~~~~~~~~~~~~~~####

## Ensure months with zero counts are included
complete_months = function(data, period_col = "period") {
  # Create all months for period 1
  period_1_months = zoo::as.yearmon(seq.Date(
    from = period_1_start,
    to = period_1_end,
    by = "month"
  ))

  # Create all months for period 2
  period_2_months = zoo::as.yearmon(seq.Date(
    from = period_2_start,
    to = period_2_end,
    by = "month"
  ))

  # Combine and create grid with all sources
  all_months = tidyr::expand_grid(
    year_month = c(period_1_months, period_2_months),
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
  dplyr::summarise(sightings = dplyr::n(), .groups = "drop") %>%
  # Add missing months with 0 counts
  dplyr::right_join(
    complete_months(period_sightings),
    by = c("year_month", "report_source_entity", "period")
  ) %>%
  dplyr::mutate(sightings = tidyr::replace_na(sightings, 0))

## Aggregate alerts by month and source
alerts_by_month = period_alerts %>%
  dplyr::group_by(period, year_month = alert_year_month, report_source_entity) %>%
  dplyr::summarise(alerts = dplyr::n(), .groups = "drop") %>%
  # Add missing months with 0 counts
  dplyr::right_join(
    complete_months(period_alerts),
    by = c("year_month", "report_source_entity", "period")
  ) %>%
  dplyr::mutate(alerts = tidyr::replace_na(alerts, 0))

####~~~~~~~~~~~~~~~~~~~~~~Line Graph Function~~~~~~~~~~~~~~~~~~~~~~~####

make_comparison_lines = function(source, metric = "both") {
  # Join sightings and alerts
  joined_data = dplyr::left_join(
    sightings_by_month,
    alerts_by_month,
    by = c("year_month", "report_source_entity", "period")
  )

  # Special handling for WhaleSpotter - aggregate all sources containing "whalespotter"
  # Comment out the if block below to use only exact "WhaleSpotter" source
  if (source == "WhaleSpotter") {
    plot_data = joined_data %>%
      dplyr::filter(grepl("whalespotter", report_source_entity, ignore.case = TRUE)) %>%
      dplyr::mutate(
        month_num = as.numeric(format(year_month, "%m")),
        month = factor(format(year_month, "%b"), levels = month.abb, ordered = TRUE)
      ) %>%
      dplyr::group_by(month, month_num, period) %>%
      dplyr::summarise(
        sightings = sum(sightings, na.rm = TRUE),
        alerts = sum(alerts, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      tidyr::pivot_wider(
        names_from = period,
        values_from = c(sightings, alerts),
        values_fill = 0,
        values_fn = sum
      ) %>%
      dplyr::arrange(month_num)
  } else {
    # Standard filtering for other sources
    plot_data = joined_data %>%
      # dplyr::filter(report_source_entity == source) %>%
      dplyr::filter(stringr::str_detect(report_source_entity, source)) %>%
      dplyr::mutate(
        # Extract month number and name BEFORE pivoting
        month_num = as.numeric(format(year_month, "%m")),
        month = factor(format(year_month, "%b"), levels = month.abb, ordered = TRUE)
      ) %>%
      # Select only the columns we need (drop year_month to avoid duplication)
      dplyr::select(month, month_num, period, sightings, alerts) %>%
      # Pivot to get one row per month (not per year_month)
      tidyr::pivot_wider(
        names_from = period,
        values_from = c(sightings, alerts),
        values_fill = 0,
        values_fn = sum  # Sum values for the same month across different years
      ) %>%
      # Ensure months are in calendar order (Jan-Dec)
      dplyr::arrange(month_num)
  }

  # Create empty plot object
  p = plotly::plot_ly(data = plot_data)

  # Add traces based on metric parameter
  if (metric %in% c("both", "sightings")) {
    p = p %>%
      plotly::add_trace(
        x = ~month,
        y = ~get(paste0("sightings_", period_2_label)),
        name = paste("Sightings", period_2_label),
        type = "scatter",
        mode = "lines",
        line = list(color = ocean_wise_palette["Coral"], dash = "solid", width = 2)
      ) %>%
      plotly::add_trace(
        x = ~month,
        y = ~get(paste0("sightings_", period_1_label)),
        name = paste("Sightings", period_1_label),
        type = "scatter",
        mode = "lines",
        line = list(color = ocean_wise_palette["Tide"], dash = "solid", width = 2)
      )
  }

  if (metric %in% c("both", "alerts")) {
    p = p %>%
      plotly::add_trace(
        x = ~month,
        y = ~get(paste0("alerts_", period_2_label)),
        name = paste("Alerts", period_2_label),
        type = "scatter",
        mode = "lines",
        line = list(color = ocean_wise_palette["Coral"], dash = "dash", width = 2)
      ) %>%
      plotly::add_trace(
        x = ~month,
        y = ~get(paste0("alerts_", period_1_label)),
        name = paste("Alerts", period_1_label),
        type = "scatter",
        mode = "lines",
        line = list(color = ocean_wise_palette["Tide"], dash = "dash", width = 2)
      )
  }

  # Layout with explicit axis configuration
  p = p %>%
    plotly::layout(
      xaxis = list(
        title = "",
        showgrid = FALSE,
        tickfont = list(size = 12, weight = "600"),
        categoryorder = "array",
        categoryarray = month.abb
      ),
      yaxis = list(
        title = "",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600"),
        rangemode = "tozero"
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.1
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
# make_comparison_lines("Whale Alert")

####~~~~~~~~~~~~~~~~~~~~~~Day vs Night Analysis~~~~~~~~~~~~~~~~~~~~~~~####

day_vs_night_analysis = function(sources, year) {
  
  # Get all unique source entities in the dataset
  available_sources = period_sightings %>%
    dplyr::pull(report_source_entity) %>%
    unique()
  
  # Expand sources using pattern matching
  expanded_sources = c()
  for (source_pattern in "WhaleSpotter") {
    if (is.na(source_pattern)) {
      expanded_sources = c(expanded_sources, NA)
    } else {
      matches = available_sources[grepl(source_pattern, available_sources, ignore.case = TRUE)]
      expanded_sources = c(expanded_sources, matches)
    }
  }
  expanded_sources = unique(expanded_sources)
  
  # Filter data - simplified distinct() to just use sighting_date
  data = period_sightings %>%
    dplyr::filter(
      report_source_entity %in% expanded_sources,
      lubridate::year(sighting_date) == 2025
    ) %>%
    dplyr::mutate(
      sighting_date = dplyr::case_when(
        is.na(comments) == F ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
        is.na(comments) == T ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
      )) %>% 
    dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
    dplyr::distinct(sighting_date, .keep_all = TRUE) # keep first occurrence of each datetime
  
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
    dplyr::select(sighting_date, date, dawn = sun_info_dawn, dusk = sun_info_dusk)  # Fixed to include sighting_date
  
  # Join back and classify
  result = data %>%
    dplyr::left_join(day_night, by = c("sighting_date", "date")) %>%
    dplyr::mutate(
      time_of_day = dplyr::case_when(
        sighting_date >= dawn & sighting_date <= dusk ~ "day",
        TRUE ~ "night"
      ),
      month = lubridate::floor_date(sighting_date, unit = "month")
    ) %>% 
    dplyr::distinct()
  
  # Aggregate by month
  monthly_counts = result %>%
    dplyr::group_by(month, time_of_day) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(
      month = seq.Date(
        from = lubridate::as_date(paste0("2025", "-01-01")),
        to = lubridate::as_date(paste0("2025", "-12-01")),
        by = "month"
      ),
      time_of_day = c("day", "night"),
      fill = list(count = 0)
    ) %>%
    dplyr::mutate(
      month_num = as.numeric(format(month, "%m")),
      month_label = factor(format(month, "%b"), levels = month.abb, ordered = TRUE)
    ) %>%
    dplyr::arrange(month_num, time_of_day)
  
  # Create plot
  p = plotly::plot_ly(
    data = monthly_counts,
    x = ~month_label,
    y = ~count,
    color = ~time_of_day,
    colors = c("day" = unname(ocean_wise_palette["Sun"]), "night" = unname(ocean_wise_palette["Anemone"])),
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600"),
        categoryorder = "array",
        categoryarray = month.abb
      ),
      yaxis = list(
        title = "Sightings",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600"),
        rangemode = "tozero"
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
day_vs_night_analysis(c("Ocean Wise", "Orca Network", "Whale Alert", "WhaleSpotter", "JASCO", "SMRU"), 2025)
day_vs_night_analysis(c("WhaleSpotter"), 2025)
day_vs_night_analysis(c("Ocean Wise", "Whale Alert", "Orca Network"), 2025)
day_vs_night_analysis(c("SMRU"), 2025)

####~~~~~~~~~~~~~~~~~~~~~~Stacked Bar Charts~~~~~~~~~~~~~~~~~~~~~~~####

## Sightings stacked bar by source
create_stacked_bar_sightings = function(year) {
  # Map standardized sources to specific palette colors
  source_colors = c(
    "Ocean Wise" = ocean_wise_palette[["Kelp"]],
    "Orca Network" = ocean_wise_palette[["Coral"]],
    "Whale Alert" = ocean_wise_palette[["Ocean"]],
    "SMRU" = ocean_wise_palette[["Anemone"]],
    "JASCO" = ocean_wise_palette[["Sun"]],
    "WhaleSpotter" = ocean_wise_palette[["Tide"]]
  )
  
  data = sightings_by_month %>%
    dplyr::filter(period == as.character(year)) %>%
    dplyr::mutate(
      # Standardize source names
      standardized_source = dplyr::case_when(
        grepl("Ocean Wise", report_source_entity, ignore.case = TRUE) ~ "Ocean Wise",
        grepl("WhaleSpotter", report_source_entity, ignore.case = TRUE) ~ "WhaleSpotter",
        grepl("Orca Network", report_source_entity, ignore.case = TRUE) ~ "Orca Network",
        grepl("Whale Alert", report_source_entity, ignore.case = TRUE) ~ "Whale Alert",
        grepl("JASCO", report_source_entity, ignore.case = TRUE) ~ "JASCO",
        grepl("SMRU", report_source_entity, ignore.case = TRUE) ~ "SMRU",
        TRUE ~ "Other"
      ),
      month_num = as.numeric(format(year_month, "%m")),
      month = factor(format(year_month, "%b"), levels = month.abb, ordered = TRUE)
    ) %>%
    # Group by standardized source and sum sightings
    dplyr::group_by(year_month, month_num, month, standardized_source) %>%
    dplyr::summarise(sightings = sum(sightings, na.rm = TRUE), .groups = "drop") %>%
    # Complete all months for all sources
    tidyr::complete(
      month = factor(month.abb, levels = month.abb, ordered = TRUE),
      standardized_source,
      fill = list(sightings = 0)
    ) %>%
    dplyr::mutate(month_num = as.numeric(month)) %>%
    # Ensure chronological order
    dplyr::arrange(month_num, standardized_source)
  
  p = plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~sightings,
    color = ~standardized_source,
    colors = source_colors,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600"),
        categoryorder = "array",
        categoryarray = month.abb
      ),
      yaxis = list(
        title = "Sightings",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600"),
        rangemode = "tozero"
      ),
      legend = list(
        title = list(text = "<b>Source</b>"),
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
  # Map standardized sources to specific palette colors
  source_colors = c(
    "Ocean Wise" = ocean_wise_palette[["Kelp"]],
    "Orca Network" = ocean_wise_palette[["Coral"]],
    "Whale Alert" = ocean_wise_palette[["Ocean"]],
    "SMRU" = ocean_wise_palette[["Anemone"]],
    "JASCO" = ocean_wise_palette[["Sun"]],
    "WhaleSpotter" = ocean_wise_palette[["Tide"]]
  )
  
  data = alerts_by_month %>%
    dplyr::filter(period == as.character(year)) %>%
    dplyr::mutate(
      # Standardize source names
      standardized_source = dplyr::case_when(
        grepl("Ocean Wise", report_source_entity, ignore.case = TRUE) ~ "Ocean Wise",
        grepl("WhaleSpotter", report_source_entity, ignore.case = TRUE) ~ "WhaleSpotter",
        grepl("Orca Network", report_source_entity, ignore.case = TRUE) ~ "Orca Network",
        grepl("Whale Alert", report_source_entity, ignore.case = TRUE) ~ "Whale Alert",
        grepl("JASCO", report_source_entity, ignore.case = TRUE) ~ "JASCO",
        grepl("SMRU", report_source_entity, ignore.case = TRUE) ~ "SMRU",
        TRUE ~ "Ocean Wise"
      ),
      month_num = as.numeric(format(year_month, "%m")),
      month = factor(format(year_month, "%b"), levels = month.abb, ordered = TRUE)
    ) %>%
    # Group by standardized source and sum alerts
    dplyr::group_by(year_month, month_num, month, standardized_source) %>%
    dplyr::summarise(alerts = sum(alerts, na.rm = TRUE), .groups = "drop") %>%
    # Complete all months for all sources
    tidyr::complete(
      month = factor(month.abb, levels = month.abb, ordered = TRUE),
      standardized_source,
      fill = list(alerts = 0)
    ) %>%
    dplyr::mutate(month_num = as.numeric(month)) %>%
    # Ensure chronological order
    dplyr::arrange(month_num, standardized_source)
  
  p = plotly::plot_ly(
    data = data,
    x = ~month,
    y = ~alerts,
    color = ~standardized_source,
    colors = source_colors,
    type = "bar"
  ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(
        title = "",
        tickfont = list(size = 12, weight = "600"),
        categoryorder = "array",
        categoryarray = month.abb
      ),
      yaxis = list(
        title = "Alerts",
        tickformat = ",",
        tickfont = list(size = 12, weight = "600"),
        rangemode = "tozero"
      ),
      legend = list(
        title = list(text = "<b>Source</b>"),
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      )
    )
  
  return(p)
}

# Example usage:
create_stacked_bar_sightings(2025)
create_stacked_bar_alerts(2025)

####~~~~~~~~~~~~~~~~~~~~~~Summary Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Total quarterly sightings
quarterly_sightings = period_sightings %>%
  dplyr::group_by(period, year_month = sighting_year_month) %>%
  dplyr::summarise(total_sightings = dplyr::n(), .groups = "drop")

## Total quarterly alerts
quarterly_alerts = period_alerts %>%
  dplyr::group_by(period, year_month = sighting_year_month) %>%
  dplyr::summarise(total_alerts = dplyr::n(), .groups = "drop")

####~~~~~~~~~~~~~~~~~~~~~~Data Tables~~~~~~~~~~~~~~~~~~~~~~~####

## WhaleSpotter monthly summary table
create_whalespotter_table = function(year) {
  # Filter for WhaleSpotter sources (case-insensitive)
  whalespotter_sightings = period_sightings %>%
    dplyr::filter(
      grepl("whalespotter", report_source_entity, ignore.case = TRUE)) %>%
    dplyr::distinct(sighting_date, .keep_all = TRUE) %>% 
    dplyr::filter(lubridate::year(sighting_date) == year)
  
  whalespotter_alerts = period_alerts %>%
    dplyr::filter(
      grepl("whalespotter", report_source_entity, ignore.case = TRUE),
      lubridate::year(alert_created_at) == year)
  
  # Aggregate by month
  monthly_summary = whalespotter_sightings %>%
    dplyr::mutate(
      month = lubridate::floor_date(sighting_date, unit = "month"),
      month_label = format(month, "%b")
    ) %>%
    dplyr::group_by(month, month_label) %>%
    dplyr::summarise(
      total_detections = dplyr::n(),
      total_validated_kw = sum(grepl("killer whale", species_name, ignore.case = TRUE)),
      total_validated_srkw = sum(grepl("southern resident", species_name, ignore.case = TRUE)),
      total_validated_humpback = sum(grepl("humpback", species_name, ignore.case = TRUE)),
      .groups = "drop"
    )
  
  # Count detections that produced alerts
  alerted_sightings = whalespotter_alerts %>%
    dplyr::mutate(month = lubridate::floor_date(alert_created_at, unit = "month")) %>%  # Changed to sighting_date
    dplyr::group_by(month) %>%
    dplyr::summarise(
      detections_with_alerts = dplyr::n_distinct(alert_created_at),
      .groups = "drop"
    )
  
  # Join and calculate final table
  final_table = monthly_summary %>%
    dplyr::left_join(alerted_sightings, by = "month") %>%
    dplyr::mutate(
      detections_with_alerts = tidyr::replace_na(detections_with_alerts, 0),
      false_positive_rate = "0%",
      effort = "100%",
      system_availability = "Unknown"
    ) %>%
    # Ensure all months present
    tidyr::complete(
      # month = seq.Date(
      #   from = lubridate::as_date(paste0(year, "-01-01")),
      #   to = lubridate::as_date(paste0(year, "-12-01")),
      #   by = "month"
      # ),
      fill = list(
        total_detections = 0,
        total_validated_kw = 0,
        total_validated_srkw = 0,
        total_validated_humpback = 0,
        detections_with_alerts = 0,
        false_positive_rate = "0%",
        effort = "100%",
        system_availability = "Unknown"
      )
    ) %>%
    dplyr::mutate(
      month_label = format(month, "%b")
    ) %>%
    dplyr::select(
      Month = month_label,
      `Total Detections` = total_detections,
      `Total Validated KW Events` = total_validated_kw,
      `Total Validated SRKW Events` = total_validated_srkw,
      `Total Validated Humpback Whale Events` = total_validated_humpback,
      `Detections That Produced Alerts` = detections_with_alerts,
      `False Positive Rate` = false_positive_rate,
      Effort = effort,
      `System Availability` = system_availability
    )
  
  return(final_table)
}

# Example usage:
View(create_whalespotter_table(2025))

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
  
  # Categorize sources using pattern matching
  map_data = map_data %>%
    dplyr::mutate(
      detection_method = dplyr::case_when(
        grepl("WhaleSpotter", report_source_entity, ignore.case = TRUE) ~ "Thermal Camera",
        grepl("Ocean Wise", report_source_entity, ignore.case = TRUE) ~ "Whale Report App",
        grepl("JASCO|SMRU", report_source_entity, ignore.case = TRUE) ~ "Hydrophone",
        grepl("Orca Network|Whale Alert", report_source_entity, ignore.case = TRUE) ~ "Partner Sighting Networks",
        TRUE ~ "Other"
      ),
      col_palette = dplyr::case_when(
        detection_method == "Thermal Camera" ~ unname(ocean_wise_palette["Coral"]),
        detection_method == "Whale Report App" ~ unname(ocean_wise_palette["Kelp"]),
        detection_method == "Hydrophone" ~ unname(ocean_wise_palette["Ocean"]),
        detection_method == "Partner Sighting Networks" ~ unname(ocean_wise_palette["Sun"]),
        TRUE ~ unname(ocean_wise_palette["Dolphin"])
      ),
      popup_content = paste(
        "<b>Species:</b>", species_name,
        "<br><b>Source:</b>", report_source_entity,
        "<br><b>Detection method:</b>", detection_method,
        "<br><b>Date:</b>", as.Date(sighting_date)
      )
    )
  
  # Create legend data
  legend_data = map_data %>%
    dplyr::distinct(detection_method, col_palette) %>%
    dplyr::arrange(detection_method)
  
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
      colors = legend_data$col_palette,
      labels = legend_data$detection_method,
      opacity = 0.8
    ) %>%
    leaflet::addMiniMap(toggleDisplay = TRUE)
  
  return(map)
}

# Example usage:
create_sightings_map(2025)
create_sightings_map(2025, include_non_alerting = TRUE)

####~~~~~~~~~~~~~~~~~~~~~~Regional Analysis~~~~~~~~~~~~~~~~~~~~~~~####
## Load shapefiles from SharePoint

sharepoint_site_url = "https://vamsc.sharepoint.com/sites/MMRP"

## Step 1: Connect to SharePoint and download shapefiles
site = Microsoft365R::get_sharepoint_site(site_url = sharepoint_site_url)
drive = site$get_drive()
folder = drive$get_item("General/Ocean Wise Data/Shapefiles/echo-data-filter")
files = folder$list_items()

# Download all ECHO files to tempdir
echo_files = files$name[grepl("echo", files$name, ignore.case = TRUE)]

lapply(echo_files, function(fname) {
  folder$get_item(fname)$download(file.path(tempdir(), fname))
})

## Step 2: Load and prepare shapefiles
shapefiles = sf::st_read(file.path(tempdir(), "echo-slowdown.shp")) %>%
  dplyr::mutate(
    region = dplyr::case_when(
      grepl("swiftsure", Name, ignore.case = TRUE) ~ "Swiftsure Bank",
      TRUE ~ Name
    )
  ) %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(geometry = sf::st_union(geometry)) %>%
  sf::st_as_sf()

## Step 3: Create regional datasets
area_sightings = period_sightings %>%
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326) %>%
  sf::st_join(shapefiles, left = TRUE) %>%
  dplyr::filter(!is.na(region))

area_alerts = period_alerts %>%
  dplyr::filter(is.na(report_latitude)==F) %>% 
  sf::st_as_sf(coords = c("report_longitude", "report_latitude"), crs = 4326) %>%
  sf::st_join(shapefiles, left = TRUE) %>%
  dplyr::filter(!is.na(region))

## Map maker function
map_maker_function = function(area, year) {
  area_data = area_sightings %>%
    dplyr::filter(region == area, lubridate::year(sighting_date) == year) %>%
    dplyr::mutate(
      species_clean = dplyr::case_when(
        stringr::str_detect(species_name, "dolphin") ~ "Dolphin/Porpoise species",
        stringr::str_detect(species_name, "porpoise") ~ "Dolphin/Porpoise species",
        stringr::str_detect(species_name, "turtle") ~ "Potential Turtle species",
        stringr::str_detect(species_name, "False") ~ "Dolphin/Porpoise species",
        stringr::str_detect(species_name, "Sei") ~ "Unidentified whale",
        .default = as.character(species_name)
      ),
      col_palette = dplyr::case_when(
        species_clean == "Minke whale" ~ "#A569BD",
        species_clean == "Killer whale" ~ "#17202A",
        species_clean == "Humpback whale" ~ "#E74C3C",
        species_clean == "Fin whale" ~ "#F4D03F",
        species_clean == "Dolphin/Porpoise species" ~ "#566573",
        species_clean == "Grey whale" ~ "#AAB7B8",
        species_clean == "Dolphin species" ~ "#1ABC9C",
        species_clean == "Sperm whale" ~ "blue",
        species_clean == "Unidentified whale" ~ "#B7950B",
        species_clean == "Potential Turtle species" ~ "darkgreen"
      )
    )

  # Extract coordinates from geometry
  coords = sf::st_coordinates(area_data)
  area_data$longitude = coords[, "X"]
  area_data$latitude = coords[, "Y"]

  leaflet::leaflet(data = area_data) %>%
    leaflet::addProviderTiles("CartoDB.Positron") %>%
    leaflet::addTiles(
      urlTemplate = "https://tiles.openseamap.org/seamark/{z}/{x}/{y}.png",
      attribution = 'Map data: &copy; <a href="https://www.openseamap.org">OpenSeaMap</a> contributors',
      group = "OpenSeaMap"
    ) %>%
    leaflet::addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      color = ~col_palette,
      radius = 5,
      stroke = FALSE,
      fillOpacity = 0.8
    ) %>%
    leaflet::addLegend(
      "bottomright",
      colors = unique(area_data$col_palette),
      labels = unique(area_data$species_clean),
      opacity = 0.8
    ) %>%
    leaflet::setView(lng = -123.5, lat = 48.5, zoom = 8)
}

## Regional lines function - detections and alerts over time
make_regional_lines_function = function(area) {
  # Sightings by month
  detections = area_sightings %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(region == area) %>%
    dplyr::group_by(year_month = sighting_year_month) %>%
    dplyr::summarize(detections = dplyr::n(), .groups = "drop")
  
  # Alerts by month
  alerts = area_alerts %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(region == area) %>%
    dplyr::group_by(year_month = alert_year_month) %>%
    dplyr::summarize(alerts = dplyr::n(), .groups = "drop")
  
  # Join and prepare plot data
  plot_data = dplyr::full_join(detections, alerts, by = "year_month") %>%
    dplyr::mutate(
      month = format(year_month, "%b"),  # format() works directly on yearmon
      year = as.integer(format(year_month, "%Y"))
    ) %>%
    tidyr::pivot_wider(
      id_cols = month,
      names_from = year, 
      values_from = c(detections, alerts)
    ) %>%
    # Force complete 12-month axis
    dplyr::right_join(
      tibble::tibble(month = month.abb),
      by = "month"
    ) %>%
    dplyr::mutate(
      month = factor(month, levels = month.abb, ordered = TRUE),
      # Replace NAs with 0 in all detection/alert columns
      dplyr::across(
        .cols = dplyr::starts_with(c("detections_", "alerts_")),
        .fns = ~tidyr::replace_na(.x, 0)
      )
    ) %>%
    dplyr::arrange(month)
  
  # Create plot
  plotly::plot_ly(
    data = plot_data,
    x = ~month,
    y = ~detections_2025,
    name = "Detections 2025",
    type = "scatter",
    mode = "lines",
    line = list(color = "#800080", dash = "solid"),
    connectgaps = FALSE
  ) %>%
    plotly::add_trace(
      y = ~detections_2024,
      name = "Detections 2024",
      line = list(color = "#D8BFD8", dash = "solid"),
      connectgaps = FALSE
    ) %>%
    plotly::add_trace(
      y = ~alerts_2025,
      name = "Alerts 2025",
      line = list(color = "#800080", dash = "dash"),
      connectgaps = FALSE
    ) %>%
    plotly::add_trace(
      y = ~alerts_2024,
      name = "Alerts 2024",
      line = list(color = "#D8BFD8", dash = "dash"),
      connectgaps = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(title = "", showgrid = FALSE),
      yaxis = list(title = ""),
      legend = list(title = list(text = "<b>Metric</b>"))
    )
}

## Stacked bar by source for a region
area_bar_maker_function = function(area, year) {
  area_sightings %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(region == area, lubridate::year(sighting_date) == year) %>%
    dplyr::mutate(year_month = zoo::as.yearmon(sighting_date)) %>%
    dplyr::group_by(year_month, report_source_entity) %>%
    dplyr::summarise(detections = dplyr::n(), .groups = "drop") %>%
    tidyr::complete(
      report_source_entity,
      year_month = zoo::as.yearmon(paste(year, month.abb), format = "%Y %b")
    ) %>%
    dplyr::mutate(
      detections = tidyr::replace_na(detections, 0),
      month = factor(format(year_month, "%b"), levels = month.abb)
    ) %>%
    plotly::plot_ly(
      x = ~month,
      y = ~detections,
      color = ~report_source_entity,
      type = "bar"
    ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = "", categoryorder = "array", categoryarray = month.abb),
      yaxis = list(title = "Detections"),
      legend = list(
        title = list(text = "<b>Source</b>"),
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.1
      )
    )
}

## Day vs night for a region
day_vs_night_region_function = function(area, year) {
  # Calculate day/night for each sighting in the region
  day_night_detections = area_sightings %>%
    dplyr::filter(region == area, lubridate::year(sighting_date) == year) %>%
    dplyr::mutate(
      date = lubridate::as_date(sighting_date),
      report_longitude = sf::st_coordinates(geometry)[, 1],  # Extract longitude from geometry
      report_latitude = sf::st_coordinates(geometry)[, 2]    # Extract latitude from geometry
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::distinct(sighting_date, date, report_latitude, report_longitude) %>%
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
    dplyr::select(sighting_date, dawn = sun_info_dawn, dusk = sun_info_dusk)
  
  # Complete months grid
  full_months = tidyr::expand_grid(
    month = seq.Date(
      from = lubridate::as_date(paste0(year, "-01-01")),
      to = lubridate::as_date(paste0(year, "-12-01")),
      by = "month"
    ),
    time_of_day = c("day", "night")
  )
  
  # Join and classify
  area_sightings %>%
    sf::st_drop_geometry() %>%
    dplyr::filter(region == area, lubridate::year(sighting_date) == year) %>%
    dplyr::left_join(day_night_detections, by = "sighting_date") %>%
    dplyr::mutate(
      sighting_date = dplyr::case_when(
        lubridate::month(sighting_date) < 5 ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
        lubridate::month(sighting_date) > 4 ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
      ),
        # lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      time_of_day = dplyr::case_when(
        sighting_date >= dawn & sighting_date <= dusk ~ "day",
        TRUE ~ "night"
      ),
      month = lubridate::floor_date(sighting_date, unit = "month")
    ) %>%
    dplyr::group_by(month, time_of_day) %>%
    dplyr::summarise(count = dplyr::n(), .groups = "drop") %>%
    dplyr::full_join(full_months, by = c("month", "time_of_day")) %>%
    dplyr::mutate(
      count = tidyr::replace_na(count, 0),
      month_label = factor(format(month, "%b"), levels = month.abb)
    ) %>%
    plotly::plot_ly(
      x = ~month_label,
      y = ~count,
      color = ~time_of_day,
      colors = c("day" = "#FDB813", "night" = "#2C3E50"),
      type = "bar"
    ) %>%
    plotly::layout(
      barmode = "stack",
      xaxis = list(title = ""),
      yaxis = list(title = "Detections"),
      legend = list(title = list(text = "<b>Time of Day</b>"))
    )
}

## Example usage (uncomment after loading shapefiles):
map_maker_function("Haro Strait", 2025)
map_maker_function("Boundary Pass", 2025)
map_maker_function("Swiftsure Bank", 2025)
area_bar_maker_function("Haro Strait", 2025)
area_bar_maker_function("Boundary Pass", 2025)
area_bar_maker_function("Swiftsure Bank", 2025)
make_regional_lines_function("Haro Strait")
make_regional_lines_function("Boundary Pass")
make_regional_lines_function("Swiftsure Bank")
day_vs_night_region_function("Haro Strait", 2025)
day_vs_night_region_function("Boundary Pass", 2025)
day_vs_night_region_function("Swiftsure Bank", 2025)

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

####~~~~~~~~~~~~~~~~~~~~~~Summary Statistics~~~~~~~~~~~~~~~~~~~~~~~####

cat("\n====== VFPA Reporting Summary ======\n")
cat("Period 1:", period_1_label, "-", format(period_1_start, "%Y-%m-%d"), "to", format(period_1_end, "%Y-%m-%d"), "\n")
cat("Period 2:", period_2_label, "-", format(period_2_start, "%Y-%m-%d"), "to", format(period_2_end, "%Y-%m-%d"), "\n\n")

cat("--- Period 1 (", period_1_label, ") ---\n", sep = "")
cat("Total sightings:", sum(sightings_by_month$sightings[sightings_by_month$period == period_1_label]), "\n")
cat("Total alerts:", sum(alerts_by_month$alerts[alerts_by_month$period == period_1_label]), "\n")
cat("Unique recipients:", dplyr::n_distinct(period_alerts$user_id[period_alerts$period == period_1_label], na.rm = TRUE), "\n\n")

cat("--- Period 2 (", period_2_label, ") ---\n", sep = "")
cat("Total sightings:", sum(sightings_by_month$sightings[sightings_by_month$period == period_2_label]), "\n")
cat("Total alerts:", sum(alerts_by_month$alerts[alerts_by_month$period == period_2_label]), "\n")
cat("Unique recipients:", dplyr::n_distinct(period_alerts$user_id[period_alerts$period == period_2_label], na.rm = TRUE), "\n\n")

cat("=====================================\n")