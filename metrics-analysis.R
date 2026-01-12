####~~~~~~~~~~~~~~~~~~~~~~Metrics & Visualizations~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: Compare data from different years
## Date written: 2025-12-09

####~~~~~~~~~~~~~~~~~~~~~~Setup~~~~~~~~~~~~~~~~~~~~~~~####
## Note: Using namespacing instead of loading libraries

## Define comparison period
comparison_months = 1:12
comparison_years = c(2019,2020,2021,2022,2023)
## Flexible comparison years (can support up to 5 years)
## Inherits from config.R if available, otherwise uses default
if (!exists("comparison_years")) {
  comparison_years = c(2023, 2025)  # Default fallback
}

####~~~~~~~~~~~~~~~~~~~~~~Filter Data to Comparison Period~~~~~~~~~~~~~~~~~~~~~~~####

## Filter main_dataset for comparison period and exclude BCHN/SWAG
main_comparison = main_dataset %>%
  dplyr::filter(
    alert_year %in% comparison_years,
    alert_month %in% comparison_months
  )

## Apply BCHN/SWAG exclusion if exclude_sources exists from config.R
if (exists("exclude_sources") && length(exclude_sources) > 0) {
  main_comparison = main_comparison %>%
    dplyr::filter(!report_source_entity %in% exclude_sources | is.na(report_source_entity))
}

## Filter sightings_main for comparison period and exclude BCHN/SWAG
sightings_comparison = sightings_main %>%
  dplyr::filter(
    sighting_year %in% comparison_years,
    sighting_month %in% comparison_months
  )

## Apply BCHN/SWAG exclusion if exclude_sources exists from config.R
if (exists("exclude_sources") && length(exclude_sources) > 0) {
  sightings_comparison = sightings_comparison %>%
    dplyr::filter(!report_source_entity %in% exclude_sources | is.na(report_source_entity))
}

## Filter alerts_main for comparison period and exclude BCHN/SWAG
alerts_comparison = alerts_main %>%
  dplyr::filter(
    alert_year %in% comparison_years,
    alert_month %in% comparison_months
  )

## Apply BCHN/SWAG exclusion if exclude_sources exists from config.R
if (exists("exclude_sources") && length(exclude_sources) > 0) {
  alerts_comparison = alerts_comparison %>%
    dplyr::filter(!report_source_entity %in% exclude_sources | is.na(report_source_entity))
}

####~~~~~~~~~~~~~~~~~~~~~~1. Unique Users Receiving Notifications~~~~~~~~~~~~~~~~~~~~~~~####

## Average unique users per year
users_per_year = main_comparison %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(alert_year) %>%
  dplyr::summarise(
    total_unique_users = dplyr::n_distinct(user_id),
    months_in_period = dplyr::n_distinct(alert_month),
    avg_unique_users_per_month = total_unique_users / months_in_period
  )
print(users_per_year)

## Unique users per month
users_per_month = main_comparison %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(alert_year, alert_month) %>%
  dplyr::summarise(
    unique_users = dplyr::n_distinct(user_id),
    .groups = "drop"
  ) %>%
  dplyr::mutate(year_month = zoo::as.yearmon(paste(alert_year, alert_month, sep = "-")))

## Visualization: Unique Users per Month
p1 = plotly::plot_ly(users_per_month, 
                     x = ~alert_month, 
                     y = ~unique_users, 
                     color = ~factor(alert_year),
                     colors = setNames(c(ocean_wise_palette["Ocean"], ocean_wise_palette["Coral"]), c("2023", "2025")),
                     type = 'scatter',
                     mode = 'lines',
                     line = list(width = 3)) %>%
  plotly::layout(
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      ticktext = month.abb[4:12],
      tickvals = 4:12,
      title = ""
    ),
    yaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      title = "Unique recipients"
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p1)

htmlwidgets::saveWidget(p1, "/mnt/user-data/outputs/unique_users_per_month.html", selfcontained = TRUE)
## Try to save PNG if orca is available
tryCatch({
  plotly::orca(p1, "/mnt/user-data/outputs/unique_users_per_month.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})

####~~~~~~~~~~~~~~~~~~~~~~2. Geographic Map of Sightings~~~~~~~~~~~~~~~~~~~~~~~####

## Prepare sightings data with valid coordinates
sightings_map_data = sightings_comparison %>%
  dplyr::filter(
    !is.na(report_latitude),
    !is.na(report_longitude),
    report_latitude >= -90,
    report_latitude <= 90,
    report_longitude >= -180,
    report_longitude <= 180
  )

## Create worldwide map with plotly
p2 = plotly::plot_geo(sightings_map_data,
                      lat = ~report_latitude,
                      lon = ~report_longitude,
                      color = ~factor(sighting_year),
                      colors = setNames(c(ocean_wise_palette["Ocean"], ocean_wise_palette["Coral"]), c("2023", "2025")),
                      marker = list(size = 6, opacity = 0.6)) %>%
  plotly::layout(
    geo = list(
      projection = list(type = 'natural earth'),
      showland = TRUE,
      landcolor = 'lightgray',
      coastlinecolor = 'white',
      showframe = FALSE,
      showcountries = FALSE
    ),
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p2)
htmlwidgets::saveWidget(p2, "/mnt/user-data/outputs/sightings_map_worldwide.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p2, "/mnt/user-data/outputs/sightings_map_worldwide.png", width = 1400, height = 800)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})



####~~~~~~~~~~~~~~~~~~~~~~3. Total Sightings Submitted per Month~~~~~~~~~~~~~~~~~~~~~~~####

## Sightings per month
sightings_per_month = sightings_comparison %>%
  dplyr::group_by(sighting_year, sighting_month) %>%
  dplyr::summarise(
    total_sightings = dplyr::n(),
    .groups = "drop"
  )

## Calculate averages per year
sightings_avg_per_year = sightings_per_month %>%
  dplyr::group_by(sighting_year) %>%
  dplyr::summarise(
    total_sightings = sum(total_sightings),
    avg_sightings_per_month = mean(total_sightings),
    .groups = "drop"
  )

print("=== Sightings Submitted ===")
print(sightings_avg_per_year)

## Visualization: Sightings per Month
p3 = plotly::plot_ly(sightings_per_month,
                     x = ~sighting_month,
                     y = ~total_sightings,
                     color = ~factor(sighting_year),
                     colors = setNames(c(ocean_wise_palette["Ocean"], ocean_wise_palette["Coral"]), c("2023", "2025")),
                     type = 'scatter',
                     mode = 'lines',
                     line = list(width = 3)) %>%
  plotly::layout(
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      ticktext = month.abb[4:12],
      tickvals = 4:12,
      title = "",
      tickfont = list(size = 14, family = "Arial, sans-serif", weight = 700)
    ),
    yaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      title = list(text = "Total Sightings", standoff = 15),
      tickformat = ","
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p3)
htmlwidgets::saveWidget(p3, "/mnt/user-data/outputs/sightings_per_month.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p3, "/mnt/user-data/outputs/sightings_per_month.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})

####~~~~~~~~~~~~~~~~~~~~~~4. Notifications by Context (Zone vs Proximity)~~~~~~~~~~~~~~~~~~~~~~~####

## Notifications by context
notifications_by_context = main_comparison %>%
  dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(alert_year, context) %>%
  dplyr::summarise(
    total_notifications = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(context = tidyr::replace_na(context, "Unknown")) %>% 
  dplyr::mutate(context = 
                  dplyr::case_when(
                    context == "current_location" ~ "Proximity",
                    context == "preferred_area" ~ "Zone of Interest"
                  ))

print("=== Notifications by Context ===")
print(notifications_by_context)

## Visualization: Notifications by Context
p4 = plotly::plot_ly(notifications_by_context,
                     x = ~factor(alert_year),
                     y = ~total_notifications,
                     color = ~context,
                     colors = setNames(
                       c(ocean_wise_palette["Coral"], ocean_wise_palette["Ocean"], ocean_wise_palette["Dolphin"]),
                       c("Proximity", "Zone of Interest", "Unknown")
                     ),
                     type = 'bar') %>%
  plotly::layout(
    barmode = 'group',
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(size = 14, weight = 600),
      title = "",
      tickfont = list(size = 14, family = "Arial, sans-serif", weight = 700)
    ),
    yaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      tickformat = ",",
      title = list(text = "Total Notifications", standoff = 15)
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )
print(p4)
htmlwidgets::saveWidget(p4, "/mnt/user-data/outputs/notifications_by_context.html", selfcontained = TRUE)
htmlwidgets::saveWidget(p4, "/mnt/user-data/outputs/notifications_by_context.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p4, "/mnt/user-data/outputs/notifications_by_context.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})

####~~~~~~~~~~~~~~~~~~~~~~5. Total Notifications Sent per Month~~~~~~~~~~~~~~~~~~~~~~~####

## Notifications per month
notifications_per_month = alerts_main %>%
  dplyr::filter(
    alert_year %in% comparison_years,
    alert_month %in% comparison_months
  ) %>% 
  # dplyr::filter(delivery_successful == TRUE) %>%
  dplyr::group_by(alert_year, alert_month) %>%
  dplyr::summarise(
    total_notifications = dplyr::n(),
    .groups = "drop"
  )

## Calculate averages per year
notifications_avg_per_year = notifications_per_month %>%
  dplyr::group_by(alert_year) %>%
  dplyr::summarise(
    total_notifications = sum(total_notifications),
    avg_notifications_per_month = mean(total_notifications),
    .groups = "drop"
  )

print("=== Notifications Sent ===")
print(notifications_avg_per_year)

## Visualization: Notifications per Month
p5 = plotly::plot_ly(notifications_per_month,
                     x = ~alert_month,
                     y = ~total_notifications,
                     color = ~factor(alert_year),
                     colors = setNames(c(ocean_wise_palette["Ocean"], ocean_wise_palette["Coral"]), c("2023", "2025")),
                     type = 'scatter',
                     mode = 'lines',
                     line = list(width = 3)) %>%
  plotly::layout(
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        ticktext = month.abb[4:12],
        tickvals = 4:12,
        title = "",
        tickfont = list(size = 14, family = "Arial, sans-serif", weight = 700)
      ),
    yaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      title = list(text = "Total Notifications", standoff = 15),
      tickformat = ","
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p5)
htmlwidgets::saveWidget(p5, "/mnt/user-data/outputs/notifications_per_month.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p5, "/mnt/user-data/outputs/notifications_per_month.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})

####~~~~~~~~~~~~~~~~~~~~~~6. Unique Sighters Submitting per Month~~~~~~~~~~~~~~~~~~~~~~~####

## Unique observers per month
observers_per_month = sightings_comparison %>%
  dplyr::filter(!is.na(observer_email)) %>%
  dplyr::group_by(sighting_year, sighting_month) %>%
  dplyr::summarise(
    unique_observers = dplyr::n_distinct(observer_email),
    .groups = "drop"
  )

## Calculate averages per year
observers_avg_per_year = observers_per_month %>%
  dplyr::group_by(sighting_year) %>%
  dplyr::summarise(
    total_unique_observers = sum(unique_observers),
    avg_observers_per_month = mean(unique_observers),
    .groups = "drop"
  )

print("=== Unique Sighters ===")
print(observers_avg_per_year)

## Visualization: Unique Observers per Month
p6 = plotly::plot_ly(observers_per_month,
                     x = ~sighting_month,
                     y = ~unique_observers,
                     color = ~factor(sighting_year),
                     colors = setNames(c(ocean_wise_palette["Ocean"], ocean_wise_palette["Coral"]), c("2023", "2025")),
                     type = 'bar') %>%
  plotly::layout(
    barmode = 'group',
    xaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      ticktext = month.abb[4:12],
      tickvals = 4:12,
      title = "",
      tickfont = list(size = 14, family = "Arial, sans-serif", weight = 700)
    ),
    yaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      title = list(text = "Total Sightings", standoff = 15),
      tickformat = ","
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p6)
htmlwidgets::saveWidget(p6, "/mnt/user-data/outputs/unique_observers_per_month.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p6, "/mnt/user-data/outputs/unique_observers_per_month.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})

####~~~~~~~~~~~~~~~~~~~~~~7. Breakdown by Source Entity per Month~~~~~~~~~~~~~~~~~~~~~~~####

## Sightings by source per month using CONDENSED source names
source_breakdown = sightings_comparison %>%
  dplyr::mutate(
    ## Use condensed source if available, otherwise fall back to report_source_entity
    source_display = dplyr::if_else(
      !is.na(report_source_condensed),
      report_source_condensed,
      tidyr::replace_na(report_source_entity, "Unknown")
    )
  ) %>%
  dplyr::group_by(sighting_year, sighting_month, source_display) %>%
  dplyr::summarise(
    total_sightings = dplyr::n(),
    .groups = "drop"
  )

## Total by source per year
source_totals = source_breakdown %>%
  dplyr::group_by(sighting_year, source_display) %>%
  dplyr::summarise(
    total_sightings = sum(total_sightings),
    .groups = "drop"
  )

print("=== Sightings by Condensed Source Entity ===")
print(source_totals)

## Visualization: Stacked bar chart by source
## Create separate plots for each year - dynamically handle any number of comparison years
plots_list = list()

for (year in sort(comparison_years)) {
  source_year = source_breakdown %>% dplyr::filter(sighting_year == year)

  ## Get unique sources and assign colors
  unique_sources = unique(source_breakdown$source_display)
  source_colors = setNames(get_ocean_wise_colors(length(unique_sources)), unique_sources)

  plots_list[[as.character(year)]] = plotly::plot_ly(source_year,
                            x = ~sighting_month,
                            y = ~total_sightings,
                            color = ~source_display,
                            colors = source_colors,
                            type = 'bar',
                            name = ~source_display) %>%
    plotly::layout(
      barmode = 'stack',
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = TRUE,
        gridcolor = 'lightgray',
        zeroline = FALSE
      ),
      plot_bgcolor = 'white',
      paper_bgcolor = 'white',
      showlegend = TRUE
    )
}

## Combine all plots (handles 2-5 years dynamically)
p7 = plotly::subplot(plots_list, nrows = length(comparison_years), shareX = TRUE, titleY = TRUE)

print(p7)
htmlwidgets::saveWidget(p7, "/mnt/user-data/outputs/source_breakdown_per_month.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p7, "/mnt/user-data/outputs/source_breakdown_per_month.png", width = 1200, height = max(800, 400 * length(comparison_years)))
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})
####~~~~~~~~~~~~~~~~~~~~~~8. Email vs SMS by Context (2025)~~~~~~~~~~~~~~~~~~~~~~~####

## Prepare data: delivery method by context for 2025
delivery_by_context_2025 = main_comparison %>%
  dplyr::filter(
    delivery_successful == TRUE,
    alert_year == 2025,
    context %in% c("current_location", "preferred_area"),
    alert_type_name %in% c("email", "sms")
  ) %>%
  dplyr::group_by(context, alert_type_name) %>%
  dplyr::summarise(
    total_notifications = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    context = dplyr::case_when(
      context == "current_location" ~ "Proximity",
      context == "preferred_area" ~ "Zone of interest",
      TRUE ~ context
    )
  )

print("=== Email vs SMS by Context (2025) ===")
print(delivery_by_context_2025)

## Visualization: Email vs SMS by Context
p8 = plotly::plot_ly(delivery_by_context_2025,
                     y = ~context,
                     x = ~total_notifications,
                     color = ~alert_type_name,
                     colors = setNames(
                       c(ocean_wise_palette["Anemone"], ocean_wise_palette["Kelp"]),
                       c("email", "sms")
                     ),
                     type = 'bar') %>%
  plotly::layout(
    barmode = 'group',
    yaxis = list(
      showgrid = FALSE,
      zeroline = FALSE,
      tickfont = list(size = 14, weight = 600),
      title = ""
    ),
    xaxis = list(
      showgrid = TRUE,
      gridcolor = 'lightgray',
      zeroline = FALSE,
      tickformat = ",",
      title = "Total Notifications"
    ),
    plot_bgcolor = 'white',
    paper_bgcolor = 'white',
    showlegend = TRUE,
    legend = list(orientation = 'h', y = -0.1)
  )

print(p8)
htmlwidgets::saveWidget(p8, "/mnt/user-data/outputs/email_sms_by_context_2025.html", selfcontained = TRUE)
tryCatch({
  plotly::orca(p8, "/mnt/user-data/outputs/email_sms_by_context_2025.png", width = 1000, height = 600)
}, error = function(e) {
  cat("Note: PNG export requires plotly orca. HTML file saved instead.\n")
})


####~~~~~~~~~~~~~~~~~~~~~~Summary Metrics Table~~~~~~~~~~~~~~~~~~~~~~~####

## Create a comprehensive summary table
summary_metrics = data.frame(
  Metric = c(
    "Total Sightings",
    "Avg Sightings/Month",
    "Total Notifications",
    "Avg Notifications/Month",
    "Unique Recipients",
    "Unique Observers",
    "Avg Observers/Month"
  ),
  Year_2023 = c(
    sightings_avg_per_year$total_sightings[1],
    round(sightings_avg_per_year$avg_sightings_per_month[1], 1),
    notifications_avg_per_year$total_notifications[1],
    round(notifications_avg_per_year$avg_notifications_per_month[1], 0),
    users_per_year$total_unique_users[1],
    sum(observers_per_month$unique_observers[observers_per_month$sighting_year == 2023]),
    round(observers_avg_per_year$avg_observers_per_month[1], 1)
  ),
  Year_2025 = c(
    sightings_avg_per_year$total_sightings[2],
    round(sightings_avg_per_year$avg_sightings_per_month[2], 1),
    notifications_avg_per_year$total_notifications[2],
    round(notifications_avg_per_year$avg_notifications_per_month[2], 0),
    users_per_year$total_unique_users[2],
    sum(observers_per_month$unique_observers[observers_per_month$sighting_year == 2025]),
    round(observers_avg_per_year$avg_observers_per_month[2], 1)
  )
)

## Calculate percent change
summary_metrics$Percent_Change = round(
  ((summary_metrics$Year_2025 - summary_metrics$Year_2023) / summary_metrics$Year_2023) * 100,
  1
)

print("=== SUMMARY METRICS (April - December) ===")
print(summary_metrics)

## Save summary table
write.csv(summary_metrics, "/mnt/user-data/outputs/summary_metrics.csv", row.names = FALSE)

####~~~~~~~~~~~~~~~~~~~~~~Export All Metric Tables~~~~~~~~~~~~~~~~~~~~~~~####

## Save all metric tables for reference
write.csv(users_per_month, "/mnt/user-data/outputs/users_per_month.csv", row.names = FALSE)
write.csv(sightings_per_month, "/mnt/user-data/outputs/sightings_per_month.csv", row.names = FALSE)
write.csv(notifications_per_month, "/mnt/user-data/outputs/notifications_per_month.csv", row.names = FALSE)
write.csv(observers_per_month, "/mnt/user-data/outputs/observers_per_month.csv", row.names = FALSE)
write.csv(source_breakdown, "/mnt/user-data/outputs/source_breakdown.csv", row.names = FALSE)
write.csv(notifications_by_context, "/mnt/user-data/outputs/notifications_by_context.csv", row.names = FALSE)

cat("\n=== Analysis Complete ===\n")
cat("All visualizations saved to /mnt/user-data/outputs/\n")
cat("All metric tables saved as CSV files\n")
cat("========================\n")
