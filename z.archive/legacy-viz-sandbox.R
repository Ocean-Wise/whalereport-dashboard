####~~~~~~~~~~~~~~~~~~~~~~Monthly Dashboard Metrics Visualization~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To visualize production of metrics for the ELT dashboard. Should be able to be run by anyone in the team.
## Date written: 2024-01-19
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## WILL REQUIRE DYLAN OR ALEX TO PULL DATA FROM THE DATABASE FIRST AND STORE IN SP. SEE FILE PATHS FOR INFO IN DATA LOADING.
## For this script to work you should...
##      1. Link up to the GitHub repo and pull the project. 
##      2. Open the project

## What data views do we want to see?

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

# update this for the start date of period
start_date = as.Date("2025-01-01")

# update this for the end date of period
end_date = as.Date("2025-06-30")


## Year on year growth of detection and alerts

sightings_yearly = detections %>% 
  dplyr::select(1:8) %>%
  dplyr::group_by(year_mon) %>% 
  tidyr::pivot_longer(!c(year,year_mon), names_to = "source", values_to = "count") %>% 
  dplyr::summarise(detection_count = sum(count)) %>% 
  dplyr::filter(year_mon != zoo::as.yearmon(Sys.Date()))


alerts_yearly = overall_alerts %>% 
  dplyr::select(1:9) %>%
  dplyr::group_by(year_mon = zoo::as.yearmon(date)) %>% 
  dplyr::select(-c(year,month,date)) %>% 
  tidyr::pivot_longer(!year_mon, names_to = "source", values_to = "count") %>% 
  dplyr::summarise(alert_count = sum(count)) %>% 
  dplyr::filter(year_mon != zoo::as.yearmon(Sys.Date()))
  
wras_growth = sightings_yearly %>% 
  dplyr::left_join(alerts_yearly) %>% 
  tidyr::pivot_longer(cols = c(detection_count, alert_count),
                      names_to = "type",
                      values_to = "count") %>% 
  ggplot2::ggplot(ggplot2::aes(x = year_mon, y = count, color = type)) +
  ggplot2::geom_smooth(method = "loess", span = 0.2, se = FALSE, size = 1) +
  # geom_line(linewidth = 1, linejoin = "round") +
  ggplot2::scale_color_manual(values = c("#A8007E", "#AAAC24")) +
  zoo::scale_x_yearmon(format = "%b %Y", n = 10) + # Format yearmon axis
  # scale_y_continuous(labels = comma) +
  ggplot2::labs(
    title = "",
    x = "",
    y = ""
  ) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    panel.grid.major.x = ggplot2::element_blank(), # Remove major vertical grid lines
    panel.grid.minor.x = ggplot2::element_blank(),  # Remove minor vertical grid lines
    axis.text.x = ggplot2::element_text(size = 14), # Increase x-axis text size
    axis.text.y = ggplot2::element_text(size = 14), # Increase y-axis text size
    axis.title.x = ggplot2::element_text(size = 16), # Increase x-axis title size
    axis.title.y = ggplot2::element_text(size = 16)  # Increase y-axis title size
    )
  
  





#### ~~~~~~~~~~~~~~~~~~~~~~~ Sighting proportions ~~~~~~~~~~~~~~~~~~~~####

## All real-time sightings, not just sightings that led to alerts

sights_props = detections %>%
  dplyr::select(-year) %>% 
  tidyr::pivot_longer(
    !year_mon,
    names_to = "source",
    values_to = "count"
  ) %>% 
  # dplyr::filter(year_mon != "Aug 2024", source != "Whale Alert Alaska") %>% 
  dplyr::group_by(year = lubridate::year(year_mon), source) %>% 
  dplyr::summarise(count = sum(count)) 
  # dplyr::filter(year == 2024)
  # update the year to get different years, or remove to get a timeline
  # remove whale alert alaska when we get more data from them 
  
  

## Drop this into Excel and make a unit chart from the data
sights_species = alerts_detections %>% 
  dplyr::mutate(species_id = dplyr::case_when(
    species_id == 1 ~ "Killer whale",
    species_id == 2 ~ "Humpback whale",
    species_id == 3 ~ "Grey whale",
    species_id == 4 ~ "Minke whale",
    species_id == 5 ~ "Fin whale",
    species_id == 6 ~ "Sperm whale",
    species_id == 7 ~ "Blue whale",
    species_id == 8 ~ "Sei whale", #"Rare whale species"
    species_id == 9 ~ "North Pacific Right whale", #"Rare whale species"
    species_id == 10 ~ "Bairds Beaked whale", #"Rare whale species"
    species_id == 11 ~ "Cuviers Beaked whale", #"Rare whale species"
    species_id == 12 ~ "Other Rare whale", #"Rare whale species"
    species_id == 13 ~ "Unidentified whale",
    species_id == 14 ~ "Killer whale",
    species_id == 15 ~ "Harbour porpoise", #"Porpoise/dolphin species"
    species_id == 16 ~ "Dalls porpoise", #"Porpoise/dolphin species"
    species_id == 17 ~ "Pacific White-sided dolphin", #"Porpoise/dolphin species"
    species_id == 18 ~ "Rissos dolphin", #"Porpoise/dolphin species"
    species_id == 19 ~ "Northern Right Whale dolphin", #"Porpoise/dolphin species"
    species_id == 20 ~ "False Killer whale", #"Porpoise/dolphin species"
    species_id == 21 ~ "Common dolphin", #"Porpoise/dolphin species"
    species_id == 22 ~ "Unidentified dolphin / porpoise", 
    species_id == 23 ~ "Leatherback sea turtle", # "Turtle"
    species_id == 24 ~ "Green sea turtle", # "Turtle"
    species_id == 25 ~ "Olive ridley sea turtle", # "Turtle"
    species_id == 26 ~ "Loggerhead sea turtle", # "Turtle"
    species_id == 27 ~ "unidentified sea turtle" # "Turtle"
  )) %>% 
  dplyr::group_by(year = lubridate::year(sighted_at), month = lubridate::month(sighted_at), species_id) %>% 
  dplyr::filter(dplyr::between(sighted_at, start_date, end_date)) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  # dplyr::filter(year == 2024 & month < 8) %>%
  dplyr::ungroup() %>% 
  dplyr::group_by(month) %>%
  dplyr::arrange(desc(count), .by_group = TRUE) %>% 
  dplyr::mutate(date = as.Date(paste0(year,"-", month, "-01"), format = "%Y-%m-%d")) %>% 
  dplyr::mutate(species = factor(species_id, levels = species_id[order(-count)]))

total_species = sights_species %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(species_id) %>% 
  dplyr::summarise(count = sum(count))
  


#### ~~~~~~~~~~~~~~~~~~~~~~~ Species detections proportions ~~~~~~~~~~~~~~~~~~~~####


plotly::plot_ly(sights_species, 
        x = ~date, 
        y = ~count, 
        color = ~species, 
        type = 'bar') %>% 
        # text = ~paste(Species, ": ", Count),
        # hoverinfo = 'text',
        # marker = list(line = list(color = 'rgba(0,0,0,1)', width = 1.5))) %>%
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickformat = "%b %Y",
                              # tickvals = ~year_mon,
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = list(text='No. Alerts', font = list(size = 16, family = 'Arial Black'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2                  # move legend below the plot
                 ),
                 barmode = "stack")

#### ~~~~~~~~~~~~~~~~~~~ NIGHT VS DAY ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
## Number of alerts overall
## Edit the filter depending on what data source you want. Although may need to be edited to make it look a little nicer when using a longer timeframe... 

## Calculate sunrise and sunset for each date in your dataset
x = detections_clean %>%
  dplyr::ungroup() %>% 
  dplyr::filter(as.Date(sighted_at) >= as.Date("2025/01/01") & as.Date(sighted_at) <= as.Date("2025/07/31")) %>%
  dplyr::filter(stringr::str_detect(source_entity, "WhaleSpotter") & org_name == "Quiet Sound") %>%  
# & stringr::str_detect(org_name, "Quiet Sound")) %>% 
  dplyr::mutate(
    sun_times = suncalc::getSunlightTimes(as.Date(sighted_at),
                                          lat = 48.51566, 
                                          lon = -123.1528)) %>% 
  dplyr::mutate(sunrise = lubridate::ymd_hms(.$sun_times$sunrise, tz = "UTC") - lubridate::hours(7),
                sunset = lubridate::ymd_hms(.$sun_times$sunset, tz = "UTC") - lubridate::hours(7)) %>% 
  dplyr::select(id, sighted_at, latitude, longitude, 
                species, source_entity, sunrise, sunset) %>% 
  dplyr::mutate(day_night = 
                  dplyr::case_when(dplyr::between(sighted_at, sunrise, sunset)  ~ "day",
                                   TRUE  ~ "night"))

openxlsx::write.xlsx(x,paste0("C:/Users/", user, "/Downloads/whalespotter-quietsound", Sys.Date(),".xlsx"))

x  %>% 
  dplyr::group_by(month = lubridate::month(sighted_at),day_night) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = count
  ) %>% 
  dplyr::mutate(month_name = factor(month.name[month], levels = month.name)) %>% 
  plotly::plot_ly(
    x = ~month_name,
    y = ~day,
    type = "bar",
    name = "Daylight hours",
    marker = list(color = "#F2B949") 
  ) %>% 
  plotly::add_trace(
    y = ~night,
    name = "Outside daylight\n hours",
    marker = list(color = "#6449F2")
  ) %>% 
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = list(text='No. detections', font = list(size = 16, family = 'Arial'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 barmode = "stack")
  


vfpa_data = joined_tables %>% dplyr::filter(source_entity == "SMRUC") %>% 
  dplyr::mutate(day_night = 
                   dplyr::case_when(
                     dplyr::between(lubridate::date(sent_at), as.Date(2024-02-01),
                       dplyr::between(lubridate::hour(sent_at), 6, 20) ~ "day",
                     dplyr::between(lubridate::hour(sent_at), 21, 5) ~ "night")
      # lubridate::hour(sent_at) >= 6 & lubridate::hour(sent_at) <= 21 ~ "day",
      # lubridate::hour(sent_at) <= 5 & lubridate::hour(sent_at) >= 22 ~ "night"
      )) %>% 
  dplyr::mutate(day_night = tidyr::replace_na(day_night, "night")) %>% 
  dplyr::select(
    c(id, tracking_id, sighting_id, created_at, sent_at, sighted_at, day_night)
    ) %>% 
  # dplyr::group_by(day_night) %>% 
  # dplyr::summarise(n = dplyr::n())
  dplyr::group_by(date = lubridate::date(sent_at),day_night) %>% 
  dplyr::summarise(count = dplyr::n()) %>% 
  tidyr::pivot_wider(
    names_from = day_night,
    values_from = count
  ) %>% 
  plotly::plot_ly(
    x = ~date,
    y = ~day,
    type = "bar",
    name = "Daylight hours",
    marker = list(color = "#F2B949") 
  ) %>% 
  plotly::add_trace(
    y = ~night,
    name = "Outside daylight\n hours",
    marker = list(color = "#6449F2")
  ) %>% 
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = list(text='No. Alerts', font = list(size = 16, family = 'Arial Black'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 barmode = "stack")
  
  




#### ~~~~~~~~~~~ Alerts ~~~~~~~~~~~ ####

alert_prop = overall_alerts %>% 
  dplyr::filter(dplyr::between(date, start_date, end_date)) %>% 
  dplyr::select(1:9) %>% 
  dplyr::ungroup() %>% 
  tidyr::pivot_longer(cols = `Ocean Wise`:`Whale Alert`,
               names_to = "source",
               values_to = "count") %>% 
  dplyr::group_by(source) %>%
  dplyr::summarise(total_count = sum(count)) %>%
  dplyr::mutate(proportion = total_count / sum(total_count))


library(ggplot2)

ggplot(alert_prop, aes(x = proportion, y = 1, fill = source)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = NULL, y = "Proportion", fill = "Source",
       title = "Proportion of Data from Each Source") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), # Remove x-axis text
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        axis.title.x = element_blank()) # Remove x-axis title


###
alert_bar = overall_alerts %>% 
  # dplyr::filter(year == 2024) %>% 
  dplyr::ungroup() %>% 
  janitor::clean_names() %>% 
  dplyr::select(c(date, dplyr::contains("cumulative"))) %>%
  dplyr::mutate(hydrophone = cumulative_jasco + cumulative_smru,
                partner_networks = cumulative_whale_alert + cumulative_orca_network,
                infrared = cumulative_whale_spotter) %>%
  dplyr::select(c(date, 
                  `Whale Report` = cumulative_ocean_wise, 
                  Hydrophone = hydrophone, 
                  `Partner Networks` = partner_networks, 
                  Infrared = infrared))
  
alert_bar %>% 
  tidyr::pivot_longer(
    cols = -date,
    names_to = "catagory",
    values_to = "vals"
  ) %>% 
  plotly::plot_ly(., 
                  x = ~date, 
                  y = ~vals, 
                  color = ~catagory, 
                  type = 'bar') %>% 
  # text = ~paste(Species, ": ", Count),
  # hoverinfo = 'text',
  # marker = list(line = list(color = 'rgba(0,0,0,1)', width = 1.5))) %>%
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              autotick = T,
                              ticks = 'outside',
                              tickformat = "%b %Y",
                              # tickvals = ~year_mon,
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = list(text='No. Alerts', font = list(size = 16, family = 'Arial Black'), standoff = 25),
                              showgrid = FALSE,
                              zeroline = FALSE,
                              showline = FALSE,
                              showticklabels = T,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.1                  # move legend below the plot
                 ),
                 barmode = "stack")



### Hydrophone alerts
detections %>% 
  dplyr::ungroup() %>% 
  dplyr::select(c(year_mon, sightigns_jasco = JASCO, sightings_smru = SMRU)) %>% 
  dplyr::mutate(cum_sightings_smru = cumsum(sightings_smru),
                cum_sightings_jasco = cumsum(sightigns_jasco),
                date = lubridate::as_date(year_mon)) %>%
  dplyr::right_join(overall_alerts %>% 
                      dplyr::filter(year == 2024), by = dplyr::join_by(date)) %>% 
  dplyr::select(date, 
                sightings_smru, sightigns_jasco, cum_sightings_smru, cum_sightings_jasco,
                alert_jasco = JASCO, alert_smru = SMRU, cum_alert_jasco = `Cumulative JASCO`, cum_alert_smru =`Cumulative SMRU`) %>% 
  plotly::plot_ly(x = ~date) %>%
  plotly::add_lines(y = ~cum_sightings_smru, name = 'Detections Lime Kiln', line = list(color = '#2ca02c')) %>%
  plotly::add_lines(y = ~cum_sightings_jasco, name = 'Detections Boundary Pass', line = list(color = '#ff7f0e')) %>%
  plotly::add_lines(y = ~cum_alert_smru, name = 'Alerts Lime Kiln', line = list(color = '#2ca02c', dash = 'dash')) %>%
  plotly::add_lines(y = ~cum_alert_jasco, name = 'Alerts Boundary Pass', line = list(color = '#ff7f0e', dash = 'dash')) %>%
  plotly::layout(title = "",
                 xaxis = list(title = ""),
                 yaxis = list(title = ""),
                 shapes = list(
                   list(
                     type = "line",
                     x0 = as.Date("2024-07-15"),
                     x1 = as.Date("2024-07-15"),
                     y0 = 0,
                     y1 = 1,
                     xref = "x",
                     yref = "paper",
                     line = list(color = "darkgrey", dash = "dash")
                   ),
                   list(
                     type = "line",
                     x0 = as.Date("2024-12-01"),
                     x1 = as.Date("2024-12-01"),
                     y0 = 0,
                     y1 = 1,
                     xref = "x",
                     yref = "paper",
                     line = list(color = "darkgrey", dash = "dash")
                   )
                 ),
                 annotations = list(
                   list(
                     x = as.Date("2024-06-28"),
                     y = 1,
                     xref = "x",
                     yref = "paper",
                     text = "OW database\noutage ~3 days",
                     showarrow = FALSE,
                     font = list(size = 12, color = "black"),
                     align = "center"
                   ),
                   list(
                     x = as.Date("2024-11-15"),
                     y = 1,
                     xref = "x",
                     yref = "paper",
                     text = "OW API down for\n Boundary Pass\n ~9 days",
                     showarrow = FALSE,
                     font = list(size = 12, color = "black"),
                     align = "center"
                   )
                 )
  )

## Line graph of alerts

overall_alerts %>% 
  dplyr::filter(dplyr::between(month, 1,9) & year == 2024)  %>%
  plotly::plot_ly(
    y = ~`Cumulative Ocean Wise`,
    x = ~date,
    name = "WhaleReport",
    type = "scatter",
    mode = "lines",
    line = list(color = "#1f77b4"),  # Blue for WhaleReport
    fill = 'tonexty',
    fillcolor = '#1f77b42'# Transparent blue for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative SMRU`,
    name = "Lime Kiln",
    type = "scatter",
    mode = "lines",
    line = list(color = "#2ca02c"),  # Green for SMRU
    fill = 'tonexty',
    fillcolor = 'rgba(44, 160, 44, 0.3)' # Transparent green for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative JASCO`,
    name = "Boundary Pass",
    type = "scatter",
    mode = "lines",
    line = list(color = "#ff7f0e"),  # Orange for JASCO
    fill = 'tonexty',
    fillcolor = 'rgba(255, 127, 14, 0.3)' # Transparent orange for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative Orca Network`,
    name = "Partner Sightings Network",
    type = "scatter",
    mode = "lines",
    line = list(color = "#d62728"),  # Red for Partner Sightings Network
    fill = 'tonexty',
    fillcolor = 'rgba(214, 39, 40, 0.3)' # Transparent red for fill
  ) %>%
  plotly::add_trace(
    y = ~`Cumulative WhaleSpotter`,
    name = "IR Camera",
    type = "scatter",
    mode = "lines",
    line = list(color = "#9467bd"),  # Purple for IR Camera
    fill = 'tonexty',
    fillcolor = 'rgba(148, 103, 189, 0.3)' # Transparent purple for fill
  ) %>%
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # tickformat="%b %Y",
                              ticktext = format("%b %Y"),
                              dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the centerS
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2                  # move legend below the plot
                 ))


#### ~~~~~~~~~~~ USER GROWTH ~~~~~~~~~~~ ####

### IMPORT DATA ###

users_overall = readxl::read_xlsx(
  "C:/Users/AlexMitchell/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Whale Report Alert System/Participants/WRASUSERS_main.xlsx",
  sheet = "Authorized"
) %>% 
  janitor::clean_names()
  dplyr::mutate(approval_date = janitor::excel_numeric_to_date(as.numeric(approval_date)))
  
users_cumulative = users_overall %>% 
  # dplyr::filter(region_clean == "USA") %>%
  dplyr::group_by(year_qtr = zoo::as.yearqtr(approval_date), org_type) %>%
  dplyr::summarise(count = dplyr::n()) %>% 
  dplyr::group_by(org_type) %>% 
  dplyr::mutate(cum_count = cumsum(count)) %>% 
  dplyr::select(-count) %>% 
  tidyr::pivot_wider(names_from = org_type,
                     values_from = cum_count) %>% 
  dplyr::filter(!is.na(year_qtr))

dates = seq(from = floor(min(users_cumulative$year_qtr)),
            to = max(users_cumulative$year_qtr),
            by = 1/4) %>% 
  tibble::as_tibble(.)

users_cumulative = dates %>% 
  dplyr::left_join(users_cumulative, by = dplyr::join_by(value == year_qtr)) %>% 
  tidyr::fill(`Marine Pilots`, Ferries, 
              Enforcement, Government, 
              Industry, Indigenous,
              `Port Authorities`, Research,
              `Tug and Tow`, .direction = "down") %>% 
  dplyr::rename(year_qtr = value) %>% 
  dplyr::mutate(dplyr::across(.cols = 2:11, ~tidyr::replace_na(.x,0))) 
  
users_cumulative_total = 
  users_cumulative %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(total = sum(dplyr::across(2:11))) %>% 
  dplyr::select(year_qtr, total)



### PLOT ###



### Total by orgs


plotly::plot_ly(users_cumulative, x = ~year_qtr) %>%
  plotly::add_trace(y = ~`Marine Pilots`, name = "Marine Pilots", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Ferries, name = "Ferries", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Enforcement, name = "Enforcement", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Government, name = "Government", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Indigenous, name = "Indigenous", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Industry, name = "Industry", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~`Port Authorities`, name = "Port Authorities", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Research, name = "Research", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~`Tug and Tow`, name = "Tug and Tow", type = 'scatter', mode = 'lines') %>%
  plotly::add_trace(y = ~Developer, name = "Developer", type = 'scatter', mode = 'lines') %>% 
  plotly::layout(xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # tickformat="%b %Y",
                              # ticktext = format("%b %Y"),
                              # dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 legend = list(
                   orientation = "h",        # horizontal legend
                   xanchor = "center",       # anchor legend at the center
                   x = 0.5,                  # set position to the center (horizontally)
                   y = -0.2,                  # move legend below the plot
                   tickfont = list(family = 'Arial',
                                   size = 18,
                                   color = 'rgb(82, 82, 82)')))



### Stacked bar chart 

users_cumulative %>% 
  tidyr::pivot_longer(cols = -year_qtr, 
                      names_to = "catagory",
                      values_to = "value") %>% 
  plotly::plot_ly(x = ~year_qtr,
                  y = ~value,
                  color = ~catagory,
                  type = "bar") %>% 
  plotly::layout(barmode = 'stack',
                 xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # tickformat="%b %Y",
                              # ticktext = format("%b %Y"),
                              # dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                   tickfont = list(family = 'Arial',
                                   size = 18,
                                   color = 'rgb(82, 82, 82)'))

  


 ####~~~~~~~~~~~~~~~~~~~~~~~~~~~ Mapping ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~####

 ## Sightings
sight_map = sightings_clean %>%
  dplyr::mutate(species = 
                  stringr::str_to_sentence(species)) %>% 
 # dplyr::filter(lubridate::year(date_time) == 2024) %>%  
               # & dplyr::between(lubridate::month(date), 1,10 )) %>%
 dplyr::mutate(species = 
                 dplyr::case_when(
                   stringr::str_detect(species, "dolphin") ~ "Dolphin/Porpoise species",
                   stringr::str_detect(species, "porpoise") ~ "Dolphin/Porpoise species",
                   stringr::str_detect(species, "turtle") ~ "Potential turtle species",
                   stringr::str_detect(species, "False") ~ "Dolphin/Porpoise species",
                   stringr::str_detect(species, "Sei") ~ "Unidentified whale",
                   stringr::str_detect(species, "North pacific right whale") ~ "Other rare species",
                   stringr::str_detect(species, "Blue") ~ "Other rare species",
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
                  species == "Sperm whale" ~ "blue",
                  species == "Unidentified whale" ~ "#B7950B",
                  species == "Potential turtle species" ~ "darkgreen" 
                )) %>%
dplyr::mutate(
  popup_content = ifelse(
    !is.na(ecotype),
    paste("<b>Species:</b> ", species, "<b><br>Ecotype:</b> ", ecotype, "<b><br>Date:</b>", as.Date(date_time)),
    paste("<b>Species:</b> ", species, "<b><br>Date:</b> ", as.Date(date_time))
  )
) 

sight_map %>% 
 leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 3,
    group = ~species,
    color = ~col_palette,
    fillOpacity = 0.8,
    opacity = 0.8,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(sight_map$col_palette)),
    labels = c(unique(sight_map$species)),
    opacity = 0.8
   ) %>%
  leaflet::addMiniMap(toggleDisplay = TRUE)
  # htmltools::save_html(., paste0("C:/Users/", 
  #                                user, 
  #                                "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
  #                                "sightings-map-",
  #                                Sys.Date(),
  #                                ".html"))

## Alerts

alert_map = alerts_detections %>%
  # dplyr::filter(lubridate::year(sent_at) == 2024) %>%
  dplyr::filter(dplyr::between(sent_at, as.Date("2024-04-01"), as.Date("2025-03-31"))) %>% 
  dplyr::mutate(col_palette =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A569BD",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "#27AE60",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#F5B041",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "#17202A",
                    stringr::str_detect(source_entity, "SMRU") == T ~ "#17202A",
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "#2b547e"
                  )) %>%
  dplyr::mutate(detection_method =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "Infrared camera",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Whale report app",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "Hydrophone",
                    stringr::str_detect(source_entity, "SMRU") == T ~ "Hydrophone",
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "Whale Alert"
                  )) %>%
  dplyr::mutate(
    popup_content =
      paste("<b>Species:</b> ", species,
            "<b><br>Source:</b> ", source_entity,
            "<b><br>Detection method:</b>", detection_method,
            "<b><br>Date:</b>", as.Date(sent_at)
            ))


alert_map %>%
  # dplyr::filter(detection_method == "Orca Network" | detection_method == "Whale Alert") %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles(urlTemplate = "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png") %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 1,
    color = ~col_palette,
    fillOpacity = 0.6,
    opacity = 0.6,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(alert_map$col_palette)),
    labels = c(unique(alert_map$detection_method)),
    opacity = 0.8) %>% 
  leaflet::addMiniMap(toggleDisplay = TRUE) %>%
  leaflet::leafletOptions(zoomSnap = 0.1,  # Change zoom steps to finer intervals (0.5)
                 zoomDelta = 0.1) %>%   # Change zoom increments to 0.5) %>%
htmltools::save_html(., paste0("C:/Users/",
                               user,
                               "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
                               "alert-map-",
                               Sys.Date(),
                               ".html"))
  

## Heat map
alert_map %>% 
  dplyr::filter(is.na(latitude) == F) %>% 
  leaflet::leaflet() %>% 
  leaflet::addProviderTiles("CartoDB.Positron") %>%
  leaflet.extras::addHeatmap(
    lng = ~longitude,
    lat = ~latitude,
    minOpacity = 0.05,
    max = 0.05,
    radius = 15,
    blur = 5) %>% 
  leaflet::addMiniMap(toggleDisplay = TRUE)
# %>% 
#   htmltools::save_html(., paste0("C:/Users/", 
#                                  user, 
#                                  "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
#                                  "heat-map-",
#                                  Sys.Date(),
#                                  ".html"))


## Detections map - not alert map 

detections_clean %>% 
  dplyr::mutate(source_entity =
                  dplyr::case_when(
                    is.na(source_entity) == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network",
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "WhaleSpotter",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "JASCO",
                    stringr::str_detect(source_entity, "SMRUC") == T ~ "SMRU",
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "Whale Alert",
                    TRUE ~ source_entity)) %>% 
  dplyr::mutate(col_palette =
                  dplyr::case_when(
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "#A569BD",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "#27AE60",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "#F5B041",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "#17202A",
                    stringr::str_detect(source_entity, "SMRU") == T ~ "#17202A",
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "#2b547e"
                  )) %>%
  # dplyr::filter(source_entity == "Orca Network" | source_entity == "Whale Alert") %>%
  dplyr::filter(source_entity != "string" & source_entity != "TEST") %>%
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 2,
    group = ~source_entity,
    color = ~col_palette,
    fillOpacity = 0.8,
    opacity = 0.8
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = ~unique(col_palette),
    labels = ~unique(source_entity),
    opacity = 0.8) %>% 
  leaflet::leafletOptions(zoomSnap = 0.1,  # Change zoom steps to finer intervals (0.5)
                          zoomDelta = 0.1)
  


## Where are the sightings vs what sightings led to alerts? 


x = detections_clean %>% 
  dplyr::left_join(joined_tables, by = dplyr::join_by(id == sighting_id)) %>% 
  dplyr::mutate(flagged = ifelse(!is.na(auth_id), TRUE, FALSE)) %>% 
  dplyr::select(c(id, flagged)) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(detections_clean) %>% 
  dplyr::filter(lubridate::year(sighted_at) == 2024) %>% 
  leaflet::leaflet(.) %>%
  leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) %>%
  leaflet::addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~ifelse(flagged, "#A8007E", "#AAAC24"),
    radius = 2
  )




######################### SANDBOX ######################################


## Map of sightings in US

us_sightings = sights_pre %>% 
  dplyr::filter(is.na(latitude) == F) %>% 
  dplyr::filter(sighted_at >= as.Date("2023-01-01")) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>% 
  sf::st_make_valid() %>% 
  sf::st_join(x = ., 
              y = US_EZZ,
              join = sf::st_within) %>% 
  dplyr::filter(GEONAME == "United States Exclusive Economic Zone") %>%
  dplyr::select(sighted_at, species,number_of_animals,source_entity)
  

us_map = us_sightings %>% 
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
                  )) %>%
  dplyr::mutate(
    popup_content =
      paste("<b>Species:</b> ", species,
            "<b><br>Source:</b> ", source_entity,
            "<b><br>Date:</b>", as.Date(sighted_at)
      )) %>% 
  leaflet::leaflet() %>%
  leaflet::addTiles() %>%
  leaflet::addCircleMarkers(
    radius = 3,
    group = ~species,
    color = ~col_palette,
    fillOpacity = 0.8,
    opacity = 0.8,
    popup = ~popup_content
  ) %>%
  leaflet::addLegend(
    "bottomright",
    colors = c(unique(sight_map$col_palette)),
    labels = c(unique(sight_map$species)),
    opacity = 0.8
  ) %>% 
  leaflet::addMiniMap(toggleDisplay = TRUE) 

## Save map
htmlwidgets::saveWidget(us_map, paste0("C:/Users/",
                                 user,
                                 "/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/visualizations/",
                                 "US-sightings-map-",
                                 Sys.Date(),
                                 ".html"),
                        selfcontained = TRUE)

write.csv()






# 
# overall_alerts %>% 
#   dplyr::ungroup() %>% 
#   dplyr::filter(year == 2024) %>% 
#   dplyr::select(date, `Cumulative JASCO`, `Cumulative SMRU`) %>% 
#   # tidyr::pivot_longer(
#   #   cols = -date,
#   #   names_to = "catagory",
#   #   values_to = "vals"
#   # ) %>% 
#   plotly::plot_ly(., 
#                   x = ~date, 
#                   y = ~`Cumulative JASCO`, 
#                   name = "Boundary Pass",
#                   type = 'scatter', 
#                   mode = 'lines',
#                   line = list(color = "A8007E")) %>%
#   plotly::add_trace(y = ~`Cumulative SMRU`,
#                     name = "Lime Kiln",
#                     type = "scatter",
#                     mode = "lines",
#                     line = list(color = "005580")) %>% 
#   plotly::layout(xaxis = list(title = "",
#                               showline = TRUE,
#                               showgrid = FALSE,
#                               showticklabels = TRUE,
#                               linecolor = 'rgb(204, 204, 204)',
#                               linewidth = 2,
#                               ticks = 'outside',
#                               tickcolor = 'rgb(204, 204, 204)',
#                               tickwidth = 2,
#                               ticklength = 5,
#                               # # tickformat="%b %Y",
#                               # ticktext = format("%b %Y"),
#                               # dtick = "M1",
#                               tickfont = list(family = 'Arial',
#                                               size = 16,
#                                               color = 'rgb(82, 82, 82)')),
#                  yaxis = list(title = "",
#                               showgrid = T,
#                               zeroline = FALSE,
#                               showline = FALSE,
#                               tickfont = list(family = 'Arial',
#                                               size = 16,
#                                               color = 'rgb(82, 82, 82)')),
#                  legend = list(
#                    orientation = "h",        # horizontal legend
#                    xanchor = "center",       # anchor legend at the center
#                    tickfont = list(family = 'Arial',
#                                    size = 16,
#                                    color = 'rgb(82, 82, 82)'),
#                    x = 0.5,                  # set position to the center (horizontally)
#                    y = -0.1                  # move legend below the plot
#                  ),


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# 
# ## Fiscal table detections
# 
# joined_tables %>% 
#   dplyr::distinct() %>% 
#   dplyr::group_by(
#     year = lubridate::year(sent_at), 
#     month = lubridate::month(sent_at),
#     source = source_entity
#   ) %>% 
#   dplyr::summarise(
#     count = dplyr::n()) %>% 
#   # dplyr::filter(
#   # year == 2023 | year == 2024) %>%
#   dplyr::mutate(date = lubridate::as_date(paste0(year,"/",month,"/01"))) %>% 
#   tidyr::pivot_wider(
#     names_from = source,
#     values_from = count
#   ) %>% 
#   dplyr::mutate(
#     dplyr::across(
#       dplyr::everything(), ~tidyr::replace_na(.x, 0))
#   ) %>% 
#   dplyr::group_by(year) %>% 
#   dplyr::mutate(
#     `Cumulative Ocean Wise` = cumsum(`Ocean Wise`),
#     `Cumulative Orca Network` = cumsum(`Orca Network`),
#     `Cumulative WhaleSpotter` = cumsum(`WhaleSpotter`),
#     `Cumulative JASCO` = cumsum(JASCO),
#     `Cumulative SMRU` = cumsum(SMRU),
#     `Cumulative Whale Alert` = cumsum(`Whale Alert`),
#     Total = cumsum(`Ocean Wise` + JASCO + `WhaleSpotter` + `Orca Network` + SMRU + `Whale Alert`)
#   ) %>% 
#   dplyr::mutate(
#     `Ocean Wise %` = (`Cumulative Ocean Wise`/Total)*100,
#     `Orca Network %` = (`Cumulative Orca Network`/Total)*100,
#     `JASCO %` = (`Cumulative JASCO`/Total)*100,
#     `WhaleSpotter %` = (`Cumulative WhaleSpotter`/Total)*100,
#     `SMRU %` = (`Cumulative SMRU`/Total)*100,
#     `Whale Alert %` = (`Cumulative Whale Alert`/Total)*100
#   ) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Fiscal table detections
# 
# joined_tables %>% 
#   dplyr::distinct() %>% 
#   dplyr::group_by(
#     year = lubridate::year(sent_at), 
#     month = lubridate::month(sent_at),
#     source = source_entity
#   ) %>% 
#   dplyr::summarise(
#     count = dplyr::n()) %>% 
#   # dplyr::filter(
#   # year == 2023 | year == 2024) %>%
#   dplyr::mutate(date = lubridate::as_date(paste0(year,"/",month,"/01"))) %>% 
#   tidyr::pivot_wider(
#     names_from = source,
#     values_from = count
#   ) %>% 
#   dplyr::mutate(
#     dplyr::across(
#       dplyr::everything(), ~tidyr::replace_na(.x, 0))
#   ) %>% 
#   dplyr::group_by(year) %>% 
#   dplyr::mutate(
#     `Cumulative Ocean Wise` = cumsum(`Ocean Wise`),
#     `Cumulative Orca Network` = cumsum(`Orca Network`),
#     `Cumulative WhaleSpotter` = cumsum(`WhaleSpotter`),
#     `Cumulative JASCO` = cumsum(JASCO),
#     `Cumulative SMRU` = cumsum(SMRU),
#     `Cumulative Whale Alert` = cumsum(`Whale Alert`),
#     Total = cumsum(`Ocean Wise` + JASCO + `WhaleSpotter` + `Orca Network` + SMRU + `Whale Alert`)
#   ) %>% 
#   dplyr::mutate(
#     `Ocean Wise %` = (`Cumulative Ocean Wise`/Total)*100,
#     `Orca Network %` = (`Cumulative Orca Network`/Total)*100,
#     `JASCO %` = (`Cumulative JASCO`/Total)*100,
#     `WhaleSpotter %` = (`Cumulative WhaleSpotter`/Total)*100,
#     `SMRU %` = (`Cumulative SMRU`/Total)*100,
#     `Whale Alert %` = (`Cumulative Whale Alert`/Total)*100
#   ) 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## Fiscal Sightings
# #total
# detections_clean %>% 
#   dplyr::filter(dplyr::between(sighted_at, as.Date("2023-04-01"), as.Date("2025-03-31"))) %>% 
#   dplyr::mutate(fiscal = 
#                   dplyr::case_when(
#                     dplyr::between(sighted_at, as.Date("2023-04-01"), as.Date("2024-03-31")) ~ "23/24",
#                     dplyr::between(sighted_at, as.Date("2024-04-01"), as.Date("2025-03-31")) ~ "24/25"
#                   )
#                 ) %>% 
#   dplyr::select(id, sighted_at, species, fiscal, source_entity) %>% 
#   dplyr::distinct() %>% 
#   dplyr::group_by(fiscal) %>% 
#   dplyr::summarise(count = dplyr::n())
# 
# #table
# detections_clean %>% 
#   dplyr::filter(dplyr::between(sighted_at, as.Date("2023-04-01"), as.Date("2025-03-31"))) %>% 
#   dplyr::mutate(fiscal = 
#                   dplyr::case_when(
#                     dplyr::between(sighted_at, as.Date("2023-04-01"), as.Date("2024-03-31")) ~ "23/24",
#                     dplyr::between(sighted_at, as.Date("2024-04-01"), as.Date("2025-03-31")) ~ "24/25"
#                   )
#   ) %>% 
#   dplyr::select(id, sighted_at, species, fiscal, source_entity) %>% 
#   dplyr::distinct() %>%
#   dplyr::group_by(fiscal, source_entity) %>% 
#   dplyr::summarise(count = dplyr::n())
# 
# 
# 
## Fiscal Alerts
# total
joined_tables %>%
  dplyr::filter(dplyr::between(sent_at, as.Date("2023-04-01"), as.Date("2025-03-31"))) %>%
  dplyr::mutate(fiscal =
                  dplyr::case_when(
                    dplyr::between(sent_at, as.Date("2023-04-01"), as.Date("2024-03-31")) ~ "23/24",
                    dplyr::between(sent_at, as.Date("2024-04-01"), as.Date("2025-03-31")) ~ "24/25"
                  )
  ) %>%
  dplyr::select(id, sent_at, species, fiscal, source_entity) %>%
  dplyr::distinct() %>%
  dplyr::group_by(fiscal) %>%
  dplyr::summarise(count = dplyr::n())

# table
joined_tables %>%
  dplyr::filter(dplyr::between(sent_at, as.Date("2023-04-01"), as.Date("2025-03-31"))) %>%
  dplyr::mutate(fiscal =
                  dplyr::case_when(
                    dplyr::between(sent_at, as.Date("2023-04-01"), as.Date("2024-03-31")) ~ "23/24",
                    dplyr::between(sent_at, as.Date("2024-04-01"), as.Date("2025-03-31")) ~ "24/25"
                  )
  ) %>%
  dplyr::select(id, sent_at, species, fiscal, source_entity) %>%
  dplyr::mutate(kw = dplyr::case_when(
    stringr::str_detect(species, "Killer") ~ 1,
    TRUE ~ 0
  )) %>%
  dplyr::distinct() %>%
  dplyr::mutate() %>%
  dplyr::group_by(fiscal, source_entity
                  , kw
                  ) %>%
  dplyr::summarise(count = dplyr::n())


## Bar chart of detections
sightings_clean %>% 
  dplyr::mutate(species = stringr::str_replace(species, "Whale", "whale")) %>% 
  dplyr::mutate(species = dplyr::case_when(
    species == "Sei whale" ~ "Rare whale species",
    species == "Blue whale" ~ "Rare whale species",
    species == "Sperm whale" ~ "Rare whale species",
    species == "North Pacific right whale" ~ "Rare whale species",
    species == "Bairds Beaked whale" ~ "Rare whale species",
    species == "Cuviers Beaked whale" ~ "Rare whale species",
    species == "Other rare species" ~ "Rare whale species",
    stringr::str_detect(species,"porpoise") == T ~ "Porpoise/dolphin species",
    stringr::str_detect(species,"dolphin") == T ~ "Porpoise/dolphin species",
    stringr::str_detect(species,"turtle") == T ~ "Turtle species",
    species == "False killer whale" ~ "Porpoise/dolphin species",
    TRUE ~ species
  )) %>% 
  dplyr::select(species) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise(detections = dplyr::n()) %>% 
  dplyr::arrange(desc(detections)) %>% 
  dplyr::mutate(species = factor(species, levels = species[order(detections)])) %>% 
  plotly::plot_ly(x = ~detections,
                  y = ~species,
                  color = ~species,
                  type = "bar") %>% 
  plotly::layout(legend = list(
    show = F
  ), 
    xaxis = list(title = "",
                              showline = TRUE,
                              showgrid = FALSE,
                              showticklabels = TRUE,
                              linecolor = 'rgb(204, 204, 204)',
                              linewidth = 2,
                              ticks = 'outside',
                              tickcolor = 'rgb(204, 204, 204)',
                              tickwidth = 2,
                              ticklength = 5,
                              # tickformat="%b %Y",
                              # ticktext = format("%b %Y"),
                              # dtick = "M1",
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 yaxis = list(title = "",
                              showgrid = T,
                              zeroline = FALSE,
                              showline = FALSE,
                              tickfont = list(family = 'Arial',
                                              size = 16,
                                              color = 'rgb(82, 82, 82)')),
                 tickfont = list(family = 'Arial',
                                 size = 18,
                                 color = 'rgb(82, 82, 82)'))


  





