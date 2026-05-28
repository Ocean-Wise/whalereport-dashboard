####~~~~~~~~~~~~~~~~~~Common Data Request Configuration~~~~~~~~~~~~~~~~~~####
##Author: Carly Green
##Date: May 24 2026 
##Purpose: This script is for common data requests both external and internal to the program of the Whale Report App/WRAS

####~~~~~~~~~~~~~~~~~~Step 1: Load Data Sources~~~~~~~~~~~~~~~~~~####

source("config.R")
source("data-import.R")
source("data-cleaning.R")

##Note that most data requests we are alerting sightings_main table for sightings, and alerts_main for alerts.

####~~~~~~~~~~~~~~~~~~~~~~Step 2: Setting Parameters~~~~~~~~~~~~~~~~~~~~~~####
  
#### Date Range (Default is from Jan 1 1931 to today) 
start_date = lubridate::as_date("1931-01-01")
end_date = lubridate::today() 
 ## Question - is it better to have as start_date = lubridate::ymd_hms("2025-04-01 00:00:00", tz = "America/Los_Angeles")

#### Species Filter (use null for all species)
species_filter = NULL
### example species_filter = c("Killer whale")

#### Source Entity Filter
source_filter = NULL 
### example species_filter = c("Ocean Wise Conservation Association", "Orca Network via Conserve.io app")

#### add context about which source entities to include when*******
## Generally, external data requests, just filter for OWCA as the report_source_entity

#### Area Filter
area_of_interest = NULL
### example 
area_of_interest = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/samplearea.geojson"

if(!is.null(area_of_interest)) {
  request_area = sf::st_read(area_of_interest)
  
  ## Check geometry
  sf::st_crs(request_area)
  
  ## Transform if required to WGS84
  request_area = sf::st_transform(request_area, 4326)
}

## OPTIONAL check the request_area 
leaflet::leaflet() |> 
  leaflet::addTiles() |>  # or addProviderTiles(providers$CartoDB.Positron)
  leaflet::addPolygons(data = request_area, 
                       color = "red", 
                       fill = FALSE, 
                       weight = 2)

####~~~~~~~~~~~~~~~~~~~~~~Step 3: Filter sightings_main table for sightings~~~~~~~~~~~~~~~~~~~~~~####

#### SIGHTINGS MAIN TO SF 
sightings_filtered = sightings_main |> 
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

sightings_filtered = sightings_filtered |> 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) 
##Question at this point sightings main and sightings_filtered should be the same, sightings_main has about 100 more sightings.

### SPECIES FILTER
if(!is.null(species_filter)) {
  sightings_filtered = sightings_filtered |> 
    dplyr::filter(species_name %in% species_filter)
}

### OPTIONAL ECOTYPE FILTER 
# sightings_filtered = sightings_filtered |> 
#   dplyr::filter(ecotype_name %in% c("Northern Resident", "Transient"))
  # dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name))


### SOURCE ENTITY FILTER 
if(!is.null(source_filter)) {
  sightings_filtered = sightings_filtered |> 
    dplyr::filter(report_source_entity %in% source_filter)
}

### AREA FILTER 
if(!is.null(area_of_interest)) {
  sightings_filtered = sightings_filtered |> 
    sf::st_filter(request_area)
}

### REMOVE REJECTED 
sightings_filtered = sightings_filtered |> 
  dplyr::filter(
    report_status %in% c("approved", "auto_approved", "on_review", "created", "waiting_review")
  )

###OPTIONAL ROUNDING TO CHECK FOR DUPLICATES 
sightings_filtered = sightings_filtered |> 
  dplyr::mutate(
    ##Convert everything to PST/PDT consistently and rounding by 15 minutes
    # sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "5 mins"),
  ) 

## Final cleaning and duplicate removal 
sightings_filtered = sightings_filtered |> 
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) |> 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) |> 
  dplyr::slice(1)

## Tidying up arrangement and rename confidence column
sightings_filtered = sightings_filtered |> 
  dplyr::ungroup() |> 
  dplyr::rename(confidence = observer_confidence) |> 
  dplyr::arrange(sighting_date) |> 
  sf::st_drop_geometry()

cat(sprintf("Records after date, species, source, + area filter: %d\n", nrow(sightings_filtered)))


####~~~~~~~~~~~~~~~~~~~~~~Step 4: Filter alerts_main table for unique notifications~~~~~~~~~~~~~~~~~~~~~~####

##use Alerts_main for unique notifications
##use Main_dataset could work but it has more of the sighting columns too? 

alerts_filtered = alerts_main |> 
  dplyr::filter(
    alert_created_at >= start_date,
    alert_created_at <= end_date)

alerts_filtered = alerts_filtered |> 
  dplyr::filter(sighting_id %in% filtered_sighting_ids)

cat(sprintf("Records after date, species, source, + area filter: %d\n", nrow(alerts_filtered)))
