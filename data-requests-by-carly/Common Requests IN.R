####~~~~~~~~~~~~~~~~~~Common Data Request Configuration~~~~~~~~~~~~~~~~~~####
##Author: Carly Green
##Date: May 24 2026 
##Purpose: This script is for common data requests both external and internal to the program of the Whale Report App/WRAS

####~~~~~~~~~~~~~~~~~~Step 1: Load Data Sources~~~~~~~~~~~~~~~~~~####

source("config.R")
source("data-import.R")
source("data-cleaning.R")

##Note that most data requests we are filtering sightings_main table for sightings, and alerts_main for alerts.

####~~~~~~~~~~~~~~~~~~~~~~Step 2: Setting Parameters~~~~~~~~~~~~~~~~~~~~~~####
  
#### Date Range (Default is from Jan 1 1931 to today) 
start_date = lubridate::as_date("1931-01-01")
end_date = lubridate::today() 
## QUESTION - is it better to have as start_date = lubridate::ymd_hms("2025-04-01 00:00:00", tz = "America/Los_Angeles") or UTC? or none?
## QUESTION/COMMENT - need to add year comparisons? calendar year and fiscal year? 

#### Species Filter (use null for all species)
species_filter = NULL
### example species_filter = c("Killer whale")

#### Source Entity Filter
source_filter = NULL 
### example 
#species_filter = c("Ocean Wise Conservation Association", "Orca Network via Conserve.io app")

#### **********I need to still add context about which source entities to include when*******
## Generally, external data requests, just filter for OWCA as the report_source_entity. Using common sense for most right now. 

#### Area Filter Default = no spatial filter
area_of_interest = NULL

### examples
# area_of_interest = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/samplearea.geojson"

# area_of_interest = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/North Coast Requests/North Coast Data Pull.shp"


### Request Area
request_area = NULL

### if area is provided then uncomment and run 
# if(!is.null(area_of_interest)) {
#   request_area = sf::st_read(area_of_interest)
#   
#   ## Check geometry
#   print(sf::st_crs(request_area))
#   
#   ## Transform if required to WGS84
#   request_area = sf::st_transform(request_area, 4326)
# }
# 
# ## OPTIONAL check the request_area 
# leaflet::leaflet() |> 
#   leaflet::addTiles() |>  # or addProviderTiles(providers$CartoDB.Positron)
#   leaflet::addPolygons(data = request_area, 
#                        color = "red", 
#                        fill = FALSE, 
#                        weight = 2)

####~~~~~~~~~~~~~~~~~~~~~~Step 3: Filter sightings_main table for sightings~~~~~~~~~~~~~~~~~~~~~~####

### Sightings Main to SF 
sightings_filtered = sightings_main |> 
  sf::st_as_sf(
    coords = c("report_longitude", "report_latitude"), crs = 4326, remove = FALSE)

sightings_filtered = sightings_filtered |> 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date) 
##QUESTION/COMMENT at this point sightings main and sightings_filtered should be the same, I'm not getting exact match. hmm.

### Species Filter
if(!is.null(species_filter)) {
  sightings_filtered = sightings_filtered |> 
    dplyr::filter(species_name %in% species_filter)
}

### OPTIONAL Ecotype Filter 
# sightings_filtered = sightings_filtered |> 
#   dplyr::filter(ecotype_name %in% c("Northern Resident", "Transient"))
  # dplyr::filter(ecotype_name != "Northern Resident" | is.na(ecotype_name))


### Source Entity Filter 
##QUESTION what is the source just WhaleSpotter (others are labeled specifically as CMN, or FIN ISLAND, or SATURNA etc.)
if(!is.null(source_filter)) {
  sightings_filtered = sightings_filtered |> 
    dplyr::filter(report_source_entity %in% source_filter)
}

### Area Filter
if(!is.null(area_of_interest)) {
  sightings_filtered = sightings_filtered |> 
    sf::st_filter(request_area)
}

### Remove Rejected Sightings
sightings_filtered = sightings_filtered |> 
  dplyr::filter(
    report_status %in% c("approved", "auto_approved", "on_review", "created", "waiting_review")
  )

### Rounding time by 5 minutes and removing duplicates -- ##Question should it be 5 min or3 min?
sightings_filtered = sightings_filtered |> 
  dplyr::mutate(
    ##Convert everything to PST/PDT consistently and rounding by 15 minutes (question - is everything UTC now?)
    # sighting_date = lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles"),
    lat_rnd = round(report_latitude, 4),
    lon_rnd = round(report_longitude, 4),
    time_bucket = lubridate::round_date(sighting_date, "5 mins"),
  ) 

### Final cleaning and duplicate removal 
## QUESTION - does this not work for the historical sightings that have no time stamp (as it will round them all to midnight?)
sightings_filtered = sightings_filtered |> 
  dplyr::group_by(lat_rnd, lon_rnd, time_bucket) |> 
  dplyr::mutate(
    is_duplicate = dplyr::n() > 1
  ) |> 
  dplyr::slice(1)

### Tidying up arrangement and rename confidence column, drop geometry
sightings_filtered = sightings_filtered |> 
  dplyr::ungroup() |> 
  dplyr::rename(confidence = observer_confidence) |> 
  dplyr::arrange(sighting_date) |> 
  sf::st_drop_geometry()

cat(sprintf("Records after date, species, source, + area filter: %d\n", nrow(sightings_filtered)))


####~~~~~~~~~~~~~~~~~~~~~~Step 4: Filter alerts_main table for unique notifications~~~~~~~~~~~~~~~~~~~~~~####

##use alerts_main for unique notifications
#QUESTION - #use main_dataset could work but it has more of the sighting columns too? Is that the only difference now? 

alerts_filtered = alerts_main |> 
  dplyr::filter(
    alert_created_at >= start_date,
    alert_created_at <= end_date)

alerts_filtered = alerts_filtered |> 
  dplyr::filter(sighting_id %in% sightings_filtered$sighting_id) |> 
  dplyr::mutate(latitude = report_latitude,
                longitude = report_longitude)

cat(sprintf("Records of unique notifications: %d\n", nrow(alerts_filtered)))

##Use Common Requests OUT for specific visualizations, output tables, summaries.
