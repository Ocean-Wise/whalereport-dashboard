####~~~~~~~~~~~~~~~~~~~~~~Cleaning WRAS data~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Alex Mitchell
## Purpose: To automate the cleaning of sightings network data as far as possible to be used in future
##          analysis for reporting, monthly dashboard etc.
## Date written: 2023-12-21
## Date updated: 2024-09-16
## Quality Assured: No

####~~~~~~~~~~~~~~~~~~~~~~Info~~~~~~~~~~~~~~~~~~~~~~~####
## Currently not able to automate this data process, so to download the data, ensure to change the

library(magrittr)
####~~~~~~~~~~~~~~~~~~~~~~Data Import API ~~~~~~~~~~~~~~~~~~~~~~~####

# 
# ## API query to get sightings data from the sightings database without downloading the whole dataset...
# 
#  base_url = "https://sightingsapi.ocean.org/sightings"
#  api_key = "XXX"
# 
#  params = list(
#    api_key = api_key,
#    limit = 100,
#    page = 1,
#    sourceType = "observed"
#  )
# 
#  # Function to query API
#  fetch_data = function(base_url, params) {
#    response = httr::GET(
#      url = base_url,
#      query = params,
#      httr::add_headers(`x-api-key` = api_key)
#    )
#    content = httr::content(response, as = "parsed")
#    content$sightings
#  }
# 
#  # Function to flatten a single record
#  flatten_record = function(record) {
#    record$additionalProperties = NULL
#    purrr::list_flatten(record)
#  }
# 
#  # Fetch and combine all pages
#  all_data = list()
# 
#  repeat {
#    data = fetch_data(base_url, params)
# 
#    if (length(data) == 0) break
# 
#    data_df = purrr::map_dfr(data, flatten_record)
#    all_data = dplyr::bind_rows(all_data, data_df)
# 
#    params$page = params$page + 1
#  }
# test test 
#  source_filter = c("Ocean Wise", "Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert")
#  
#  # Clean and filter
# combined_data = all_data %>%
#  dplyr::distinct() %>%
#  janitor::clean_names() %>%
#  dplyr::mutate(source_entity =
#                  dplyr::case_when(
#                    is.na(source_entity) == T ~ "Ocean Wise",
#                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
#                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network",
#                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network",
#                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "WhaleSpotter",
#                    stringr::str_detect(source_entity, "JASCO") == T ~ "JASCO",
#                    stringr::str_detect(source_entity, "SMRUC") == T ~ "SMRU",
#                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "Whale Alert",
#                    stringr::str_detect(source_entity, "SWAG") == T ~ "SWAG",
#                    TRUE ~ "test")) %>% 
#  dplyr::filter(source_entity != "test" & source_entity %in% source_filter) %>% 
#  dplyr::filter(
#    !stringr::str_detect(location_desc, "(?i)\\btest\\b"),
#    !stringr::str_detect(comments, "(?i)\\btest\\b"),
#    !stringr::str_detect(reporter_email, "(?i)\\btest\\b"),
#    !stringr::str_detect(reporter_name, "(?i)\\btest\\b")
#  )
 
####~~~~~~~~~~~~~~~~~~~~~~Data Import - non-API~~~~~~~~~~~~~~~~~~~~~~~####

## Define a filter for the data so we can remove any org that we shouldn't have in the data
source_filter = c("Ocean Wise", "Orca Network", "WhaleSpotter", "JASCO", "SMRU", "Whale Alert")

## Get a list of files in the directory which we want to get the data from. It is important that older files are overwritten, not added. 
file_list = list.files("/Users/alexmitchell/Downloads/OneDrive_1_12-15-2025/", full.names = T) %>% 
  purrr::keep(~ stringr::str_ends(.x, ".csv"))
  


list_of_dfs = purrr::map(file_list, ~readr::read_csv(.x) %>% 
                           janitor::clean_names() %>% 
                           ## filter any columns which start with "s" and finish with "_at" for dates >= 2019 jan 1st and onwards
                           dplyr::filter(., dplyr::if_any(dplyr::starts_with("s") & dplyr::ends_with("_at"), 
                                                          ~ . >= as.Date("2019-01-01"))))



df_name = c("alert_historic", #alert recent data from database 
            "alert_raw", #alert historic data from database 
            "detection_historic", #contains sightings
            "detection_recent", #historical data from database so we don't have to process all data everytime
            "user_raw")

named_dfs = setNames(list_of_dfs, df_name)

list2env(named_dfs, envir = .GlobalEnv)

rm(list = c("list_of_dfs", "named_dfs", "df_name", "file_list"))

sightings_spreadsheet = openxlsx::read.xlsx(paste0("C:/Users/", user,
                                                 "/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Sightings/Cheat Sheet Archive/BCCSN Sightings Master.xlsx"),
                                          sheet = "app") %>%
  janitor::clean_names() %>% 
  tibble::as_tibble()

####~~~~~~~~~~~~~~~~~~~~~~Data Clean~~~~~~~~~~~~~~~~~~~~~~~####
## alert cleaning
alert_clean = alert_historic %>%
  dplyr::bind_rows(alert_raw) %>% 
  dplyr::select(-location_id) %>% 
  dplyr::distinct()

  
## sightings spreadsheet cleaning
sightings_clean = sightings_spreadsheet %>% 
  dplyr::mutate(date = as.Date(date, origin = "1899-12-30"),
                time = time * 86400,
                date_time = as.POSIXct(date) + time,
                sub_date_time = strptime(stringr::str_extract(source_name, 
                                                              "(?<=Report)[0-9\\- ]+"), 
                                         format = "%Y-%m-%d %H-%M-%S")) %>% 
  ## using the = in here just renames column in the format new name = old name - i.e. id = report_id
  dplyr::select(sub_date_time, date_time, id = report_id, species = species_name, ecotype,
                species_category, latitude = latitude_dd, longitude = longitude_dd,
                number_of_animals, email = reporter_email, organization = db_org,
                confidence = id_confidence) %>% 
  dplyr::mutate(latitude = as.numeric(latitude),
                longitude = as.numeric(longitude),
                number_of_animals = as.numeric(number_of_animals)) %>% 
                ## THIS NEEDS TO BE ALTERED TO EXTRACT RANGE exp. 4~7
  dplyr::distinct()


## Detection cleaning
detections_clean = detection_historic %>% 
  dplyr::mutate(sighted_at = lubridate::ymd_hm(detection_historic$sighted_at),
                created_at = lubridate::ymd_hm(detection_historic$created_at)) %>%
    dplyr::bind_rows(detection_recent) %>% 
  dplyr::distinct() %>% 
  ## The following steps turn the "details" column into individual columns which contain data
  dplyr::mutate(details = stringr::str_remove_all(details, "[\\\\\"{}]")) %>% 
  dplyr::mutate(details = stringr::str_remove(details, "^[^:]+:")) %>% 
  dplyr::mutate(details = stringr::str_replace_all(details, "\\s*:\\s*", ":")) %>%
  dplyr::mutate(details = stringr::str_replace_all(details, "\\s*,\\s*", ",")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "sightingDistance:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "travelDirection:")) %>%
  dplyr::mutate(details = stringr::str_remove(details, "hydrophoneMeta:")) %>%
  tidyr::separate_rows(details, sep = ",") %>% 
  tidyr::separate(col = details, into = c("colname", "data"), sep = ":") %>% 
  dplyr::mutate(colname = stringr::str_replace(colname, "id", "value")) %>% 
  dplyr::mutate(colname = stringr::str_replace(colname, "name", "species")) %>% 
  dplyr::mutate(colname = stringr::str_trim(colname)) %>% 
  ## Always group by ID to let R know the records are related
  dplyr::group_by(id) %>% 
  dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., ""))) %>% 
  # dplyr::select(-c("category", "direction")) %>% 
  ## This then creates the individual columns by pivoting the data from long format to WIDE format
  tidyr::pivot_wider(names_from = colname, values_from = data) %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty(which = "cols") %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(source_entity =
                  dplyr::case_when(
                    is.na(source_entity) == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Ocean Wise") == T ~ "Ocean Wise",
                    stringr::str_detect(source_entity, "Acartia") == T ~ "Orca Network via Conserve.io app",
                    stringr::str_detect(source_entity, "Orca Network") == T ~ "Orca Network via Conserve.io app",
                    stringr::str_detect(source_entity, "WhaleSpotter") == T ~ "WhaleSpotter",
                    stringr::str_detect(source_entity, "QUIETSOUND") == T ~ "WhaleSpotter",
                    stringr::str_detect(source_entity, "JASCO") == T ~ "JASCO",
                    stringr::str_detect(source_entity, "SMRUC") == T ~ "SMRU",
                    stringr::str_detect(source_entity, "Whale Alert") == T ~ "Whale Alert Alaska",
                    stringr::str_detect(source_entity, "SWAG") == T ~ "SWAG",
                    TRUE ~ source_entity)) %>% 
  dplyr::mutate(source_entity = tolower(source_entity)) %>% 
  dplyr::filter(stringr::str_detect(source_entity, "test|string|swag") == F)



## user cleaning
user_clean = user_raw %>%
  dplyr::select(c(tracking_id = id, auth_id, name, phonenumber, organization, vessel = vesselname))


  ####~~~~~~~~~~~~~~~~~~~~~~Data tidy~~~~~~~~~~~~~~~~~~~~~~~####
 
rm(list = c("alert_historic", 
            "alert_raw", 
            "detection_historic", 
            "detection_recent",
            "sightings_spreadsheet",
            "user_raw"))
  

####~~~~~~~~~~~~~~~~~~~~~~Metric Sandbox~~~~~~~~~~~~~~~~~~~~~~~####


## sightings 2022 vs 2023
### Use a combination of the Detections df and the sightings spreadsheet to get update to date info on sightings
### 1. match cols between detections and sightings
### 2. rbind rows to merge datasets 
### 3. group per year
### 4. comparison per year (with and without autonomous detections?)

## As all of the sightings in the spreadsheet are not in real time (some submitted later via the app),
## I need to match the sub time vs sighting time to see what is in real time, I would count real time 
## as within an "Alert Period" of 30mins. Therefore, subtime - date < 30mins

# real_time_sightings = sightings_clean %>% 
  
  
#   
# sightings_match = sightings_clean %>% 
#   dplyr::select(id, species, 
#                 latitude, longitude, 
#                 date, number_of_animals,
#                 email)
# 
# detections_match = detections_clean %>% 
#   dplyr::select(c(date = sighted_at,
#                   latitude, longitude,
#                   number_of_animals = numberOfAnimals,
#                   email, id,
#                   species = name)) %>% 
#   dplyr::filter(!id %in% sightings_match$id)
# 
# sightings_combined = rbind(sightings_match, detections_match)
# 
# total_sightings = sightings_combined %>% 
#   dplyr::group_by(year = lubridate::year(date)) %>% 
#   dplyr::summarise(count = dplyr::n())
# 
# period_2022 = total_sightings[6,2]
# 
# period_2023 = total_sightings[7,2]


## number of north coast 2023 vs 2022
# 
# north_coast_sightings = sightings_combined %>% 
#   dplyr::filter(dplyr::between(latitude, 51, 56) | dplyr::between(longitude, 126, 135)) %>% 
#   dplyr::group_by(year = lubridate::year(date)) %>% 
#   dplyr::summarise(count = dplyr::n()) %>% 
#   dplyr::mutate(perc_diff = ((count-dplyr::lag(count)/dplyr::lag(count)))*100)


## BC Sightings
# Coordinates 
# 51°00.00’N to 56°00.00’N (with Stewart, BC) 
# 126°00.00’W to 135°00.00’W 


#   
#   ## Who recieved alerts? 
#   
#   alert_id = alert_clean %>% 
#     dplyr::left_join(user_clean, by = "tracking_id") %>% 
#     dplyr::mutate(vessel = stringr::str_remove_all(vessel, "mv |m v|MV|M/V ")) %>% 
#     ## The next mutates clean up some of the common spelling errors and bits the fuzzy matching misses. Repeated for vessel data.
#     dplyr::mutate(vessel = tolower(vessel)) %>% 
#     dplyr::mutate(vessel = dplyr::case_when(vessel == "charles hays amwaal" ~ "charles hays",
#                                             vessel == "amwaal" ~ "charles hays",
#                                             vessel == "cowichan" ~ "queen of cowichan",
#                                             vessel == "sobc" ~ "spirit of british columbia",
#                                             vessel == "qalb" ~ "queen of alberni",
#                                             vessel == "spirit of bc" ~ "spirit of british columbia",
#                                             vessel == "reliant" ~ "seaspan reliant",
#                                             vessel == "queen of newwest" ~ "queen of new westminster",
#                                             vessel == "q of alberni" ~ "queen of alberni",
#                                             vessel == "mazuru bishamon" ~ "maizuru bishamon",
#                                             vessel == "coastal renn" ~ "coastal renaissance",
#                                             vessel == "sovc" ~ "spirit of vancouver island",
#                                             vessel == "q of alberni" ~ "queen of alberni",
#                                             vessel == "qnw" ~ "queen of new westminster",
#                                             vessel == "laurier" ~ "sir wilfrid laurier",
#                                             vessel == "suquamish" ~ "wsf suquamish",
#                                             vessel == "howe sounbd queen" ~ "howe sound queen",
#                                             vessel == "inspiration" ~ "coastal inspiration",
#                                             vessel == "suquwamish" ~ "wsf suquamish",
#                                             vessel == "seapan zambizi" ~ "seaspan zambezi",
#                                             vessel == "sprit of british columbia" ~ "spirit of british columbia",
#                                             vessel == "roald almundsen" ~ "roald amundsen",
#                                             vessel == "ovean clio" ~ "ocean clio",
#                                             vessel == "coroleader ol" ~ "coreleader ol",
#                                             vessel == "zuidetdam" ~ "zuiderdam",
#                                             vessel == "seaspan anadonis" ~ "seaspan adonis",
#                                             vessel == "berge yotie" ~ "berge yotei",
#                                             vessel == "carnval splendor" ~ "carnival splendor",
#                                             vessel == "blackball" ~ "coho",
#                                             vessel == "zeta" ~ "star zeta",
#                                             vessel == "cma cgm rigalito" ~ "cma cgm rigoletto",
#                                             vessel == "gulf islands spirit" ~ "spirit of vancouver island",
#                                             vessel == "zeda" ~ "star zeta",
#                                             vessel == "zeta" ~ "star zeta",
#                                             TRUE ~ vessel))
# 
# bc_ferries_alerts = alert_id %>% 
#   dplyr::filter(stringr::str_detect(organization, "B[[:space:].]*C[[:space:].]*F")) %>% 
#   dplyr::mutate(vessel = dplyr::case_when(stringr::str_detect(vessel, "land") == T ~ "unknown",
#                                           stringr::str_detect(vessel, "on land") == T ~ "unknown",
#                                           stringr::str_detect(vessel, "(?i)BC\\s*Ferries?|Ferry") == T ~ "unknown",
#                                           vessel == "mix" ~ "unknown",
#                                           vessel == "shore" ~ "unknown",
#                                           vessel == "spirit of british colombia" ~ "spirit of british columbia",
#                                           vessel == "c falcon" ~ "centennial falcon",
#                                           vessel == "spirit of britis columbia" ~ "spirit of british columbia",
#                                           vessel == "howe soubd queen" ~ "howe sound queen",
#                                           vessel == "howe soubd queen" ~ "howe sound queen",
#                                           vessel == "sovi" ~ "spirit of vancouver island",
#                                           vessel == "cosstal inspiration" ~ "coastal inspiration",
#                                           vessel == "queen alberni" ~ "queen of alberni",
#                                           vessel == "norther adventure" ~ "northern adventure",
#                                           TRUE ~ vessel)) %>% 
#   dplyr::group_by(vessel) %>% 
#   dplyr::summarise(count = dplyr::n())
#   
# user_clean %>% 
#   dplyr::filter(tracking_id %in% c("208a89cd-daea-cd53-6e83-f49e4da9d3a1", 
#                                    "4deac481-edb3-c5bd-cfb9-3e4b3fa48161",
#                                    "864f93b8-ead2-cd5f-6278-e67dedfa8c1c"))
    

# file_list = list.files("C:/Users/alext/Ocean Wise Conservation Association/Whales Initiative - General/Ocean Wise Data/dashboard/", full.names = T)

# sightings_spreadsheet = readxl::read_xlsx(paste0("C:/Users/alext/Ocean Wise Conservation Association/Whales Initiative - General/BCCSN.Groups/Sightings/Cheat Sheet Archive/BCCSN Sightings Master.xlsx"),
#                                           sheet = "app") %>%
#   janitor::clean_names()

