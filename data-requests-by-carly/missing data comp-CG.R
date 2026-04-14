####~~~~~~~~~~~~~~~~~~~~~~Comparisons~~~~~~~~~~~~~~~~~~~~~~~####
## Author: Carly Green
## Purpose: Comparing the difference between Kaylas Owsn Data pull from May 2025 and Carly's Data pull trying to recreate in new database
## Date written: 2026-02-24

##Sources:Kayla used BCCSN master spreadsheet and OWSN Access Database Carly used the new database. 

##First testing the humpbacks only which is in coordinates box below. 
##dates are Jan 1 2015 - May 25 2025 


box_coords <- matrix(
  c(
    -133.7003333, 51.916667, #lower left
    -133.7003333, 55.63341667, #upper left
    -128.0416667, 55.63341667, #upper right 
    -128.0416667, 51.916667,#lower right
    -133.7003333, 51.916667 #closer polygon
  ),
  ncol = 2,
  byrow = TRUE
)

box_polynew = sf::st_polygon(list(box_coords)) %>% 
  sf::st_sfc(crs = 4326)


##load library
library(readxl)

##Using OWSN OLD DATA PULL 2015-2025 - Copy as sheet 1 and NC_Request_2015_to_May25 as sheet 2 and my original sighting merge from June as sheet 3
sheet_1 = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/OWSN OLD DATA PULL 2015-2025 - Copy.xlsx") %>%
  dplyr::mutate(
    sighting_date = lubridate::mdy(sighting_date),
    time_numeric = as.numeric(time),
    time = hms::as_hms(time_numeric * 86400)
  ) %>%
  dplyr::select(-time_numeric) %>% 
  dplyr::mutate(sighting_date = as.POSIXct(sighting_date) + time) %>% 
  dplyr::select(-c(time, ecotype_name))

sheet_2 = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/NC_Request_2015_to_May25.xlsx")
sheet_3 = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/sighting-test2025-06-10.xlsx")

sheet_3_clean = sheet_3 %>% 
  dplyr::filter(year >= 2015 & year <= 2025,
                stringr::str_detect(species_name, stringr::regex("humpback", ignore_case = TRUE))) %>% 
  tidyr::drop_na(latitude, longitude) %>% 
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = FALSE) %>% 
  sf::st_filter(box_polynew) %>% 
  dplyr::rename(report_latitude = latitude,
                report_longitude = longitude)
  

##find rows in sheet 3 clean that are not in sheet 1
diff_in_3 = dplyr::anti_join(sheet_3_clean, sheet_1, by = c("report_longitude"))

##find rows in sheet 3 clean that are not in sheet 2 (the new database)
diff_in_4 = dplyr::anti_join(sheet_3_clean, sheet_2, by = c("report_longitude")) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(.$source) %>% 
  dplyr::summarise(count = dplyr::n())


##total count of sources in sheet 3 clean (filtered for humpbacks in NC)
sheet_3_clean %>% 
  dplyr::group_by(.$source) %>% 
  dplyr::summarise(count = dplyr::n())

##what is not in sightings_main that is in sheet_3 
diff_in_db = dplyr::anti_join

##check str
str(sheet_1)
str(sheet_2)


##Find rows in sheet 1 that are NOT in sheet 2 
diff_in_1 = dplyr::anti_join(sheet_1, sheet_2, by = c("report_latitude"))

#Find rows in sheet 2 that are NOT in sheet 1 (Reverse check)
diff_in_2 = dplyr::anti_join(sheet_2, sheet_1, by = c("report_latitude"))

##View results
print(diff_in_1)
print(diff_in_2)


##~~~~~Now filtering and comparing for ALL missing data~~~~##

##step 1 filter sightings_main by report source entity OWCA as that should get the number of successfully imported data from OWCA  
sightings_main_sources = sightings_main %>% 
  dplyr::group_by(.$report_source_entity) %>% 
  dplyr::summarise(count = dplyr::n())

### looks like 165930 sightings from Owca were imported 


####~~~~comparing missing data using sept sightings merge vs sightings main (sample size)~~~~~#####

##step 2 using a sample size of sightings from 2023

##load library and spreadsheets
sample_sheet = readxl::read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/sample_size_2023.xlsx")

##step 3 pull out sightings_main from 2023 April and May and save this in appropriate folder
sightings_filtered = sightings_main %>% 
  dplyr::filter(sighting_year == 2023) %>% 
  dplyr::filter(sighting_month == 4 & 5)

##step 4 save the sightings_filtered as xlsx to look for matching sample sightings. 
writexl::write_xlsx(sightings_filtered, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/sightings_filtered_2023.xlsx")

##step 5 load new sample size of sightings_main which just has the sightings from sample_sheet 
sightings_main_sample = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/sightings_main_sample.xlsx")

###ensured that latitude and longitude in sightings merge are up to 8 decimal places

##comparing sample_sheet and sightings_main_sample
differences = dplyr::anti_join(sample_sheet, sightings_main_sample, by = c("report_longitude")) ##no differences by longitude.
 
##summary notes 
##when I generally compare these I am noticing mapping issues in ecotype,observer confidence, sightings platform, 
##number of animals for range blank, some differences in org but that may not be mapping.
##sometimes range leads to blank, sometimes it is a number, should it be the bottom or top number? 

#####~~~~~ Not just using sampple size~~~~~#####

##attempt with september sightings merge - I have ensured lat and long is to as many dec points as possible and columns are report_latitude and report_longitude
sightings_merge = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/sighting-merge-copy-2025-09-12.xlsx")

##total of 6473 sightings that are in sightings merge not in sightings main (I think)
differences_all = dplyr::anti_join(sightings_merge, sightings_main, by = c("report_longitude"))

# writexl::write_xlsx(differences_all, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/difference_table.xlsx")
# writexl::write_xlsx(missing_sources, "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/difference_table1.xlsx")


##look at what makes ups the 6473 missing sightings by source
missing_sources = differences_all %>% 
  dplyr::group_by(.$source) %>% 
  dplyr::summarise(count = dplyr::n())

#summary notes 
##Does not matter that sightings merge sheet goes to september 12, all missing data is historical (pre may 2025)
##I was expecting to find 5843 missing sightings.
## I seemed to find 6473 missing sightings primarily from logbooks but various sources. 





###OLD Attempt###
# ##reformat the sample_sheet to have a new column that matches the format of sighting_date 
# sample_sheet = sample_sheet %>% 
#   dplyr::mutate(
#     sighting_date = as.POSIXct(
#       paste(sub_date, sub_time),
#       format = "%Y-%m-%d %H:%M:%OS"
#     ),
#     sighting_date = format(sighting_date, "%Y-%m-%d %H:%M:00")
#   )

##checking the structure and time zones in sample_sheet 
# str(sightings_main$sighting_date)
# attr(sightings_main$sighting_date, "tzone")
# 
# 
# str(sample_sheet$sighting_date)
# attr(sample_sheet$sighting_date, "tzone")
# 
# ##change sample_sheet to POSIXct and UTC time zone 
# sample_sheet = sample_sheet %>%
#   dplyr::mutate(
#     sighting_date = as.POSIXct(
#       sighting_date,
#       format = "%Y-%m-%d %H:%M:%S",
#       tz = "UTC"
#     )
#   )
# 
# 
# ##first filter did not work - need to adjust the sighting_date column to be rounded the same as in sample_sheet
# sightings_main =  sightings_main %>%
#   dplyr::mutate(
#     sighting_date = lubridate::floor_date(sighting_date, "minute")
#   )
# 
# sample_sheet = sample_sheet %>% 
#   dplyr::mutate(sighting_date = lubridate::floor_date(sighting_date, "minute"))
# 
# ##filter sightings_main by the sightings in sample_sheet but now am filtering for sightings within sample_sheet in sightings_main 
# sightings_filtered = sightings_main %>% 
#   dplyr::filter(sighting_date %in% sample_sheet$sighting_date)
# 
# 
# ##Find rows in sample size not in sightings main 
# diffs_sample = dplyr::anti_join(sample_sheet, sightings_main, by = c("report_latitude"))
# 
