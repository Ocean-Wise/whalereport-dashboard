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

##load library and user name 
library(readxl)

##Using OWSN OLD DATA PULL 2015-2025 - Copy as sheet 1 and NC_Request_2015_to_May25 as sheet 2
sheet_1 = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/OWSN OLD DATA PULL 2015-2025 - Copy.xlsx")
sheet_2 = read_excel("C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/Comparisons/NC_Request_2015_to_May25.xlsx")


##check str
str(sheet_1)
str(sheet_2)


##Find rows in sheet 1 that are NOT in sheet 2 
diff_in_1 = dplyr::anti_join(sheet_1, sheet_2, by = c("report_latitude", "report_longitude"))

#Find rows in sheet 2 that are NOT in sheet 1 (Reverse check)
diff_in_2 = dplyr::anti_join(sheet_1, sheet_2, by = c("report_latitude", "report_longitude"))

##View results
print(diff_in_1)
print(diff_in_2)
