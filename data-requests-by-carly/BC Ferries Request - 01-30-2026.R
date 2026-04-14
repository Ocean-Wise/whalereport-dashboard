##BC Ferries Data Request January 2026##
##Author: Carly Green
##Date: Jan 16 2026

##need to provide tables of all bc ferry users sightings, as well as who is receiving alerts for 2025
##requester wants to be able to see 
#who the top sighters are
#who has provided most detail in their sighting such as comments and behaviours
#who reported the most variety of species types 

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##update the start date
start_date = lubridate::as_date("2025-01-01")
end_date = lubridate::as_date("2025-12-31")

View(sightings_main %>% 
       dplyr:: filter(sighting_date >= start_date, sighting_date <= end_date))

bcferries = sightings_main %>% 
  dplyr::filter(
    sighting_date >= start_date,
    sighting_date <= end_date,
    stringr::str_detect(observer_organization, 
                        stringr::regex("bc\\s*fer", ignore_case = TRUE)
    ) | stringr::str_detect(observer_email, "bcferries") #including any bc ferries organizations OR bcferries emails
  ) %>%  
  dplyr::mutate(
    sighting_date = dplyr::case_when(
      stringr::str_detect(comments,"Historical Import") == T ~ lubridate::force_tz(sighting_date, tzone = "America/Los_Angeles"),
      TRUE ~ lubridate::with_tz(sighting_date, tzone = "America/Los_Angeles")
    )) %>% 
  dplyr::mutate(sighting_date = lubridate::floor_date(sighting_date, unit = "minute")) %>% 
  dplyr::mutate(date = lubridate::as_date(sighting_date)) %>% 
  dplyr::distinct(sighting_date, .keep_all = TRUE) %>% 
  dplyr::mutate(
    comments = dplyr::case_when( #historical import WITH usable comments extracted 
      stringr::str_detect(comments, "Historical Import") &
        stringr::str_detect(comments, "Comments:") ~
        stringr::str_trim(
          stringr::str_extract(
            comments, '(?i)(?<=comments:)\\s*.*')),
      !is.na(comments) &
        stringr::str_detect(comments, "Historical") ~ NA_character_,
      TRUE ~ comments
    ),
    comments = stringr::str_trim(comments),
    comments = dplyr::na_if(comments, ""),
    comments = dplyr::na_if(comments, "-")) #historical comments with just a dash are removed to NA

###ensuring unique categories are what we want
unique(bcferries$observer_organization) #lots of BC Ferries varieties and spellings. 
unique(bcferries$report_source_entity) #Just OWCA is good. 
unique(bcferries$sighting_platform_name) #I will remove this column 

##make all of the observer_organization the same 
bcferries = bcferries %>% 
  dplyr::mutate(
    observer_organization = "BC Ferries"
  ) %>% ##remove columns we don't want 
  dplyr::select(-c(observer_confidence, sighting_platform_name, total_reports, observer_type_name, report_modality, report_source_type))

##checking only organization is BC Ferries
unique(bcferries$observer_organization)

##breakdown of total sighters or vessels in BC ferries and their sighitngs by species 
bcferries_observers = bcferries %>%
  dplyr::group_by(
    observer = observer_email,
    species = species_name
  ) %>%
  dplyr::summarise(
    sightings_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = species,
    values_from = sightings_count,
    values_fill = 0
  ) %>%
  dplyr::mutate(
    total_sightings = rowSums(dplyr::across(where(is.numeric)))
  ) %>% 
  dplyr::arrange(desc((total_sightings)))

##Table of users who left comments and behaviours and total sightings all in one table
bcferries_summary = bcferries %>% 
  dplyr::group_by(observer_email) %>% 
  dplyr::summarise(
    total_sightings = dplyr::n(),
    behaviour_count = sum(!is.na(behaviour)),
    comment_count = sum(!is.na(comments)),
    behaviour_and_comments = behaviour_count + comment_count,
    .groups = "drop"
  ) %>% 
  dplyr::arrange(desc(behaviour_and_comments))

###Attempting to remove duplicates###
unique(bcferries$sighting_date)

bcferries %>%
  dplyr::distinct(sighting_date) %>%
  nrow() #837 yay! 

##~~~~~~~~~~~~~~##

##alerts received by BC Ferries Vessels and individuals 

bcferries_alerts = main_dataset %>%
  dplyr::filter(
    alert_year == 2025, 
    delivery_successful == TRUE,
    user_email_recipient %in% unique(bcferries$observer_email) #only user emails from bcferries
  ) %>%
  dplyr::group_by(user_email_recipient) %>%
  dplyr::summarise(
    email_only = sum(email_sent & !sms_sent),
    sms_only = sum(sms_sent & !email_sent),
    both = sum(email_sent & sms_sent),
    total_alerts = dplyr::n(),
    .groups = "drop"
  ) %>% 
  dplyr::arrange(desc(total_alerts))

##add the users who got no alerts to the table

bcferries_users = bcferries %>% 
  dplyr::distinct(observer_email)

bcferries_alerts = bcferries_users %>% 
  dplyr::left_join(
    bcferries_alerts,
    by = c("observer_email" = "user_email_recipient"))

##now turn all the NA into 0 
bcferries_alerts = bcferries_alerts %>% 
  dplyr::mutate(
    dplyr::across(
      c(total_alerts, email_only, sms_only, both),
      ~tidyr::replace_na(.x, 0)
    )
  ) %>% 
  dplyr::arrange(desc(total_alerts))


##save this data as xlsx file
install.packages("writexl")
writexl::write_xlsx(
  list(
    "BC Ferries Sightings" = bcferries,##all sightings 
    "BC Ferries Observers" = bcferries_observers, ##all observer emails and species they reported
    "BC Ferries Summary" = bcferries_summary,  ##all observer emails and their detail stats 
    "BC Ferries Alerts" = bcferries_alerts #Alerts table
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/bcferries_sightings_Jan_30_2026.xlsx")


#Sandbox

##getting rid of the exact,range sighting_platform_name as they are incorrectly in this column for some sightings
# dplyr::mutate(
# sighting_platform_name = dplyr::case_when(
# stringr::str_detect(sighting_platform_name, "Range") ~ NA_character_,
# stringr::str_detect(sighting_platform_name, "Exact") ~ NA_character_,
# TRUE ~ sighting_platform_name
# )
# ) 



