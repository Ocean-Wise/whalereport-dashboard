##Seaspan Anlaysis of users 
##author: Carly Green 
##Date Jan 26 2025

##run Config R, Data-import R, Data-cleaning R
##using the sightings_main table as it contains all sightings, not just sightings that sent alerts

##filtering for seaspan users which includes seaspan emails and some personal emails. Salesforce has 14 PEs 

seaspan_personal_emails = c(
  "skottym@yahoo.ca",
  "colinsands@yahoo.com",
  "reinhard44@shaw.ca",
  "ryansjames96@hotmail.com",
  "gbwilson4@hotmail.com"
)

seaspan_sightings = sightings_main %>% 
  dplyr::filter(
    stringr::str_detect(observer_email, 
                        stringr::regex("seaspan", ignore_case = TRUE)) |
      observer_email %in% seaspan_personal_emails
  )

unique(seaspan_sightings$observer_organization) #there are a few organizations but they all seem accurate to seaspan users 

seaspan_observers = seaspan_sightings %>%
  dplyr::group_by(
    observer = observer_email,
    species = species_name
  ) %>%
  dplyr::summarise(
    sightings_count = dplyr::n(),
    .groups = "drop"
  ) %>%
  dplyr::arrange(observer, species)

##only 4 sightings submitted in 2025 by seaspan users 

##looking at alerts now for Seaspan

seaspan_alerts = main_dataset %>%
  dplyr::filter(
    alert_year %in% 2018: 2025, 
    delivery_successful == TRUE,
    user_email_recipient %in% unique(seaspan_sightings$observer_email)
  ) %>%
  dplyr::group_by(user_email_recipient) %>%
  dplyr::summarise(
    total_alerts = dplyr::n(),
    email_only = sum(email_sent & !sms_sent),
    sms_only = sum(sms_sent & !email_sent),
    both = sum(email_sent & sms_sent),
    species_seen = dplyr::n_distinct(species_name),
    .groups = "drop"
  ) %>% 
  dplyr::arrange(desc(total_alerts))

seaspan_alerts_all <- main_dataset %>%
  dplyr::filter(
    alert_year %in% 2018:2025,
    delivery_successful == TRUE,
    user_email_recipient %in% unique(seaspan_sightings$observer_email)
  ) %>%
  dplyr::arrange(user_email_recipient, alert_user_created_at)

writexl::write_xlsx(
  list(
    "Seaspan Sightings" = seaspan_sightings, #sightings
    "seaspan observers" = seaspan_observers,   #users
    "Seaspan Alerts Summary" = seaspan_alerts,          #alert summary
    "Seaspan Alerts"   = seaspan_alerts_all   # summary stats per observer
  ),
  path = "C:/Users/CarlyGreen/OneDrive - Ocean Wise Conservation Association/Documents/Operations/RStudio/Data Requests/seaspan_sightings_jan_26_2026.xlsx"
)

