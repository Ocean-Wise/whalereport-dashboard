# Whales Reporting Codebase Documentation

**Repository**: whales-reporting **Generated**: 2026-01-15 **Author**: Alex Mitchell

## Document Version

**Version**: 1.0 **Last Updated**: 2026-01-15 **Branch**: new-database **Generated for**: Alex Mitchell

------------------------------------------------------------------------

## Table of Contents

1.  [Summary](#summary)
2.  [Project Structure](#project-structure)
3.  [Data Flow Pipeline](#data-flow-pipeline)
4.  [Dataset Metadata](#dataset-metadata)
5.  [Column Derivation Reference](#column-derivation-reference)
6.  [Key Functions](#key-functions)
7.  [Visualization Library](#visualization-library)
8.  [Configuration Guide](#configuration-guide)
9.  [Data Quality Notes](#data-quality-notes)
10. [Troubleshooting](#troubleshooting)

------------------------------------------------------------------------

## Summary {#summary}

### Purpose

-   The whales-reporting project is an R-based ETL (evaluate - transform - load) pipeline that processes whale sighting and alert notification data from the Whale database (Azure based, MySQL).
-   It transforms raw operational data into analytical datasets and interactive visualizations for reporting on the Whale Report Alert System (WRAS).

### Core Functionality

-   **Extract**: Pulls 11 tables from MariaDB database
-   **Transform**: Joins, cleans, and aggregates into 5 analytical datasets
-   **Load**: Creates interactive Plotly charts and Leaflet maps
-   **Report**: Generates year-on-year comparisons and specialized reports

### Data Volume

-   **Alert records**: \~200,000+ delivery attempts
-   **Sightings**: \~15,000+ whale observations
-   **Users**: \~500+ active alert recipients
-   **Time span**: Sightings = 1931 - present. Alerts = 2019-present. Both are configurable

------------------------------------------------------------------------

## Project Structure {#project-structure}

### File Organization

```         
whales-reporting/
├── config.R                      # Database connection, parameters, helper functions
├── data-import.R                 # Extracts 11 raw tables from database
├── data-cleaning.R               # Core transformation pipeline (creates 5 main datasets)
├── data-visualization.R          # interactive visualizations
├── metrics-analysis.R            # Year-on-year comparison analytics
├── README.md                     # Project overview
│
├── funding-reports/
│   ├── quiet-sound-24.R         # US EEZ-specific reporting
│   └── vfpa-24-25.R             # Vancouver Fraser Port Authority reporting
│
└── z.archive/                    # Legacy code (not in use)
    ├── legacy-data-processing.R
    ├── legacy-monthly-dashboard-numbers.R
    └── legacy-viz-sandbox.R
```

### Technology Stack

| Component      | Technology            | Purpose                            |
|------------------|----------------------|---------------------------------|
| Language       | R 4.x                 | Statistical computing              |
| Database       | MariaDB               | Operational data store (read-only) |
| Data Wrangling | dplyr, tidyr, stringr | Transform and reshape data         |
| Date/Time      | lubridate, zoo        | Handle timestamps and periods      |
| Visualization  | plotly, leaflet       | Interactive charts and maps        |
| Spatial        | sf, suncalc           | Geographic analysis                |
| Database       | DBI, RMariaDB         | Database connectivity              |

------------------------------------------------------------------------

## Data Flow Pipeline {#data-flow-pipeline}

### Execution Order

``` r
# Step 1: Configuration
source("config.R")
# → Establishes database connection
# → Sets date ranges, filters, comparison years
# → Defines helper functions

# Step 2: Import
source("data-import.R")
# → Pulls 11 tables from database
# → Applies initial source_entity_mapping
# → Prints import summary

# Step 3: Transform
source("data-cleaning.R")
# → Creates main_dataset (alert-level)
# → Creates sightings_main (sighting-level)
# → Creates alerts_main (sighting-user level)
# → Creates aggregated breakdowns
# → Prints transformation summary

# Step 4: Visualize
source("data-visualization.R")
# → Generates 10 visualizations
# → Creates filtered viz datasets
# → Prints viz inventory

# Step 5: Analyze (optional)
source("metrics-analysis.R")
# → Year-on-year comparisons
# → Generates metrics tables
```

### Data Pipeline Diagram

```         
┌─────────────────┐
│  MariaDB        │
│  Database       │
└────────┬────────┘
         │
         ├──→ alert_user_raw       ┐
         ├──→ alert_raw            │
         ├──→ alert_type_raw       │
         ├──→ sighting_raw         │
         ├──→ report_raw           ├─→ data-import.R
         ├──→ user_raw             │
         ├──→ observer_raw         │
         ├──→ observer_type_raw    │
         ├──→ species_raw          │
         ├──→ dictionary_raw       │
         └──→ organization_raw     ┘
                   │
                   ↓
         ┌─────────────────┐
         │ data-cleaning.R │
         └────────┬────────┘
                  │
    ┌─────────────┼─────────────┐
    ↓             ↓             ↓
main_dataset  sightings_main  alerts_main
    │             │             │
    └─────────────┴─────────────┘
                  │
         ┌────────┴────────┐
         ↓                 ↓
  data-visualization.R  metrics-analysis.R
```

------------------------------------------------------------------------

## Dataset Metadata {#dataset-metadata}

### Dataset 1: `main_dataset`

**Created in**: `data-cleaning.R` (lines 160-356)

**Row Structure**: One row per successful alert delivery to a user for a specific sighting

#### Purpose

The central analytical dataset that combines alert delivery records with complete context about the sighting, recipient, and submitter. Used for understanding alert system performance and user engagement.

#### Column Inventory (100+ columns)

| Column Name | Data Type | Source | Derivation | Line |
|---------------|---------------|---------------|---------------|---------------|
| **Alert Delivery Columns** |  |  |  |  |
| alert_user_id | int | alert_user_raw | Direct | 58 |
| alert_user_created_at | datetime | alert_user_raw | Direct | 59 |
| alert_user_updated_at | datetime | alert_user_raw | Direct | 60 |
| recipient | chr | alert_user_raw | Direct (email or phone) | 61 |
| alert_id | int | alert_user_raw | Direct | 62 |
| user_id | int | alert_user_raw | Direct (FK to user) | 63 |
| status | chr | alert_user_raw | Direct (sent/failed/pending) | 64 |
| alert_type_id | int | alert_user_raw | Direct (FK to alert_type) | 65 |
| context | chr | alert_user_raw | Direct (current_location/preferred_area) | 66 |
| delivery_successful | logical | Derived | status == "sent" | 331 |
| delivery_methods | chr | Derived | paste(sort(unique(alert_type_name)), collapse=", ") | 348 |
| num_delivery_methods | int | Derived | n_distinct(alert_type_name) | 349 |
| sms_sent | logical | Derived | "sms" %in% alert_type_name | 350 |
| email_sent | logical | Derived | "email" %in% alert_type_name | 351 |
| inapp_sent | logical | Derived | "in_app" %in% alert_type_name | 352 |
| **Alert Metadata** |  |  |  |  |
| alert_created_at | datetime | alert_raw | LEFT JOIN on alert_id | 167 |
| alert_type_name | chr | alert_type_raw | LEFT JOIN on alert_type_id | 192 |
| **Sighting Information** |  |  |  |  |
| sighting_id | int | alert_raw | LEFT JOIN on alert_id | 167 |
| sighting_created_at | datetime | sighting_raw | LEFT JOIN on sighting_id | 171 |
| sighting_name | chr | sighting_raw | Direct | 83 |
| sighting_start | datetime | sighting_raw | Direct | 84 |
| sighting_finish | datetime | sighting_raw | Direct | 85 |
| sighting_species_id | int | sighting_raw | Direct (FK to species) | 86 |
| sighting_status | chr | sighting_raw | Direct | 87 |
| sighting_code | chr | sighting_raw | Direct | 88 |
| sighting_organization_id | int | sighting_raw | Direct (FK to organization) | 89 |
| **Report Details (Primary Report)** |  |  |  |  |
| report_id | int | report_raw | First report by sighting_date | 23 |
| report_created_at | datetime | report_raw | From primary report | 24 |
| report_sighting_date | datetime | report_raw | From primary report | 25 |
| report_observer_id | int | report_raw | From primary report (FK to observer) | 26 |
| report_species_id | int | report_raw | From primary report (FK to species) | 27 |
| report_latitude | numeric | report_raw | From primary report | 28 |
| report_longitude | numeric | report_raw | From primary report | 29 |
| report_count | int | report_raw | Number of animals | 30 |
| report_direction | chr | report_raw | Direction of travel | 31 |
| report_location_desc | chr | report_raw | Location description | 32 |
| report_comments | chr | report_raw | Observer comments | 33 |
| report_source | chr | report_raw | Data source type | 34 |
| report_modality | chr | report_raw | Detection method | 35 |
| report_source_type | chr | report_raw | Source classification | 36 |
| report_source_entity | chr | report_raw | Data provider name | 37 |
| report_source_condensed | chr | Derived | source_entity_mapping(report_source_entity) | 337 |
| report_confidence_id | int | report_raw | FK to dictionary | 38 |
| report_count_measure_id | int | report_raw | FK to dictionary | 39 |
| report_sighting_platform_id | int | report_raw | FK to dictionary | 40 |
| report_sighting_range_id | int | report_raw | FK to dictionary | 41 |
| report_ecotype_id | int | report_raw | FK to dictionary (for killer whales) | 42 |
| report_vessel_name | chr | report_raw | Vessel name if applicable | 43 |
| report_status | chr | report_raw | Report status | 44 |
| total_reports | int | Aggregated | Count of reports for this sighting_id | 181 |
| **Recipient (User) Information** |  |  |  |  |
| user_firstname_recipient | chr | user_raw | LEFT JOIN on user_id with suffix | 96 |
| user_lastname_recipient | chr | user_raw | Direct with suffix | 97 |
| user_email_recipient | chr | user_raw | Direct with suffix | 98 |
| user_phone_recipient | chr | user_raw | Direct with suffix | 99 |
| user_organization_recipient | chr | user_raw | Direct with suffix | 100 |
| user_auth0_id_recipient | chr | user_raw | Authentication ID with suffix | 101 |
| user_experience_recipient | chr | user_raw | Experience level with suffix | 102 |
| user_type_id_recipient | int | user_raw | FK to user_type with suffix | 103 |
| user_organization_id_recipient | int | user_raw | FK to organization with suffix | 104 |
| recipient_full_name | chr | Derived | paste(firstname, lastname) | 319 |
| recipient_org_name | chr | organization_raw | LEFT JOIN on organization_id | 218 |
| **Observer/Submitter Information** |  |  |  |  |
| observer_id | int | observer_raw | LEFT JOIN on report_observer_id | 197 |
| observer_user_id | int | observer_raw | FK to user (if registered) | 111 |
| observer_name | chr | observer_raw | Direct | 112 |
| observer_email | chr | observer_raw | Direct | 113 |
| observer_organization | chr | observer_raw | Direct | 114 |
| observer_phone | chr | observer_raw | Direct | 115 |
| observer_type_id | int | observer_raw | FK to observer_type | 116 |
| observer_type_name | chr | observer_type_raw | LEFT JOIN on observer_type_id | 123 |
| user_firstname_submitter | chr | user_raw | LEFT JOIN on observer_user_id with suffix | 213 |
| user_lastname_submitter | chr | user_raw | Direct with suffix | 213 |
| user_email_submitter | chr | user_raw | Direct with suffix | 213 |
| submitter_full_name | chr | Derived | Coalesce user name or observer_name | 320-324 |
| submitter_org_name | chr | organization_raw | LEFT JOIN on organization_id | 222 |
| **Species Information** |  |  |  |  |
| species_name | chr | species_raw | LEFT JOIN on report_species_id | 131 |
| species_scientific_name | chr | species_raw | Direct | 131 |
| species_category_id | int | species_raw | FK to category | 132 |
| species_subcategory_id | int | species_raw | FK to subcategory | 133 |
| **Dictionary Lookups** |  |  |  |  |
| confidence_name | chr | dictionary_raw | LEFT JOIN on report_confidence_id | 232 |
| count_measure_name | chr | dictionary_raw | LEFT JOIN on report_count_measure_id | 240 |
| sighting_platform_name | chr | dictionary_raw | LEFT JOIN on report_sighting_platform_id | 248 |
| sighting_range_name | chr | dictionary_raw | LEFT JOIN on report_sighting_range_id | 256 |
| ecotype_name | chr | dictionary_raw | LEFT JOIN on report_ecotype_id | 264 |
| **Derived Date/Time Columns** |  |  |  |  |
| alert_year | int | Derived | year(alert_user_created_at) | 307 |
| alert_month | int | Derived | month(alert_user_created_at) | 308 |
| alert_year_month | yearmon | Derived | as.yearmon(alert_user_created_at) | 309 |
| alert_date | date | Derived | as_date(alert_user_created_at) | 310 |
| sighting_year | int | Derived | year(sighting_start) | 311 |
| sighting_month | int | Derived | month(sighting_start) | 312 |
| sighting_year_month | yearmon | Derived | as.yearmon(sighting_start) | 313 |
| **Other Derived Columns** |  |  |  |  |
| vessel_name | chr | Derived | Alias for report_vessel_name | 325 |

#### Data Transformation Steps

1.  **Base Table Selection** (line 163): Start with `alert_user_clean`
2.  **Join Alert Metadata** (line 165): Add alert creation timestamp
3.  **Join Sighting Details** (line 170): Add sighting information
4.  **Join Primary Report** (line 174): Add earliest report details
5.  **Join Report Count** (line 179): Add total reports per sighting
6.  **Join Recipient** (line 184): Add user who received alert
7.  **Join Alert Type** (line 189): Add delivery method
8.  **Join Observer** (line 194): Add submitter details
9.  **Join Observer Type** (line 199): Add observer classification
10. **Join Species** (line 204): Add species information
11. **Join Submitter User** (line 209): Add submitter user details (if registered observer)
12. **Join Organizations** (lines 216-223): Add recipient and submitter org names
13. **Join Dictionary** (lines 228-265): Add 5 dictionary lookups
14. **Apply Filters** (lines 269-300): Date range, sources, test users, push notifications, deduplication
15. **Add Derived Columns** (lines 305-325): Date components, names, delivery flags
16. **Aggregate Alert Types** (lines 343-356): Group by sighting-user, aggregate delivery methods, keep one row

#### Filters Applied

| Filter | Logic | Line |
|---------------------------|------------------------|---------------------|
| Date Range | alert_user_created_at BETWEEN start_date AND end_date | 271-274 |
| Source Include | report_source_entity IN source_filter (if defined) | 277-280 |
| Source Exclude | report_source_entity NOT IN exclude_sources | 283-286 |
| Test Users | user_id NOT IN test_user_ids | 289-292 |
| Alert Types | alert_type_name != "push" | 295-296 |
| Deduplication | Remove exact duplicates | 299-300 |
| Successful Only | delivery_successful == TRUE | 344 |

#### Sample Use Cases

1.  **Alert Delivery Performance**: Count delivery success rates by alert_type_name
2.  **User Engagement**: Identify most active recipients by user_id
3.  **Source Analysis**: Compare sighting volume by report_source_entity
4.  **Temporal Trends**: Group by alert_year_month for time series
5.  **Geographic Analysis**: Plot by report_latitude/report_longitude

------------------------------------------------------------------------

### Dataset 2: `sightings_main`

**Created in**: `data-cleaning.R` (lines 388-559) **Row Structure**: One row per whale sighting event **Typical Row Count**: \~15,000+ **Primary Key**: `sighting_id`

#### Purpose

A comprehensive sighting-level dataset built directly from report_raw. Captures ALL whale sightings, regardless of whether they generated alerts. Used for understanding detection patterns, data quality, and observer engagement.

#### Key Difference from main_dataset

-   **Source**: Built from `report_raw`, not `main_dataset`
-   **Coverage**: Includes sightings that didn't generate alerts
-   **Granularity**: One row per sighting (not per alert delivery)

#### Column Inventory (35+ columns)

| Column Name | Data Type | Source | Derivation | Line |
|---------------|---------------|---------------|---------------|---------------|
| **Core Identifiers** |  |  |  |  |
| sighting_id | int | report_raw | Group key | 386 |
| report_id | int | report_raw | From earliest report | 514 |
| sighting_code | chr | sighting_raw | LEFT JOIN | 516 |
| **Temporal Information** |  |  |  |  |
| sighting_date | datetime | report_raw | From earliest report | 515 |
| sighting_year | int | Derived | year(sighting_date) | 540 |
| sighting_month | int | Derived | month(sighting_date) | 541 |
| sighting_year_month | yearmon | Derived | as.yearmon(sighting_date) | 542 |
| **Species Information** |  |  |  |  |
| species_name | chr | species_raw | LEFT JOIN on species_id | 517 |
| species_scientific_name | chr | species_raw | Direct | 518 |
| ecotype_name | chr | dictionary_raw | LEFT JOIN on ecotype_id, special handling for Killer whales | 519, 461-464 |
| **Location Information** |  |  |  |  |
| report_latitude | numeric | report_raw | From earliest report | 520 |
| report_longitude | numeric | report_raw | From earliest report | 521 |
| **Count Information** |  |  |  |  |
| report_count | int | report_raw | Number of animals | 522 |
| count_type | chr | dictionary_raw | LEFT JOIN on count_measure_id (after cleanup) | 523, 433-440 |
| **Observation Details** |  |  |  |  |
| observer_confidence | chr | dictionary_raw | LEFT JOIN on confidence_id | 524 |
| comments | chr | report_raw | Merged with additional_props | 525, 506-519 |
| behaviour | chr | dictionary_raw | Extracted from array, collapsed | 526, 478-503 |
| sighting_platform_name | chr | dictionary_raw | LEFT JOIN | 527 |
| **Source Information** |  |  |  |  |
| report_source_entity | chr | report_raw | Replaced NA with "Ocean Wise Conservation Association" | 528, 546 |
| report_source_type | chr | report_raw | Direct | 529 |
| report_modality | chr | report_raw | Direct (infrared, visual, hydrophone) | 530 |
| **Report Metadata** |  |  |  |  |
| total_reports | int | Aggregated | Count of reports for this sighting_id, coalesce NA to 1 | 531, 504-505 |
| **Observer Information** |  |  |  |  |
| observer_name | chr | observer_raw | LEFT JOIN | 532 |
| observer_email | chr | observer_raw/user_raw | Coalesce user_email, observer_email | 533, 508 |
| observer_organization | chr | observer_raw/user_raw | Coalesce user_organization, observer_organization | 534, 510 |
| observer_type_name | chr | observer_type_raw | LEFT JOIN | 535 |

#### Special Data Processing

**1. Behavior Array Extraction** (lines 478-503)

``` r
# Raw format: "{1,2,5}" (PostgreSQL array as string)
# Step 1: Extract all numeric IDs with regex
behaviour_ids = str_extract_all(behaviours, "\\d+")

# Step 2: Unnest to create one row per behavior
unnest(behaviour_ids, keep_empty = TRUE)

# Step 3: Join to dictionary for behavior names
LEFT JOIN dictionary on behaviour_ids

# Step 4: Group back and collapse
GROUP BY sighting_id
behaviour_names = paste(na.omit(unique(behaviour_name)), collapse = ", ")
```

**2. Count Measure Cleanup** (lines 433-440)

``` r
# Problem: Incorrect values 1 and 2 in database
# Solution: Recode to NA before joining
count_measure_id = case_when(
  count_measure_id == 1 ~ NA,
  count_measure_id == 2 ~ NA,
  TRUE ~ count_measure_id
)
```

**3. Comments Consolidation** (lines 506-519)

``` r
# Step 1: Filter out JSON objects in additional_props
additional_props = if_else(
  str_detect(additional_props, "^\\{"),  # Starts with {
  NA,
  additional_props
)

# Step 2: Merge with priority to comments
comments = case_when(
  !is.na(comments) & !is.na(additional_props) ~ paste(comments, additional_props, sep = " | "),
  !is.na(comments) ~ comments,
  !is.na(additional_props) ~ additional_props,
  TRUE ~ NA
)
```

**4. Observer Email/Organization Coalescing** (lines 508-510)

``` r
# Registered observers have user records, others don't
observer_email = coalesce(user_email, observer_email)
observer_organization = coalesce(user_organization, observer_organization)
```

**5. Killer Whale Ecotype Handling** (lines 461-464)

``` r
# Killer whales must have ecotype, set "Unknown" if missing
ecotype_name = case_when(
  species_name == "Killer whale" & is.na(ecotype_name) ~ "Unknown",
  TRUE ~ ecotype_name
)
```

**6. Source Entity Default** (line 546)

``` r
# Any NA source becomes Ocean Wise
report_source_entity = replace_na(report_source_entity, "Ocean Wise Conservation Association")
```

#### Data Transformation Steps

1.  **Base Selection** (lines 384-389): Get earliest report per sighting_id from report_raw
2.  **Join Sighting** (lines 408-410): Add sighting metadata
3.  **Join Species** (lines 413-415): Add species names
4.  **Join Observer** (lines 418-420): Add observer details
5.  **Join Observer Type** (lines 423-425): Add observer classification
6.  **Join User** (lines 428-430): Add registered user details (if observer is registered)
7.  **Clean Count Measure** (lines 433-440): Recode incorrect values
8.  **Join Count Measure** (lines 442-445): Add count type names
9.  **Join Confidence** (lines 448-451): Add confidence levels
10. **Join Ecotype** (lines 454-457): Add ecotype for killer whales
11. **Fix Killer Whale Ecotype** (lines 460-465): Default to "Unknown" if missing
12. **Join Reports Count** (lines 467-469): Add total reports per sighting
13. **Join Platform** (lines 472-475): Add sighting platform
14. **Extract Behaviors** (lines 478-503): Parse behavior array
15. **Consolidate Comments** (lines 506-519): Merge comments fields
16. **Fix Total Reports** (line 504): Coalesce NA to 1
17. **Coalesce Observer Fields** (lines 508-510): Merge user/observer data
18. **Select Columns** (lines 512-537): Choose final column set
19. **Add Date Components** (lines 539-542): Extract year, month
20. **Default Source Entity** (line 546): Replace NA with Ocean Wise
21. **Apply Source Exclusion** (line 548): Remove excluded sources
22. **Deduplicate** (line 559): Remove any duplicates

#### Filters Applied

| Filter         | Logic                                       | Line |
|----------------|---------------------------------------------|------|
| Source Exclude | report_source_entity NOT IN exclude_sources | 548  |
| Deduplication  | Remove exact duplicates                     | 559  |

#### Sample Use Cases

1.  **Detection Coverage**: Count sightings by report_source_entity
2.  **Species Distribution**: Group by species_name and ecotype_name
3.  **Geographic Patterns**: Plot by report_latitude/report_longitude
4.  **Observer Engagement**: Count by observer_name or observer_type_name
5.  **Temporal Patterns**: Group by sighting_year_month
6.  **Behavior Analysis**: Filter by behaviour column
7.  **Data Quality**: Check total_reports, observer_confidence

------------------------------------------------------------------------

### Dataset 3: `alerts_main`

**Created in**: `data-cleaning.R` (lines 579-603) **Row Structure**: One row per unique sighting-user notification **Typical Row Count**: \~50,000+ **Primary Keys**: `sighting_id`, `user_id` (composite)

#### Purpose

Aggregated view of successful alert deliveries at the sighting-user level. Shows which users received alerts for which sightings, regardless of whether they got email, SMS, or both. Used for understanding notification reach and user engagement.

#### Key Difference from main_dataset

-   **Source**: Built from `main_dataset`
-   **Granularity**: One row per sighting-user combination (main_dataset can have multiple rows per sighting-user if they received both email and SMS)
-   **Filter**: Only successful deliveries (`delivery_successful == TRUE`)

#### Column Inventory (16 columns)

| Column Name | Data Type | Source | Derivation | Line |
|---------------|---------------|---------------|---------------|---------------|
| **Primary Keys** |  |  |  |  |
| sighting_id | int | main_dataset | Group key | 564 |
| user_id | int | main_dataset | Group key | 564 |
| **Alert Metadata** |  |  |  |  |
| alert_id | int | main_dataset | first(alert_id) | 566 |
| alert_created_at | datetime | main_dataset | first(alert_created_at) | 567 |
| alert_user_created_at | datetime | main_dataset | first(alert_user_created_at) | 568 |
| **Sighting Information** |  |  |  |  |
| sighting_start | datetime | main_dataset | first(sighting_start) | 569 |
| species_name | chr | main_dataset | first(species_name) | 570 |
| report_source_entity | chr | main_dataset | first(report_source_entity) | 571 |
| report_latitude | numeric | main_dataset | first(report_latitude) | 572 |
| report_longitude | numeric | main_dataset | first(report_longitude) | 573 |
| context | chr | main_dataset | first(context) | 574 |
| **Temporal Components** |  |  |  |  |
| alert_year | int | main_dataset | first(alert_year) | 575 |
| alert_month | int | main_dataset | first(alert_month) | 576 |
| alert_year_month | yearmon | main_dataset | first(alert_year_month) | 577 |
| **Aggregated Delivery Information** |  |  |  |  |
| delivery_methods | chr | Aggregated | paste(sort(unique(alert_type_name)), collapse=", ") | 579 |
| num_delivery_methods | int | Aggregated | n_distinct(alert_type_name) | 580 |
| sms_sent | logical | Aggregated | "sms" %in% alert_type_name | 581 |
| email_sent | logical | Aggregated | "email" %in% alert_type_name | 582 |
| inapp_sent | logical | Aggregated | "in_app" %in% alert_type_name | 583 |

#### Data Transformation Steps

1.  **Filter** (line 563): Keep only successful deliveries from main_dataset
2.  **Group** (line 564): Group by sighting_id and user_id
3.  **Take First Values** (lines 566-577): Get first occurrence of descriptive fields
4.  **Aggregate Delivery Methods** (lines 579-583):
    -   Create comma-separated list of alert types
    -   Count distinct alert types
    -   Create boolean flags for each type
5.  **Drop Groups** (line 584): Ungroup for final dataset

#### Sample Use Cases

1.  **Notification Reach**: Count distinct user_id per sighting_id
2.  **Delivery Preference**: Analyze email_sent vs sms_sent patterns
3.  **Multi-channel Users**: Filter where num_delivery_methods \> 1
4.  **Source Performance**: Group by report_source_entity and count
5.  **Context Analysis**: Compare current_location vs preferred_area
6.  **Geographic Coverage**: Plot by report_latitude/report_longitude
7.  **Temporal Trends**: Group by alert_year_month

------------------------------------------------------------------------

### Dataset 4: `primary_reports`

**Created in**: `data-cleaning.R` (lines 15-45) **Row Structure**: One row per sighting (earliest report) **Typical Row Count**: \~15,000+ **Primary Key**: `sighting_id`

#### Purpose

A lookup table used internally during the cleaning process. Identifies the "primary" (earliest) report for each sighting to use as the canonical source of sighting details. This avoids duplicating information when multiple observers report the same sighting.

#### Column Inventory (27 columns)

| Column Name | Data Type | Source | Derivation | Line |
|---------------|---------------|---------------|---------------|---------------|
| sighting_id | int | report_raw | Group key | 17 |
| report_id | int | report_raw | From earliest report by sighting_date | 23 |
| report_created_at | datetime | report_raw | Direct | 24 |
| report_sighting_date | datetime | report_raw | Direct (sort key) | 25 |
| report_observer_id | int | report_raw | Direct (FK to observer) | 26 |
| report_species_id | int | report_raw | Direct (FK to species) | 27 |
| report_latitude | numeric | report_raw | Direct | 28 |
| report_longitude | numeric | report_raw | Direct | 29 |
| report_count | int | report_raw | Direct | 30 |
| report_direction | chr | report_raw | Direct | 31 |
| report_location_desc | chr | report_raw | Direct | 32 |
| report_comments | chr | report_raw | Direct | 33 |
| report_source | chr | report_raw | Direct | 34 |
| report_modality | chr | report_raw | Direct | 35 |
| report_source_type | chr | report_raw | Direct | 36 |
| report_source_entity | chr | report_raw | Direct | 37 |
| report_confidence_id | int | report_raw | Direct (FK to dictionary) | 38 |
| report_count_measure_id | int | report_raw | Direct (FK to dictionary) | 39 |
| report_sighting_platform_id | int | report_raw | Direct (FK to dictionary) | 40 |
| report_sighting_range_id | int | report_raw | Direct (FK to dictionary) | 41 |
| report_ecotype_id | int | report_raw | Direct (FK to dictionary) | 42 |
| report_vessel_name | chr | report_raw | Direct | 43 |
| report_status | chr | report_raw | Direct | 44 |

#### Data Transformation Steps

1.  **Filter** (line 16): Remove reports without sighting_id
2.  **Group** (line 17): Group by sighting_id
3.  **Sort** (line 18): Order by sighting_date (earliest first)
4.  **Select First** (line 19): Take first row per group
5.  **Ungroup** (line 20): Remove grouping
6.  **Select Columns** (lines 21-45): Rename columns with report\_ prefix

#### Usage

This dataset is joined into `main_dataset` at line 174-178 to provide report-level details for each alert. Not typically used for analysis directly.

------------------------------------------------------------------------

### Dataset 5: `reports_per_sighting`

**Created in**: `data-cleaning.R` (lines 48-51) **Row Structure**: One row per sighting **Typical Row Count**: \~15,000+ **Primary Key**: `sighting_id`

#### Purpose

A simple aggregation table that counts how many reports were submitted for each sighting. Used to understand which sightings were reported by multiple observers.

#### Column Inventory (2 columns)

| Column Name   | Data Type | Source     | Derivation             | Line |
|---------------|-----------|------------|------------------------|------|
| sighting_id   | int       | report_raw | Group key              | 50   |
| total_reports | int       | Aggregated | n() count within group | 51   |

#### Data Transformation Steps

1.  **Filter** (line 49): Remove reports without sighting_id
2.  **Group** (line 50): Group by sighting_id
3.  **Count** (line 51): Count rows per group

#### Usage

Joined into `main_dataset` at line 179-183 to add the `total_reports` column. Indicates data quality and observer engagement.

------------------------------------------------------------------------

### Dataset 6: `sightings_by_source`

**Created in**: `data-cleaning.R` (lines 595-605) **Row Structure**: One row per month-year-source combination **Typical Row Count**: \~500+ **Primary Keys**: `year`, `month`, `source` (composite)

#### Purpose

Monthly aggregation of sightings grouped by data source. Used for tracking contributions from different data providers over time.

#### Column Inventory (4 columns)

| Column Name     | Data Type | Source         | Derivation             | Line |
|-----------------|-----------|----------------|------------------------|------|
| year            | int       | sightings_main | sighting_year          | 597  |
| month           | int       | sightings_main | sighting_month         | 598  |
| source          | chr       | sightings_main | report_source_entity   | 599  |
| sightings_count | int       | Aggregated     | n() count within group | 602  |

#### Data Transformation Steps

1.  **Group** (lines 596-600): Group by year, month, source
2.  **Count** (line 602): Count sightings per group
3.  **Sort** (line 605): Arrange by year, month, source

#### Sample Use Cases

1.  **Source Trends**: Line chart of sightings_count over time by source
2.  **Contribution Analysis**: Compare sources by total sightings
3.  **Seasonal Patterns**: Group by month to see seasonal contributions

------------------------------------------------------------------------

### Dataset 7: `notifications_by_source`

**Created in**: `data-cleaning.R` (lines 610-624) **Row Structure**: One row per month-year-source combination **Typical Row Count**: \~300+ **Primary Keys**: `year`, `month`, `source` (composite)

#### Purpose

Monthly aggregation of alert deliveries grouped by data source. Shows the reach and delivery patterns of the alert system by data provider.

#### Column Inventory (7 columns)

| Column Name | Data Type | Source | Derivation | Line |
|---------------|---------------|---------------|---------------|---------------|
| year | int | alerts_main | alert_year | 613 |
| month | int | alerts_main | alert_month | 614 |
| source | chr | alerts_main | report_source_entity | 615 |
| unique_notifications | int | Aggregated | n() count of sighting-user pairs | 618 |
| email_notifications | int | Aggregated | sum(email_sent) | 619 |
| sms_notifications | int | Aggregated | sum(sms_sent) | 620 |
| both_email_and_sms | int | Aggregated | sum(email_sent & sms_sent) | 621 |

#### Data Transformation Steps

1.  **Filter** (line 611): Keep only email OR SMS (exclude in-app only)
2.  **Group** (lines 612-616): Group by year, month, source
3.  **Aggregate** (lines 617-622):
    -   Count rows (unique sighting-user pairs)
    -   Sum boolean flags for delivery types
4.  **Sort** (line 624): Arrange by year, month, source

#### Sample Use Cases

1.  **Delivery Volume**: Track unique_notifications over time
2.  **Channel Preference**: Compare email vs SMS usage
3.  **Multi-channel**: Analyze both_email_and_sms trends
4.  **Source Performance**: Compare notification volume by source

------------------------------------------------------------------------

### Visualization Datasets

**Created in**: `data-visualization.R` (lines 20-30)

Three filtered datasets are created for visualization functions:

#### `sightings_viz`

``` r
sightings_viz = sightings_main %>%
  filter(sighting_year %in% viz_years)
```

-   **Purpose**: Sightings for comparison years only
-   **Used by**: Most sighting-related visualizations

#### `alerts_viz`

``` r
alerts_viz = alerts_main %>%
  filter(alert_year %in% viz_years)
```

-   **Purpose**: Alerts for comparison years only
-   **Used by**: Alert volume and delivery visualizations

#### `main_viz`

``` r
main_viz = main_dataset %>%
  filter(alert_year %in% viz_years)
```

-   **Purpose**: Detailed alert data for comparison years
-   **Used by**: User and observer analytics

------------------------------------------------------------------------

## Column Derivation Reference {#column-derivation-reference}

### Date/Time Derivations

All date components are derived using lubridate and zoo packages:

``` r
# Year extraction
alert_year = lubridate::year(alert_user_created_at)
sighting_year = lubridate::year(sighting_start)

# Month extraction
alert_month = lubridate::month(alert_user_created_at)
sighting_month = lubridate::month(sighting_start)

# Year-month combination (for time series)
alert_year_month = zoo::as.yearmon(alert_user_created_at)
sighting_year_month = zoo::as.yearmon(sighting_start)

# Date only (no time)
alert_date = lubridate::as_date(alert_user_created_at)
```

### Name Derivations

``` r
# Recipient full name
recipient_full_name = paste(user_firstname_recipient, user_lastname_recipient)

# Submitter full name (with fallback)
submitter_full_name = case_when(
  !is.na(user_firstname_submitter) ~ paste(user_firstname_submitter, user_lastname_submitter),
  !is.na(observer_name) ~ observer_name,
  TRUE ~ "Unknown"
)

# Vessel name (alias)
vessel_name = report_vessel_name
```

### Boolean Flag Derivations

``` r
# Delivery success
delivery_successful = (status == "sent")

# Delivery method flags
sms_sent = "sms" %in% alert_type_name
email_sent = "email" %in% alert_type_name
inapp_sent = "in_app" %in% alert_type_name
```

### Aggregated String Derivations

``` r
# Comma-separated delivery methods
delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", ")
# Examples: "email", "sms", "email, sms"

# Comma-separated behaviors
behaviour_names = paste(na.omit(unique(behaviour_name)), collapse = ", ")
# Examples: "Feeding, Traveling", "Resting"
```

### Count Derivations

``` r
# Number of distinct delivery methods
num_delivery_methods = n_distinct(alert_type_name)
# Range: 1-3 (email, sms, in_app)

# Total reports per sighting
total_reports = n()  # Within group_by(sighting_id)
# Range: 1-20+ (most sightings have 1 report)
```

### Coalesced Derivations

``` r
# Observer email (prioritize user record)
observer_email = coalesce(user_email, observer_email)

# Observer organization (prioritize user record)
observer_organization = coalesce(user_organization, observer_organization)
```

### Source Entity Derivations

``` r
# Standardized source
report_source_condensed = source_entity_mapping(report_source_entity)

# Historical import extraction
historical_source_entity = extract_historical_source_entity(comments)
# Extracts "WhaleSpotter CMN" from "Historical Import | Source Entity: WhaleSpotter CMN | ..."
```

------------------------------------------------------------------------

## Key Functions {#key-functions}

### Configuration Functions (`config.R`)

#### `source_entity_mapping(source_entity)`

**Location**: config.R:40-53

**Purpose**: Standardize raw source entity names into consistent categories **Parameters**: - `source_entity` (chr): Raw source entity name from database

**Logic**:

``` r
case_when(
  str_detect(source_entity,"Ocean Wise") ~ "Ocean Wise Conservation Association",
  str_detect(source_entity, "Orca Network") ~ "Orca Network via Conserve.io app",
  str_detect(source_entity, "Acartia") ~ "Orca Network via Conserve.io app",
  str_detect(source_entity, "JASCO") ~ "JASCO",
  str_detect(source_entity, "Whale Alert Alaska") ~ "Whale Alert Alaska",
  str_detect(source_entity, "WhaleSpotter") ~ "WhaleSpotter",
  str_detect(source_entity, "SMRU") ~ "SMRU",
  str_detect(source_entity, "quiet") ~ source_entity,
  str_detect(source_entity, "BCHN/SWAG") ~ "BCHN/SWAG",
  TRUE ~ "Ocean Wise Conservation Association"
)
```

**Examples**: - "Ocean Wise" → "Ocean Wise Conservation Association" - "Acartia" → "Orca Network via Conserve.io app" - "WhaleSpotter CMN" → "WhaleSpotter" - NA → "Ocean Wise Conservation Association"

**Used in**: - data-import.R:34 (applied to report_raw) - data-cleaning.R:337 (creates report_source_condensed)

------------------------------------------------------------------------

#### `extract_historical_source_entity(comments)`

**Location**: config.R:56-63 **Purpose**: Extract source entity from historical import comments **Parameters**: - `comments` (chr): Comments field that may contain "Source Entity: XXX"

**Logic**:

``` r
extracted = str_extract(comments, "(?<=Source Entity: )[^|]+")
trimmed = str_trim(extracted)
if_else(is.na(trimmed) | trimmed == "", NA_character_, trimmed)
```

**Examples**: - "Historical Import \| Source Entity: WhaleSpotter CMN \| Sighting Code: B09" → "WhaleSpotter CMN" - "Regular comment" → NA - NA → NA

**Used in**: - data-import.R:30 (extracts from report_raw comments)

------------------------------------------------------------------------

#### `get_ocean_wise_colors(n)`

**Location**: config.R:77-82 **Purpose**: Get n colors from Ocean Wise brand palette **Parameters**: - `n` (int): Number of colors needed

**Logic**:

``` r
if (n > length(ocean_wise_palette)) {
  warning("Not enough Ocean Wise colors — some colors will be reused.")
}
rep(ocean_wise_palette, length.out = n)
```

**Returns**: Character vector of hex colors - Example: `get_ocean_wise_colors(3)` → `c("#FFCE34", "#A2B427", "#A8007E")`

**Used in**: - data-visualization.R (multiple visualizations) - metrics-analysis.R (comparison plots)

------------------------------------------------------------------------

### Cleaning Helper Functions (implied in data-cleaning.R)

#### Sighting Deduplication Pattern

**Location**: data-cleaning.R:15-45 **Purpose**: Get earliest report per sighting **Pattern**:

``` r
report_raw %>%
  filter(!is.na(sighting_id)) %>%
  group_by(sighting_id) %>%
  arrange(sighting_date) %>%
  slice(1) %>%
  ungroup()
```

**Used for**: Creating primary_reports lookup table

------------------------------------------------------------------------

#### Alert Type Aggregation Pattern

**Location**: data-cleaning.R:343-356 **Purpose**: Combine multiple delivery attempts into one row per sighting-user **Pattern**:

``` r
main_dataset %>%
  filter(delivery_successful == TRUE) %>%
  group_by(sighting_id, user_id) %>%
  mutate(
    delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", "),
    num_delivery_methods = n_distinct(alert_type_name),
    sms_sent = "sms" %in% alert_type_name,
    email_sent = "email" %in% alert_type_name,
    inapp_sent = "in_app" %in% alert_type_name
  ) %>%
  ungroup() %>%
  distinct(sighting_id, user_id, .keep_all = TRUE)
```

**Result**: One row per sighting-user with aggregated delivery information

------------------------------------------------------------------------

## Visualization Library {#visualization-library}

### Overview

All visualizations are defined as functions in `data-visualization.R` and automatically generated when the script is sourced.

### Visualization Inventory

| \# | Function | Variable | Type | Purpose |
|---------------|---------------|---------------|---------------|---------------|
| 1 | viz_1_map() | map_viz | Leaflet Map | Geographic distribution of sightings by year |
| 2 | viz_2_detections_line() | detections_line | Plotly Line | Monthly detections over time |
| 3 | viz_3_sighters_bar() | sighters_bar | Plotly Grouped Bar | Unique observers by month |
| 4 | viz_4_source_stacked() | source_stacked | Plotly Stacked Bar | Detections by data provider |
| 5 | viz_5_notifications_line() | notifications_line | Plotly Line | Alert volume over time |
| 6 | viz_6_users_bar() | users_bar | Plotly Grouped Bar | Unique recipients by month |
| 7 | viz_7_notification_type() | notification_type | Plotly Grouped Bar | Proximity vs Zone of Interest |
| 8 | viz_8_delivery_method() | delivery_method | Plotly Horizontal Bar | SMS vs Email comparison |
| 9 | viz_9_map_by_source() | map_by_source | Leaflet Map | Alerted sightings by source entity |
| 10 | viz_10_day_night_by_source() | day_night_by_source | Plotly Subplots | Day vs night detections by source |

------------------------------------------------------------------------

### Detailed Visualization Specs

#### 1. Map of Detections by Year (`viz_1_map`)

**Function**: `viz_1_map()` **Location**: data-visualization.R:34-86 **Data Source**: `sightings_viz` **Interactive**: Yes (Leaflet)

**Features**: - Circle markers colored by year - Popup with species, source, date - CartoDB Positron basemap - OpenSeaMap overlay - Legend showing years - Minimap for navigation

**Color Scheme**: First 5 years from Ocean Wise palette

**Sample Output**: - Purple circles: Most recent year - Blue circles: Previous year - Click circle: Shows sighting details

------------------------------------------------------------------------

#### 2. Detections Over Time (`viz_2_detections_line`)

**Function**: `viz_2_detections_line()` **Location**: data-visualization.R:90-146 **Data Source**: `sightings_viz` (monthly aggregation) **Interactive**: Yes (Plotly)

**Features**: - One line per comparison year - X-axis: Months (Jan-Dec) - Y-axis: Detection count - Hover shows exact values - Unified hover mode (vertical line)

**Color Scheme**: Dynamically assigned based on number of comparison years

------------------------------------------------------------------------

#### 3. Unique Sighters by Month (`viz_3_sighters_bar`)

**Function**: `viz_3_sighters_bar()` **Location**: data-visualization.R:150-206 **Data Source**: `main_viz` (observer deduplication) **Interactive**: Yes (Plotly)

**Features**: - Grouped bars by year - X-axis: Months (Jan-Dec) - Y-axis: Unique observer count - Purple for 2025, blue for other years

**Uses**: report_observer_id to count unique observers

------------------------------------------------------------------------

#### 4. Detections by Source (`viz_4_source_stacked`)

**Function**: `viz_4_source_stacked()` **Location**: data-visualization.R:210-251 **Data Source**: `sightings_viz` (monthly by source) **Interactive**: Yes (Plotly)

**Features**: - Stacked bars showing contribution - X-axis: Year-month (continuous) - Y-axis: Detection count - One color per source - Legend shows all sources

**Color Scheme**: Ocean Wise palette, one per unique source

------------------------------------------------------------------------

#### 5. Notifications Over Time (`viz_5_notifications_line`)

**Function**: `viz_5_notifications_line()` **Location**: data-visualization.R:255-311 **Data Source**: `alerts_viz` (monthly aggregation) **Interactive**: Yes (Plotly)

**Features**: - One line per comparison year - X-axis: Months (Jan-Dec) - Y-axis: Notification count - Shows alert delivery trends

**Similar to**: viz_2 but for alerts instead of sightings

------------------------------------------------------------------------

#### 6. Unique Alert Recipients by Month (`viz_6_users_bar`)

**Function**: `viz_6_users_bar()` **Location**: data-visualization.R:315-372 **Data Source**: `main_viz` (user deduplication) **Interactive**: Yes (Plotly)

**Features**: - Grouped bars by year - X-axis: Months (Jan-Dec) - Y-axis: Unique recipient count - Shows user growth

**Uses**: user_id to count unique recipients

------------------------------------------------------------------------

#### 7. Notification Type: Proximity vs Zone (`viz_7_notification_type`)

**Function**: `viz_7_notification_type()` **Location**: data-visualization.R:376-420 **Data Source**: `alerts_viz` (filtered to context types) **Interactive**: Yes (Plotly)

**Features**: - Grouped bars comparing context types - X-axis: Year - Y-axis: Notification count - Purple: Proximity alerts - Blue: Zone of Interest alerts

**Context Types**: - `current_location` → "Proximity" - `preferred_area` → "Zone of Interest"

------------------------------------------------------------------------

#### 8. Delivery Method: SMS vs Email (`viz_8_delivery_method`)

**Function**: `viz_8_delivery_method()` **Location**: data-visualization.R:424-471 **Data Source**: `main_viz` (filtered to email/SMS) **Interactive**: Yes (Plotly)

**Features**: - Horizontal grouped bars - X-axis: Notification count - Y-axis: Year - Blue: Email - Green: SMS

**Shows**: Channel preference trends

------------------------------------------------------------------------

#### 9. Map by Source Entity (`viz_9_map_by_source`)

**Function**: `viz_9_map_by_source(year = NULL)` **Location**: data-visualization.R:475-548 **Data Source**: `main_viz` (alerted sightings only) **Interactive**: Yes (Leaflet) **Parameters**: - `year` (optional): Filter to specific year

**Features**: - Circle markers colored by source entity - Aggregates all WhaleSpotter variants into "WhaleSpotter" - Popup shows original source name - Legend shows unique sources - Only includes sightings that generated alerts

**Color Scheme**: Ocean Wise palette, one per unique source

**Sample Usage**:

``` r
map_by_source = viz_9_map_by_source()       # All years
map_2025 = viz_9_map_by_source(year = 2025) # 2025 only
```

------------------------------------------------------------------------

#### 10. Day vs Night Detections by Source (`viz_10_day_night_by_source`)

**Function**: `viz_10_day_night_by_source(year = NULL)` **Location**: data-visualization.R:552-693 **Data Source**: `sightings_viz` **Interactive**: Yes (Plotly subplots) **Parameters**: - `year` (optional): Defaults to most recent comparison year

**Features**: - Paneled layout (one panel per source) - Grouped bars (Day vs Night) - X-axis: Months (Jan-Dec) - Y-axis: Detection count - Uses sunrise/sunset calculations based on coordinates - Yellow bars: Daytime detections - Blue bars: Nighttime detections

**Solar Calculation**: Uses `suncalc::getSunlightTimes()` to determine dawn/dusk for each sighting's lat/lon and date, then classifies sighting_datetime as Day or Night.

**Sample Usage**:

``` r
day_night = viz_10_day_night_by_source(year = 2025)
```

------------------------------------------------------------------------

### Using Visualizations

**View in RStudio**:

``` r
map_viz                 # Opens in Viewer pane
detections_line         # Opens in Viewer pane
```

**Export to HTML**:

``` r
htmlwidgets::saveWidget(map_viz, "map.html")
htmlwidgets::saveWidget(detections_line, "detections.html")
```

**Export to PNG** (requires orca):

``` r
plotly::orca(detections_line, "detections.png")
```

------------------------------------------------------------------------

## Configuration Guide {#configuration-guide}

### Database Connection

**Environment Variables** (set before running):

``` bash
export DB_NAME="whales_production"
export DB_HOST="db.example.com"
export DB_USER="readonly_user"
export DB_PASS="secure_password"
export SSL_CA="/path/to/ca-cert.pem"
```

**Connection Code** (config.R:10-19):

``` r
connect = DBI::dbConnect(
  RMariaDB::MariaDB(),
  dbname = Sys.getenv("DB_NAME"),
  host = Sys.getenv("DB_HOST"),
  port = 3306,
  user = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASS"),
  ssl.ca = Sys.getenv("SSL_CA")
)
```

**Important**: Always disconnect when done:

``` r
DBI::dbDisconnect(connect)
```

------------------------------------------------------------------------

### Date Range

**Default** (config.R:25-27):

``` r
start_date = lubridate::as_date("2019-01-01")
end_date = lubridate::today()
```

**Custom Range**:

``` r
start_date = lubridate::as_date("2024-01-01")
end_date = lubridate::as_date("2024-12-31")
```

**Applied to**: alert_user_created_at in main_dataset

------------------------------------------------------------------------

### Source Filtering

**Include Sources** (config.R:29-34):

``` r
source_filter = c(
  "Ocean Wise Conservation Association",
  "Orca Network via Conserve.io app",
  "WhaleSpotter",
  "JASCO",
  "SMRU",
  "Whale Alert Alaska"
)
```

**Exclude Sources** (config.R:36):

``` r
exclude_sources = c("BCHN/SWAG")
```

**To disable filtering**: Set to empty vector

``` r
source_filter = c()      # Include all sources
exclude_sources = c()    # Exclude nothing
```

------------------------------------------------------------------------

### Comparison Years

**Default** (config.R:67):

``` r
comparison_years = c(2024, 2025)
```

**Extended Comparison** (up to 5 years):

``` r
comparison_years = c(2021, 2022, 2023, 2024, 2025)
```

**Used by**: - data-visualization.R (filters viz datasets) - metrics-analysis.R (year-on-year comparisons)

------------------------------------------------------------------------

### Test User Exclusion

**Default** (config.R:61):

``` r
test_user_ids = c()
```

**After identifying test accounts**:

``` r
test_user_ids = c(123, 456, 789)  # Replace with actual IDs
```

**Applied to**: user_id in main_dataset

------------------------------------------------------------------------

### Color Palette

**Ocean Wise Brand Colors** (config.R:64-74):

``` r
ocean_wise_palette = c(
  "Sun"      = "#FFCE34",   # Yellow
  "Kelp"     = "#A2B427",   # Green
  "Coral"    = "#A8007E",   # Purple/Magenta
  "Anemone"  = "#354EB1",   # Blue
  "Ocean"    = "#005A7C",   # Dark Blue
  "Tide"     = "#5FCBDA",   # Cyan
  "Black"    = "#000000",   # Black
  "White"    = "#FFFFFF",   # White
  "Dolphin"  = "#B1B1B1"    # Gray
)
```

**Usage**:

``` r
ocean_wise_palette["Coral"]           # Get one color
get_ocean_wise_colors(3)              # Get 3 colors
```

------------------------------------------------------------------------

## Data Quality Notes {#data-quality-notes}

### Known Issues

#### 1. Incorrect count_measure_id Values

**Issue**: Values 1 and 2 are incorrect in database **Location**: data-cleaning.R:433-440 **Solution**: Recode to NA before joining to dictionary

``` r
count_measure_id = case_when(
  count_measure_id == 1 ~ NA,
  count_measure_id == 2 ~ NA,
  TRUE ~ count_measure_id
)
```

**Impact**: Affects sightings_main, some count types will be NA

------------------------------------------------------------------------

#### 2. Behavior Array Format

**Issue**: Behaviors stored as PostgreSQL array string: `{1,2,5}` **Location**: data-cleaning.R:478-503 **Solution**: Extract IDs with regex, unnest, join, collapse

**Process**: 1. Extract: `str_extract_all(behaviours, "\\d+")` 2. Unnest: Create one row per behavior ID 3. Join: Get behavior names from dictionary 4. Collapse: `paste(unique(behaviour_name), collapse = ", ")`

**Impact**: Works correctly but requires complex processing

------------------------------------------------------------------------

#### 3. Comments Field Inconsistency

**Issue**: Comments stored in both `comments` and `additional_props` **Location**: data-cleaning.R:506-519 **Solution**: Filter JSON objects, merge with priority to comments

**Pattern**:

``` r
# Step 1: Remove JSON objects from additional_props
additional_props = if_else(str_detect(additional_props, "^\\{"), NA, additional_props)

# Step 2: Merge
comments = case_when(
  !is.na(comments) & !is.na(additional_props) ~ paste(comments, additional_props, sep = " | "),
  !is.na(comments) ~ comments,
  !is.na(additional_props) ~ additional_props,
  TRUE ~ NA
)
```

**Impact**: Preserves all comment information

------------------------------------------------------------------------

#### 4. Observer vs User Email

**Issue**: Observers may not be registered users **Location**: data-cleaning.R:508 **Solution**: Coalesce to prioritize user record

``` r
observer_email = coalesce(user_email, observer_email)
```

**Impact**: Ensures email is available when possible

------------------------------------------------------------------------

#### 5. Missing Killer Whale Ecotype

**Issue**: Some killer whale sightings lack ecotype **Location**: data-cleaning.R:461-464 **Solution**: Default to "Unknown" for killer whales

``` r
ecotype_name = case_when(
  species_name == "Killer whale" & is.na(ecotype_name) ~ "Unknown",
  TRUE ~ ecotype_name
)
```

**Impact**: Ensures all killer whales have ecotype value

------------------------------------------------------------------------

#### 6. Historical Import Source Entities

**Issue**: Historical imports have source_entity in comments, not source_entity field **Location**: data-import.R:28-37 **Solution**: Extract from comments and apply before mapping

``` r
# Extract
historical_source_entity = extract_historical_source_entity(comments)

# Override
source_entity = if_else(!is.na(historical_source_entity), historical_source_entity, source_entity)

# Map
source_entity = source_entity_mapping(source_entity)
```

**Pattern**: `"Historical Import | Source Entity: WhaleSpotter CMN | ..."`

**Impact**: Correctly attributes historical data to original sources

------------------------------------------------------------------------

### Data Quality Checks

**Run these queries after loading data**:

``` r
# 1. Check for missing coordinates
sightings_main %>%
  filter(is.na(report_latitude) | is.na(report_longitude)) %>%
  count()

# 2. Check for missing species
main_dataset %>%
  filter(is.na(species_name)) %>%
  count()

# 3. Check delivery success rate
main_dataset %>%
  count(delivery_successful) %>%
  mutate(pct = n / sum(n) * 100)

# 4. Check source distribution
sightings_main %>%
  count(report_source_entity, sort = TRUE)

# 5. Check for duplicate sighting-user pairs in main_dataset
main_dataset %>%
  count(sighting_id, user_id) %>%
  filter(n > 1)  # Should be empty after aggregation

# 6. Check total reports distribution
sightings_main %>%
  count(total_reports) %>%
  arrange(desc(total_reports))

# 7. Check for killer whales without ecotype
sightings_main %>%
  filter(species_name == "Killer whale", is.na(ecotype_name)) %>%
  count()  # Should be 0
```

------------------------------------------------------------------------

## Troubleshooting {#troubleshooting}

### Common Issues

#### Issue 1: Database Connection Failed

**Error**: `Error: Can't connect to MySQL server`

**Causes**: 1. Environment variables not set 2. Wrong host/port 3. SSL certificate missing 4. Network connectivity

**Solutions**:

``` r
# Check environment variables
Sys.getenv("DB_NAME")
Sys.getenv("DB_HOST")
Sys.getenv("DB_USER")

# Test connection with verbose errors
connect = tryCatch({
  DBI::dbConnect(
    RMariaDB::MariaDB(),
    dbname = Sys.getenv("DB_NAME"),
    host = Sys.getenv("DB_HOST"),
    port = 3306,
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASS"),
    ssl.ca = Sys.getenv("SSL_CA")
  )
}, error = function(e) {
  print(e)
  return(NULL)
})

# Check if SSL cert exists
file.exists(Sys.getenv("SSL_CA"))
```

------------------------------------------------------------------------

#### Issue 2: Out of Memory

**Error**: `Error: cannot allocate vector of size X Gb`

**Causes**: 1. Too many rows in main_dataset 2. Too many columns retained 3. Multiple large datasets in memory

**Solutions**:

``` r
# 1. Reduce date range
start_date = lubridate::as_date("2024-01-01")  # Instead of 2019

# 2. Clear unnecessary objects
rm(alert_user_raw, alert_raw, report_raw)  # After creating main_dataset
gc()  # Force garbage collection

# 3. Select fewer columns
main_dataset = main_dataset %>%
  select(sighting_id, user_id, alert_year, species_name, ...)  # Only what you need

# 4. Check memory usage
pryr::object_size(main_dataset)
pryr::mem_used()
```

------------------------------------------------------------------------

#### Issue 3: Visualizations Not Displaying

**Error**: Blank viewer pane or "Figure is too large"

**Causes**: 1. Too many data points 2. RStudio viewer size issue 3. Plotly not installed

**Solutions**:

``` r
# 1. Filter to fewer years
comparison_years = c(2024, 2025)  # Instead of 5 years

# 2. Open in browser instead of viewer
detections_line %>% plotly::plotly_build() %>% htmltools::browsable()

# 3. Export to HTML and open
htmlwidgets::saveWidget(detections_line, "/tmp/detections.html")
browseURL("/tmp/detections.html")

# 4. Reinstall plotly
install.packages("plotly")
```

------------------------------------------------------------------------

#### Issue 4: Missing Data in Visualizations

**Error**: Blank charts or "No data to display"

**Causes**: 1. Filters too restrictive 2. No data for comparison years 3. source_filter excluding all sources

**Solutions**:

``` r
# 1. Check if data exists
nrow(sightings_viz)  # Should be > 0
nrow(alerts_viz)     # Should be > 0

# 2. Check comparison years
unique(sightings_main$sighting_year)  # Available years
comparison_years  # Requested years

# 3. Disable filters temporarily
source_filter = c()
exclude_sources = c()

# 4. Check date range
range(main_dataset$alert_user_created_at)
c(start_date, end_date)
```

------------------------------------------------------------------------

#### Issue 5: Slow Performance

**Symptom**: Script takes \> 10 minutes to run

**Causes**: 1. Large date range 2. Many comparison years 3. Slow database connection 4. Complex visualizations

**Solutions**:

``` r
# 1. Profile the code
system.time({
  source("data-import.R")
})
system.time({
  source("data-cleaning.R")
})

# 2. Reduce scope
start_date = lubridate::as_date("2024-01-01")
comparison_years = c(2024, 2025)

# 3. Skip visualizations during testing
# Comment out: source("data-visualization.R")

# 4. Use sampling for testing
main_dataset_sample = main_dataset %>% slice_sample(n = 10000)
```

------------------------------------------------------------------------

#### Issue 6: Incorrect Source Entities

**Symptom**: WhaleSpotter CMN showing as "Ocean Wise Conservation Association"

**Cause**: Historical import source not extracted

**Solution**: Ensure `extract_historical_source_entity()` is working

``` r
# Test the function
test_comment = "Historical Import | Source Entity: WhaleSpotter CMN | Sighting Code: B09"
extract_historical_source_entity(test_comment)
# Should return: "WhaleSpotter CMN"

# Check report_raw
report_raw %>%
  filter(str_detect(comments, "Historical Import")) %>%
  select(comments, source_entity) %>%
  head()

# Verify mapping is applied
report_raw %>%
  filter(str_detect(source_entity, "WhaleSpotter")) %>%
  count(source_entity)
```

------------------------------------------------------------------------

#### Issue 7: Duplicate Rows in main_dataset

**Symptom**: More rows than expected in main_dataset

**Cause**: Aggregation step failed or not applied

**Solution**: Check for duplicates and re-run aggregation

``` r
# Check for duplicates
main_dataset %>%
  count(sighting_id, user_id) %>%
  filter(n > 1)

# If duplicates found, re-run aggregation manually
main_dataset = main_dataset %>%
  filter(delivery_successful == TRUE) %>%
  group_by(sighting_id, user_id) %>%
  mutate(
    delivery_methods = paste(sort(unique(alert_type_name)), collapse = ", "),
    num_delivery_methods = n_distinct(alert_type_name),
    sms_sent = "sms" %in% alert_type_name,
    email_sent = "email" %in% alert_type_name,
    inapp_sent = "in_app" %in% alert_type_name
  ) %>%
  ungroup() %>%
  distinct(sighting_id, user_id, .keep_all = TRUE)

# Verify
main_dataset %>%
  count(sighting_id, user_id) %>%
  filter(n > 1)  # Should be empty
```

------------------------------------------------------------------------

### Debug Mode

**Enable verbose output**:

``` r
# Add to config.R
options(verbose = TRUE)
options(warn = 1)  # Show warnings immediately

# Add to start of each script
cat("\n=== Starting", basename(sys.frame(1)$ofile), "===\n")

# Add checkpoints
cat("✓ Database connected\n")
cat("✓ Data imported\n")
cat("✓ Main dataset created:", nrow(main_dataset), "rows\n")
```

------------------------------------------------------------------------

### Getting Help

**Resources**: 1. **REPORTING_CHANGES.md**: Recent enhancements and changes 2. **README.md**: Project overview and quick start 3. **This document**: Comprehensive reference

**Contact**: Alex Mitchell ([alex\@oceanwise.ca](mailto:alex@oceanwise.ca){.email})

------------------------------------------------------------------------

## Appendix: Complete Workflow Example

### Standard Analysis Workflow

``` r
# ============================================
# Whales Reporting Analysis
# Standard Workflow Example
# ============================================

# Step 1: Set working directory
setwd("/path/to/whales-reporting")

# Step 2: Configure parameters (before sourcing config.R)
# Edit these in config.R or override here:
start_date = lubridate::as_date("2024-01-01")
end_date = lubridate::as_date("2024-12-31")
comparison_years = c(2023, 2024)

# Step 3: Load configuration and connect to database
source("config.R")

# Step 4: Import raw data
source("data-import.R")
# → Creates: alert_user_raw, alert_raw, sighting_raw, report_raw, etc.

# Step 5: Clean and transform data
source("data-cleaning.R")
# → Creates: main_dataset, sightings_main, alerts_main, breakdowns

# Step 6: Generate visualizations
source("data-visualization.R")
# → Creates: map_viz, detections_line, etc.

# Step 7: View visualizations
map_viz                    # Interactive map
detections_line            # Time series
map_by_source              # Map by source entity

# Step 8: Export visualizations
htmlwidgets::saveWidget(map_viz, "~/Downloads/map_2024.html")
htmlwidgets::saveWidget(detections_line, "~/Downloads/detections_2024.html")

# Step 9: Create summary tables
source_summary = sightings_main %>%
  filter(sighting_year == 2024) %>%
  count(report_source_entity, name = "Sightings") %>%
  left_join(
    alerts_main %>%
      filter(alert_year == 2024) %>%
      count(report_source_entity, name = "Alerts"),
    by = "report_source_entity"
  ) %>%
  mutate(
    Alerts = replace_na(Alerts, 0),
    `Conversion Rate` = round(Alerts / Sightings * 100, 1)
  ) %>%
  arrange(desc(Sightings))

# View table
print(source_summary)

# Export to CSV
write.csv(source_summary, "~/Downloads/source_summary_2024.csv", row.names = FALSE)

# Step 10: Disconnect from database
DBI::dbDisconnect(connect)

# Done!
```

------------------------------------------------------------------------

### Custom Analysis Example: Top Engaged Users

``` r
# Identify most engaged alert recipients in 2024

top_users = main_dataset %>%
  filter(alert_year == 2024, delivery_successful == TRUE) %>%
  group_by(user_id, recipient_full_name, user_email_recipient) %>%
  summarise(
    total_alerts = n(),
    unique_sightings = n_distinct(sighting_id),
    email_only = sum(email_sent & !sms_sent),
    sms_only = sum(sms_sent & !email_sent),
    both = sum(email_sent & sms_sent),
    species_seen = n_distinct(species_name),
    first_alert = min(alert_user_created_at),
    last_alert = max(alert_user_created_at),
    days_active = as.numeric(difftime(max(alert_user_created_at), min(alert_user_created_at), units = "days")),
    .groups = "drop"
  ) %>%
  arrange(desc(total_alerts)) %>%
  head(20)

# View
print(top_users, n = 20)

# Export
write.csv(top_users, "~/Downloads/top_users_2024.csv", row.names = FALSE)
```

------------------------------------------------------------------------

### Custom Analysis Example: Seasonal Patterns

``` r
# Analyze seasonal sighting patterns by species

seasonal_patterns = sightings_main %>%
  filter(sighting_year >= 2022) %>%
  mutate(
    season = case_when(
      sighting_month %in% c(12, 1, 2) ~ "Winter",
      sighting_month %in% 3:5 ~ "Spring",
      sighting_month %in% 6:8 ~ "Summer",
      sighting_month %in% 9:11 ~ "Fall"
    )
  ) %>%
  count(species_name, season) %>%
  pivot_wider(names_from = season, values_from = n, values_fill = 0) %>%
  mutate(Total = Winter + Spring + Summer + Fall) %>%
  arrange(desc(Total))

# View
print(seasonal_patterns, n = 20)

# Create visualization
season_plot = sightings_main %>%
  filter(sighting_year >= 2022, species_name %in% c("Humpback whale", "Killer whale", "Grey whale")) %>%
  mutate(
    season = factor(
      case_when(
        sighting_month %in% c(12, 1, 2) ~ "Winter",
        sighting_month %in% 3:5 ~ "Spring",
        sighting_month %in% 6:8 ~ "Summer",
        sighting_month %in% 9:11 ~ "Fall"
      ),
      levels = c("Winter", "Spring", "Summer", "Fall")
    )
  ) %>%
  count(species_name, season) %>%
  plotly::plot_ly(
    x = ~season,
    y = ~n,
    color = ~species_name,
    type = "bar"
  ) %>%
  plotly::layout(
    barmode = "group",
    title = "Seasonal Sighting Patterns (2022-Present)",
    xaxis = list(title = "Season"),
    yaxis = list(title = "Number of Sightings")
  )

season_plot
```

------------------------------------------------------------------------
