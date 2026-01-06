# Reporting Changes Documentation

## Overview
This document outlines the changes made to the whales-reporting codebase to support more flexible reporting and analysis.

## Changes Made

### 1. Flexible Multi-Year Period Comparison

**File**: `config.R`

**Previous behavior**: Hard-coded comparison between 2024 and 2025 only

**New behavior**: Configurable comparison across up to 5 years

**Configuration**:
```r
## In config.R, line 53-54
comparison_years = c(2024, 2025)  # Default: 2-year comparison
```

**To compare 5 years**, update to:
```r
comparison_years = c(2021, 2022, 2023, 2024, 2025)
```

**Implementation**: `monthly-dashboard-numbers.R` (lines 140-171)
- The `perc_inc` calculation now dynamically creates percentage increase columns for all consecutive years
- Column naming convention: `perc_inc_YYYY_to_YYYY` (e.g., `perc_inc_2024_to_2025`)
- Automatically handles any number of years specified in `comparison_years`

### 2. Condensed Source Entity Mapping

**File**: `config.R` (lines 38-50)

**Purpose**: Standardize report source entity names across the system

**Mapping**:
1. `"Ocean Wise"` → `"Ocean Wise Conservation Association"`
2. `"Orca Network"` → `"Orca Network via Conserve.io app"`
3. `"JASCO"` → `"JASCO"`
4. `"Whale Alert"` → `"Whale Alert Alaska"`
5. Any `"WhaleSpotter*"` → `"WhaleSpotter"` (catches all WhaleSpotter variations)
6. `"SMRU"` → `"SMRU"`

**Implementation**:
```r
source_entity_mapping = function(source_entity) {
  dplyr::case_when(
    source_entity == "Ocean Wise" ~ "Ocean Wise Conservation Association",
    source_entity == "Orca Network" ~ "Orca Network via Conserve.io app",
    source_entity == "JASCO" ~ "JASCO",
    source_entity == "Whale Alert" ~ "Whale Alert Alaska",
    stringr::str_detect(source_entity, "WhaleSpotter") ~ "WhaleSpotter",
    source_entity == "SMRU" ~ "SMRU",
    TRUE ~ source_entity
  )
}
```

**Applied to**:
- `main_dataset` (new column: `report_source_condensed`)
- `sightings_main` (new column: `report_source_condensed`)
- `alerts_main` (new column: `report_source_condensed`)

### 3. BCHN/SWAG Data Filtering

**File**: `config.R` (line 36)

**Purpose**: Exclude BCHN/SWAG data from all visualizations and reporting

**Configuration**:
```r
exclude_sources = c("BCHN/SWAG")
```

**Applied in**:
- `data-cleaning.R` (lines 282-286): Filters `main_dataset`
- `data-cleaning.R` (lines 548-551): Filters `sightings_main`
- `monthly-dashboard-numbers.R` (lines 86-88): Filters `alerts_detections`
- `monthly-dashboard-numbers.R` (line 178): Filters `detections_pre`

### 4. New Reporting Breakdowns

**File**: `data-cleaning.R` (lines 599-636)

#### 4.1 Sightings by Condensed Source Entity

**Dataset name**: `sightings_by_source`

**Description**: Monthly count of sightings grouped by condensed source entity

**Columns**:
- `year`: Sighting year
- `month`: Sighting month (1-12)
- `source`: Condensed source entity name
- `sightings_count`: Number of sightings

**Example usage**:
```r
# View sightings breakdown for 2024
sightings_by_source %>%
  filter(year == 2024) %>%
  arrange(month, source)

# Total sightings by source for 2024
sightings_by_source %>%
  filter(year == 2024) %>%
  group_by(source) %>%
  summarise(total = sum(sightings_count))
```

#### 4.2 Unique Notifications by Condensed Source Entity

**Dataset name**: `notifications_by_source`

**Description**: Monthly count of unique notifications (email OR SMS) grouped by condensed source entity

**Important**: A "unique notification" means one notification per user per sighting, regardless of delivery method(s)

**Columns**:
- `year`: Alert year
- `month`: Alert month (1-12)
- `source`: Condensed source entity name
- `unique_notifications`: Count of unique user-sighting combinations (one per user per sighting)
- `email_notifications`: Number of notifications sent via email
- `sms_notifications`: Number of notifications sent via SMS
- `both_email_and_sms`: Number of notifications sent via both methods

**Example usage**:
```r
# View notification breakdown for 2024
notifications_by_source %>%
  filter(year == 2024) %>%
  arrange(month, source)

# Total unique notifications by source for 2024
notifications_by_source %>%
  filter(year == 2024) %>%
  group_by(source) %>%
  summarise(
    total_unique = sum(unique_notifications),
    total_email = sum(email_notifications),
    total_sms = sum(sms_notifications)
  )

# Users who received both email AND SMS
notifications_by_source %>%
  filter(year == 2024) %>%
  summarise(total_both = sum(both_email_and_sms))
```

## Data Pipeline Flow

```
1. config.R
   ├── Define comparison_years
   ├── Define exclude_sources (BCHN/SWAG)
   └── Define source_entity_mapping function

2. data-cleaning.R
   ├── Filter main_dataset to exclude BCHN/SWAG
   ├── Add report_source_condensed to main_dataset
   ├── Filter sightings_main to exclude BCHN/SWAG
   ├── Add report_source_condensed to sightings_main
   ├── Add report_source_condensed to alerts_main
   ├── Create sightings_by_source breakdown
   └── Create notifications_by_source breakdown

3. monthly-dashboard-numbers.R
   ├── Filter alerts_detections to exclude BCHN/SWAG
   ├── Filter detections_pre to exclude BCHN/SWAG
   └── Create flexible year comparison (perc_inc)

4. metrics-analysis.R
   ├── Inherit comparison_years from config.R (supports 2-5 years)
   ├── Filter all comparison datasets to exclude BCHN/SWAG
   ├── Use condensed source names in visualizations
   └── Dynamically generate plots for any number of comparison years
```

## Migration Notes

### For Existing Code/Reports

1. **No breaking changes** to existing column names:
   - Original `report_source_entity` column remains unchanged
   - New `report_source_condensed` column added alongside

2. **Existing visualizations**:
   - All existing visualizations automatically filter out BCHN/SWAG
   - Historical data remains unchanged (read-only access)

3. **Year comparisons**:
   - Update any hard-coded year filters to use `comparison_years` from config
   - Dynamic column names in `perc_inc`: check for `perc_inc_*_to_*` pattern

### For New Reports

1. **Use condensed sources**:
   ```r
   # Use this for reporting
   df %>% group_by(report_source_condensed)

   # Instead of this
   df %>% group_by(report_source_entity)
   ```

2. **Use new breakdown datasets**:
   ```r
   # For sightings analysis
   sightings_by_source

   # For notification analysis
   notifications_by_source
   ```

3. **Configurable year ranges**:
   ```r
   # In config.R, adjust as needed:
   comparison_years = c(2023, 2024, 2025)  # 3-year comparison
   ```

### For metrics-analysis.R

**Important Changes**:

1. **Flexible year comparisons**:
   - Previously: Hard-coded to compare 2023 vs 2025
   - Now: Inherits `comparison_years` from config.R (supports 2-5 years)
   - Default fallback: `c(2023, 2025)` if config.R not loaded

2. **BCHN/SWAG filtering**:
   - All comparison datasets (`main_comparison`, `sightings_comparison`, `alerts_comparison`) automatically filter out BCHN/SWAG if `exclude_sources` exists

3. **Condensed source names**:
   - Source breakdown visualizations (Section 7) now use `report_source_condensed` instead of `report_source_entity`
   - Displays user-friendly names like "Ocean Wise Conservation Association" instead of "Ocean Wise"

4. **Dynamic plot generation**:
   - Source breakdown plots now dynamically adjust for any number of comparison years (2-5)
   - Plot height automatically scales based on number of years

**Usage**:
```r
# In config.R, set comparison years
comparison_years = c(2021, 2022, 2023, 2024, 2025)  # 5-year comparison

# Source files in order
source("config.R")
source("data-import.R")
source("data-cleaning.R")
source("metrics-analysis.R")  # Will automatically use 5-year comparison
```

## Testing

To verify the changes are working correctly:

```r
# 1. Check BCHN/SWAG is filtered out
main_dataset %>%
  filter(report_source_entity == "BCHN/SWAG") %>%
  nrow()  # Should return 0

# 2. Check condensed source mapping
main_dataset %>%
  distinct(report_source_entity, report_source_condensed) %>%
  arrange(report_source_condensed)

# 3. Check new breakdown datasets exist
nrow(sightings_by_source)  # Should return > 0
nrow(notifications_by_source)  # Should return > 0

# 4. Check year comparison flexibility
perc_inc  # Should show columns for all years in comparison_years

# 5. Test metrics-analysis.R with multiple years
comparison_years = c(2023, 2024, 2025)  # 3-year test
source("metrics-analysis.R")
# Should generate 3 stacked plots in source breakdown
```

## Questions or Issues?

Contact: Alex Mitchell (alex.mitchell@ocean.org)
Date: 2025-12-24
