# SharePoint Integration Setup

## Overview
This project now has Microsoft Graph integration to access SharePoint files directly from R, eliminating the need to download/upload shapefiles manually.

## Prerequisites

### 1. Install Required R Packages
```r
install.packages("Microsoft365R")
install.packages("sf")  # For reading/writing shapefiles
```

### 2. Verify SSO Permissions
Ensure your IT admin has approved the Microsoft Graph client app with these permissions:
- **Client ID**: `04b07795-8ddb-461a-bbee-02f9e1bf7b46`
- **Delegated Permissions**:
  - `Files.ReadWrite.All`
  - `Sites.ReadWrite.All`
  - `User.Read`

## Configuration

The SharePoint configuration is in `config.R` (lines 107-298). Key settings:

```r
sharepoint_site_url = "https://vamsc.sharepoint.com/sites/MMRP"
sharepoint_base_path = "Documents/General/Ocean Wise Data/Shapefiles"
```

## Usage

### First Time Setup
When you first use the SharePoint functions, a browser window will open for SSO authentication. After you authenticate, the token is cached locally for future sessions.

### Common Workflows

#### 1. List Available Shapefiles
```r
source("config.R")

# List all files in the Shapefiles folder
available_files = list_sharepoint_files()
print(available_files)

# List only .shp files
shapefiles = list_sharepoint_files(pattern = "\\.shp$")

# List files matching a pattern (e.g., MMR zones)
mmr_files = list_sharepoint_files(pattern = "MMR.*")
```

#### 2. Load a Shapefile Directly into R
```r
source("config.R")

# Load a specific shapefile (downloads to temp directory and reads)
# Specify base name without extension
mmr_zones = load_shapefile_from_sharepoint("MMR_zones")

# Plot the geometry
plot(sf::st_geometry(mmr_zones))

# Use in analysis
library(sf)
summary(mmr_zones)
```

#### 3. Load Multiple Shapefiles
```r
source("config.R")

# Define shapefiles you need (base names without extensions)
shapefiles_to_load = c("MMR_zones", "whale_habitat", "shipping_lanes")

# Load all at once
spatial_data = lapply(shapefiles_to_load, load_shapefile_from_sharepoint)
names(spatial_data) = shapefiles_to_load

# Access individual datasets
mmr_zones = spatial_data$MMR_zones
whale_habitat = spatial_data$whale_habitat
```

#### 4. Download Files to a Specific Location
```r
source("config.R")

# Download shapefile components to a local directory
# Note: Shapefiles have multiple files (.shp, .shx, .dbf, .prj, etc.)
download_shapefile_from_sharepoint(
  file_names = c("MMR_zones.shp", "MMR_zones.shx", "MMR_zones.dbf", "MMR_zones.prj"),
  local_dir = "./data/shapefiles",
  overwrite = TRUE
)
```

#### 5. Upload a Shapefile to SharePoint
```r
source("config.R")

# After creating/updating a shapefile, upload it
# This automatically uploads all related files (.shp, .shx, .dbf, etc.)
upload_shapefile_to_sharepoint("./output/updated_mmr_zones.shp")
```

## Typical Analysis Workflow

```r
# 1. Source the config file (includes SharePoint functions)
source("config.R")

# 2. Check what shapefiles are available
available = list_sharepoint_files(pattern = "\\.shp$")
print(available)

# 3. Load the shapefiles you need
zones = load_shapefile_from_sharepoint("MMR_zones")
habitat = load_shapefile_from_sharepoint("whale_habitat")

# 4. Perform your spatial analysis
library(sf)
library(dplyr)

# Example: Find zone areas
zones_with_area = zones %>%
  mutate(area_km2 = as.numeric(st_area(geometry)) / 1e6)

# 5. If you create new shapefiles, save and upload them
output_path = "./output/analysis_zones.shp"
st_write(zones_with_area, output_path, delete_dsn = TRUE)
upload_shapefile_to_sharepoint(output_path)
```

## Customization

To access different SharePoint folders, modify the configuration in `config.R`:

```r
# For a different folder, change this line:
sharepoint_base_path = "Documents/General/Ocean Wise Data/Other_Folder"

# Then reload
source("config.R")
```

## Troubleshooting

### Error: "Failed to connect to SharePoint"
- Verify your SSO permissions are approved by IT
- Check that you're connected to the internet
- Try clearing cached credentials: delete `~/.config/AzureR/` directory

### Error: "403 - Access Denied"
- Ensure the Microsoft Graph client app (ID: `04b07795-8ddb-461a-bbee-02f9e1bf7b46`) is approved
- Verify all three delegated permissions are granted
- Contact your IT admin to check tenant settings

### Files not found
- Verify the file exists: `list_sharepoint_files()`
- Check the path configuration in `config.R`
- Ensure you're using the base name without extension for `load_shapefile_from_sharepoint()`

## Benefits

1. **No Manual Downloads**: Access shapefiles directly without downloading to desktop
2. **Always Up-to-date**: Get the latest version from SharePoint automatically
3. **Save Disk Space**: Files stored in temp directory or loaded directly into memory
4. **Easy Collaboration**: Upload updated analysis results back to SharePoint
5. **Version Control**: SharePoint maintains file history

## Additional Resources

- [Microsoft365R documentation](https://github.com/Azure/Microsoft365R)
- [sf package documentation](https://r-spatial.github.io/sf/)
