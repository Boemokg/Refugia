# _targets.R
library(readxl)
library(dplyr)
library(sf)

# Define the targets pipeline
list(
  # Target for reading and processing the Excel file
  tar_target(
    raw_data,
    read_excel("/2022-08-11_064413137-BRAHMSOnlineData.xlsx"),
    format = "file"  # Mark as file type to enable caching and dependency management
  ),
  
  # Target for cleaning the data and filtering necessary columns
  tar_target(
    cleaned_data,
    raw_data %>%
      as_tibble() %>%
      filter(!is.na(Longitude)) %>%
      filter(!is.na(LatLongAccuracy)) %>%
      filter(!is.na(QDS)) %>%
      filter(LatLongAccuracy %in% c("100m", "10k", "1k", "50m", "250m", "5k",
                                    "10m", "2k", "1000m", "0.25k", "5m", "1/4g",
                                    "1/4", "¼dg", "04/01", "1/4dg"))
  ),
  
  # Target to convert the data into an sf object (spatial data)
  tar_target(
    spatial_data,
    st_as_sf(cleaned_data, coords = c("Longitude", "Latitude"), crs = 4326)
  )
)
