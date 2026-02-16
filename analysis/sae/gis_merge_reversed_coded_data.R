# Responsible for merging the reverse coded (longitude, latitude) pair for administrative level 2 and 3 
# Main objective is to unmask the cluster locations by providing a lookup table for the target admin L2 and L3
library(dplyr)


root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

reverse_geo_coding_path <- "data/processed/CMGE71FL_reverse_geocoded.csv"
converted_orignal_cmge_path <- "data/processed/CMGE71FL_202601211755.csv"

rev_geo <- read.csv(reverse_geo_coding_path)
original_geo <- read.csv(converted_orignal_cmge_path)

# merge the datasets

merged_geo_data <- left_join(original_geo, rev_geo, by = "LATNUM")

# Extracted useful columns

extract_cols <- merged_geo_data %>% 
  select("DHSID", "DHSCLUST", "CCFIPS", "DHSCC", "ADM1DHS", "ADM1NAME", "DHSREGCO",
         "DHSREGNA", "URBAN_RURA", "LATNUM", "LONGNUM","ALT_GPS","ALT_DEM", "DATUM","SOURCE",
         "city", "county","state","distance", "formatted")

# SAVE THE NEW DATASET

write.csv(extract_cols, file="data/processed/gis_reverse_geo_data.csv")

