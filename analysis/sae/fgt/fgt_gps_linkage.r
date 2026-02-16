install.packages(c("sf", "dplyr", "haven", "survey"))
library(sf)
library(dplyr)


# loading of gps data

# DHS GPS clusters
gps <- st_read("CMGE7FL.shp")

# Keep only needed variables
gps <- gps %>%
  select(DHSCLUST, URBAN_RURA, geometry) %>%
  st_transform(32632)  # UTM Zone 32N (Cameroon)


# load sub-division boundaries

subdiv <- st_read("cameroon_subdivision.shp") %>%
  st_transform(32632)

# in the above code, we assume subdivision ID is "SUBDIV_ID" but corrections will be held accordingly


# Applying displacement buffers introduced by DHS in order to assert annonymity of records

gps_buffered <- gps %>%
  mutate(
    buffer_dist = ifelse(URBAN_RURA == "U", 2000, 5000)
  ) %>%
  st_buffer(dist = .$buffer_dist)


# Spatial join (buffer → polygon)

gps_joined <- st_join(
  gps_buffered,
  subdiv,
  join = st_intersects,
  left = TRUE
)


# Resolve multiple matches (modal assignment)
## because buffers may intersect multiple sub-divisions:

gps_subdiv <- gps_joined %>%
  st_drop_geometry() %>%
  group_by(DHSCLUST) %>%
  count(SUBDIV_ID) %>%
  slice_max(n, n = 1) %>%
  ungroup()


# Merge sub-division ID back to DHS microdata

dhs <- read_dta("CMHR7FL.DTA")

dhs <- dhs %>%
  left_join(
    gps_subdiv,
    by = c("hv021" = "DHSCLUST")
  ) %>%
  rename(sub_div_id = SUBDIV_ID)
