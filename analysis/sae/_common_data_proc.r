library("foreign")
wd <- "G:/Workspaces/academics/masters_thesis/"
#wd <- "G:/Workspaces/academics/masters_thesis/data/dataset/dhs/cameroon/CM_2018_DHS_09092023_1923_174579/CMHR71SV/CMHR71FL.SAV"

setwd(wd)

#base paths
p_dataset_root <- paste0(wd, "data/dataset/")
p_dhs_data_root <- paste0(p_dataset_root, "dhs/")
p_gis_data_root <- paste0(p_dataset_root, "cameroon/gis/geojson/") # geojson files (administrative boundaries 1-3) # nolint: line_length_linter.
p_worldbank_wdi_root <- paste0(p_dataset_root, "worldbank/wdi/")


f <- paste0(p_dhs_data_root, "cameroon/CM_2018_DHS_09092023_1923_174579/CMHR71SV/CMHR71FL.SAV")
f

# dhs survey data
dhs_data_hr_2018 <- read.spss(f, to.data.frame = TRUE)
head(dhs_data_hr_2018)
View(dhs_data_hr_2018)

#   mutate(
#     wealth_index = as.numeric(wealth_index),
#     education_level = as.numeric(education_level),
#     living_standards_index = as.numeric(living_standards_index),
#     health_status = as.numeric(health_status)
#   )



