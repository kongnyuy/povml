# Preprose the cameroon DHS 2018 human recode dataset 
# we extract only attributes of interest to our work and enrich the dataset with geo-based information
# such as the administrative levels(1,2,3) and (long, latitude) pairs
library(dplyr)
library(haven)
library(foreign)
library(tidyverse)
library(labelled)
#library(ggplot2)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

dhs_HR_db_path_sta <- "data/raw/dhs/CMHR71FL.DTA"
#dhs_HR_db_path <- "data/dhs_cmr/CM_2018_DHS_09092023_1923_174579/CMHR71SV/CMHR71FL.SAV"


CMHR71FL_sta <- read_dta(dhs_HR_db_path_sta)  #stata  
#CMHR71FL <- read_spss(dhs_HR_db_path) #spss
#CMHR71FL <- read.spss(dhs_HR_db_path, use.value.labels = TRUE) #spss
#View(head(CMHR71FL))
#View(head(CMHR71FL_sta))



#ds = CMHR71FL$HV024
dsta = CMHR71FL_sta$hv024

#ggplot(data = dsta, aes(x = data$hv024)) + geom_bar()

#variables of interest
covariates = c("hv000", "hv001", "hv002", "hv004", "hv005", "hv021","hv022", "hv023","hv024", "hv025", "hv026", "hv040", 
               "hv107_01", "hv106_01", "hv109_01", "hv129_01", "hv101_01", "hv204", "hv201", "hv202", "hv206",
               "hv209", "hv212", "hv270", "hv271", "hv213", "hv214", "hv215", "hv226", "sb118_01", "ha57_01","hv205", "hv216", "hv219",
               "hv220", "hv238a", "hv009")

meaningful_names = c("cc_hv000", "cluster_id_hv001","household_number_hv002","ultimate_area_code_hv004", "household_sampling_weight_hv005",
                     "psu_hv021","sample_strata_error_hv022","sample_strata_design_hv023","region_hv024",
                     "residence_type_hv025", "place_of_residence_hv026", "cluster_altitude_hv040",
                     "highest_edu_level_hv107", "highest_edu_attained_hv106", "edu_attainment_hv109", "school_attend_status_hv129",
                     "relation_to_head_hv101", "time_to_water_hv204", "source_water_drink_hv201","source_water_no_drink_hv202", 
                     "has_electricity_hv206","has_refregerator_hv209", "has_car_hv212", "wealth_index_quantiles_hv270", "wealth_index_score_hv271"
                     ,"main_floor_material_hv213", "main_wall_material_hv214", "main_roof_material_hv215",
                     "cooking_fuel_hv226","malaria_rdt_sb118_01","anemia_level_ha57_01","toilet_facility_hv205",
                     "num_sleeping_rooms_hv216","sex_head_house_hv219", "age_head_house_hv220", "toilet_location_hv238a", "num_house_members_hv009")

#fragmented_db <- CMHR71FL_sta %>% select(all_of(covariates)) # %>% na.omit()
fragmented_db <- CMHR71FL_sta %>% select(all_of(covariates)) # %>% na.omit()



# Converting ordinal columns to factors -----------------------------------

fragmented_db <- labelled::to_factor(fragmented_db)



# save extracted data

write.csv(fragmented_db, file = "data/processed/dhs_hr_isolated_2018.csv")
 # ensuring persistence of transformed information
saveRDS(fragmented_db, file = "data/processed/dhs_hr_isolated_2018.rds")

# rename columns

renamed_frag_db <- fragmented_db %>% rename(
  cc_hv000 = hv000,
  cluster_id_hv001 = hv001,
  household_number_hv002 = hv002,
  ultimate_area_code_hv004 = hv004,
  household_sampling_weight_hv005 = hv005,
  psu_hv021 = hv021,
  sample_strata_error_hv022 = hv022,
  sample_strata_design_hv023 = hv023,
  region_hv024 = hv024, # region + (Douala, yaounde)
  residence_type_hv025 = hv025,  # strata variable (urban / rural)
  place_of_residence_hv026 = hv026,
  cluster_altitude_hv040 = hv040,
  highest_edu_level_hv107 = hv107_01,
  highest_edu_attained_hv106 = hv106_01,
  edu_attainment_hv109 = hv109_01,
  school_attend_status_hv129 = hv129_01,
  relation_to_head_hv101 = hv101_01,
  time_to_water_hv204 = hv204,
  source_water_drink_hv201 = hv201,
  source_water_no_drink_hv202 = hv202,
  has_electricity_hv206 = hv206,
  has_refregerator_hv209 = hv209,
  has_car_hv212 = hv212,
  wealth_index_quntiles_hv270 = hv270,
  wealth_index_score_hv271 = hv271, 
  main_floor_material_hv213 = hv213,
  main_wall_material_hv214 = hv214,
  main_roof_material_hv215 = hv215,
  cooking_fuel_hv226 = hv226,
  malaria_rdt_sb118_01 = sb118_01,
  anemia_level_ha57_01 = ha57_01,
  toilet_facility_hv200 = hv205,
  num_sleeping_rooms_hv216 = hv216,
  sex_head_house_hv219 = hv219, 
  age_head_house_hv220 = hv220,
  toilet_location_hv238a = hv238a,
  num_house_members_hv009 = hv009 
)

write.csv(renamed_frag_db, file = "data/processed/dhs_hr_isolated_2018_renamed.csv")
# ensuring persistence of transformed information
saveRDS(renamed_frag_db, file = "data/processed/dhs_hr_isolated_2018_renamed.rds")




# merge domain area information census + gis

#gis_data <- read.csv("data/processed/gis_reverse_geo_data.csv")
gis_data <- read.csv("data/processed/gis_reverse_geo_data_admin3_corrected.csv")

gis_data <- gis_data %>% mutate(cluster_id_hv001 = DHSCLUST)

# merge the survey data and the gis data (with meaningull names)

jdata <- inner_join(renamed_frag_db, gis_data, by = "cluster_id_hv001")
jdata <- jdata %>% rename(
  region = state,
  department = county
)

# save the new dataset

write.csv(jdata, file  = "data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")
# ensuring persistence of transformed information
saveRDS(jdata, file = "data/processed/main_meaningfull_names_dhs_hr_gis_2018.rds")

# merge the survey data and the gis data (with dhs variable naming convention)
gis_data <- gis_data %>% mutate(hv001 = cluster_id_hv001)
sdata <- inner_join(fragmented_db, gis_data, by = "hv001")
sdata <- sdata %>% rename(
  region = state,
  department = county
)



write.csv(sdata, file  = "data/processed/main_dhs_names_dhs_hr_gis_2018.csv")

saveRDS(sdata, file = "data/processed/main_dhs_names_dhs_hr_gis_2018.rds")



