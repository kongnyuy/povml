library("tidyverse")
library("foreign")
library("survey")
library("sf")
library("emdi")
library(ggplot2)
library(stringr)
library(dplyr)





root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

# Load the data
#data <- read.csv("data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")
data <- read.csv("data/processed/main_dhs_names_dhs_hr_gis_2018.csv")


# Definition of the dependent variable (monetary indicator)
#dependent_var <- "wealth_index_continues_hv271"
dependent_var <- "hv271"


# Definition of the independent variables for each dimension
# independent_vars <- c("education_level", "living_standards_index", "health_status")

# independent_vars <- c("highest_edu_attained_hv106", "highest_edu_level_hv107","edu_attainment_hv109", #education_index
#      "has_electricity_hv206","has_refregerator_hv209","main_floor_material_hv213", #living_standards_index
#     "wealth_index_continues_hv271" # wealth and employment index    
#     )

independent_vars <- c("hv106_01","hv005","hv107_01", "hv023" , # "hv109_01", #education_index
     "hv206","hv209","hv213","hv201", "hv024", "hv025" #living_standards_index
    #,"hv271" # wealth and employment index    
    )



ivars = c("cluster_id_hv001","ultimate_area_code_hv004", "household_sampling_weight_hv005"
                     ,"sample_strata_design_hv023" # NB remmoved because of correlation "sample_strata_error_hv022" 
                     ,"region_hv024","residence_type_hv025", "place_of_residence_hv026", "cluster_altitude_hv040" # location attributes (region hv024, ...)
                     ,"highest_edu_level_hv107", "highest_edu_attained_hv106", "edu_attainment_hv109", "school_attend_status_hv129" # educational attributes | watchout for high correlation
                     ,"time_to_water_hv204", "source_water_drink_hv201","source_water_no_drink_hv202" # health attributes
                     ,"has_electricity_hv206","has_refregerator_hv209", "has_car_hv212","cooking_fuel_hv226","toilet_facility_hv205", "toilet_location_hv238" #living_standards_index
                      ,"wealth_index_quantiles_hv270", "wealth_index_score_hv271" # classification
                     ,"main_floor_material_hv213", "main_wall_material_hv214", "main_roof_material_hv215" #living_standards_index -> infrastructure                     
                     ,"malaria_rdt_sb118_01","anemia_level_ha57_01" #health attributes
                     ,"num_house_members_hv009","num_sleeping_rooms_hv216" # person-to-room ratio (ppr) = if(p/r) > 1 => overcrowed | ppr > 1.5 => severely overcrowed | ppr > 3 => critical overcrowding
                     ,"sex_head_house_hv219", "age_head_house_hv220" #household attributes                     
                     )


meaningful_names = c("cc_hv000", "cluster_id_hv001","household_number_hv002","ultimate_area_code_hv004", "household_sampling_weight_hv005",
                     "psu_hv021","sample_strata_error_hv022","sample_strata_design_hv023","region_hv024",
                     "residence_type_hv025", "place_of_residence_hv026", "cluster_altitude_hv040",
                     "highest_edu_level_hv107", "highest_edu_attained_hv106", "edu_attainment_hv109", "school_attend_status_hv129",
                     "relation_to_head_hv101", "time_to_water_hv204", "source_water_drink_hv201","source_water_no_drink_hv202", 
                     "has_electricity_hv206","has_refregerator_hv209", "has_car_hv212", "wealth_index_quantiles_hv270", "wealth_index_score_hv271"
                     ,"main_floor_material_hv213", "main_wall_material_hv214", "main_roof_material_hv215",
                     "cooking_fuel_hv226","malaria_rdt_sb118_01","anemia_level_ha57_01","toilet_facility_hv205",
                     "num_sleeping_rooms_hv216","sex_head_house_hv219", "age_head_house_hv220", "toilet_location_hv238", "num_house_members_hv009")

# Prepare the data for EBP estimation
#ebp_data <- data %>%
    #select(all_of(c(dependent_var, independent_vars))) %>%
#    na.omit()

#impute missing values

fn_most_common_str <- function(arr) {
  freq_table =  sort(table(arr[!is.na(arr)]), decreasing = TRUE)
  return(names(freq_table)[1])
}



ebp_data_imputed <- data %>% mutate(
  #hv107_01 = ifelse( is.na(hv107_01), mean(hv107_01, na.rm = TRUE), hv107_01)
  hv107_01 = ifelse( is.na(hv107_01),0, hv107_01), #edu level
  hv129_01 = ifelse(is.na(hv129_01), 0, hv129_01), # school attendance
  hv202 = ifelse(is.na(hv202), 96, hv202), # source of drinking water factor(other) = 96
  hv201 = ifelse(is.na(hv201), hv202, hv201) # source of drinking water
  
)


model_formula_str = paste(dependent_var, "~", paste(independent_vars, collapse = " + "))
# model: "wealth_index_continues_hv271 ~ highest_edu_attained_hv106 + highest_edu_level_hv107 + edu_attainment_hv109 + has_electricity_hv206 + has_refregerator_hv209 + main_floor_material_hv213 + wealth_index_continues_hv271"


#fileter ebp_data to usefull set of columns

ebp_data_small <- ebp_data_imputed %>% select(
  all_of(c(dependent_var, independent_vars,"hv202","hv005", "department","city", "region","hv000", "ADMIN3"))
) %>% na.omit()


View((ebp_data_small))

tab = table(ebp_data_small$hv202)

departs = unique(ebp_data_small$department)
subdivs = unique(ebp_data_small$ADMIN3)

ggplot(data = ebp_data_small, aes(x = hv202)) + geom_bar(aes(fill = hv202)) + labs(title = "Distribution of Source of non-drinking water", x = "Source of non-drinking water", y = "count" )

# Direct estimation
# ---------------------

# Head Count Ratio thresshold: 60% of median(hv271)

y_thresshold = 0.6 * median(ebp_data_small$hv271)

m = median(ebp_data_small$hv271)



model_direct_admin3 <- emdi::direct(y = dependent_var,
                                    smp_data = ebp_data_small, 
                                    weights = "hv005", 
                                    threshold = m,
                                    smp_domains = "ADMIN3",
                                    design = "hv023",
                                    var = FALSE,
                                    na.rm = TRUE
                                    )



# Custom survey sampling inorder to satisfy the tool
# create a sample data for the survey
# ----------------
dhs_cluster_size = 25
no_admin2_cm = 58
popul_size = nrow(ebp_data_small)

sample_size = dhs_cluster_size * no_admin2_cm

inclusion_prop = sample_size / popul_size # n/N 

sampling_weight = 1 / inclusion_prop # 1 / pi_k => N/n

samples_data = list()


# ----- end of custom survey sampling ------------

#inspired by booStraping using SRS with replacement and assuming similar distribution
# between the samples and the population
#for( i in 1:sample_size) {
sample = slice_sample(ebp_data_small, n = sample_size, replace = TRUE)
  #samples_data[i] = sample
#}





# REMOVING the department compound word spacing to match map NAME_2
library(stringr)


# MODIFY ADMIN2(department) NAMES TO MATCH THE GEOJSON DATASET GIS MAPS
ebp_data_small <- ebp_data_small  %>% mutate(
  department = gsub("-", "", department)
)

# MODIFY ADMIN3 NAMES TO MATCH THE GEOJSON DATASET GIS MAPS
ebp_data_small <- ebp_data_small  %>% mutate(
  ADMIN3 = gsub("-", "", ADMIN3)
)

ebp_data_small <- ebp_data_small  %>% mutate(
  ADMIN3 = gsub(" ", "", ADMIN3)
)

# checkpionting the working dataset

write.csv(ebp_data_small, file =  "data/processed/ebp_model_analysis_db_dhs_vars_compatible_with_map_var.csv")

# Fit the EBP model

# using meaningfull names
ebp_model <- ebp(
  fixed = as.formula(model_formula_str),
  pop_data = ebp_data,
  pop_domains = "department", # Replace with your domain variable
  smp_data = ebp_data,
  smp_domains = "department", # Replace with your domain variable
  threshold = NULL, # Define a threshold if needed
  weights = "household_sampling_weight_hv005",
  transformation = "log", # Transformation for the dependent variable
  L = 10 # Number of Monte Carlo simulations
)

#using dhs naming convention


ebp_model_admin2 <- ebp(
  fixed = as.formula(model_formula_str),
  pop_data = ebp_data_small,
  pop_domains = "department", # Replace with your domain variable
  smp_data = ebp_data_small,
  smp_domains = "department", # Replace with your domain variable
  threshold = NULL, # Define a threshold if needed
  weights = "hv005",
  transformation = "log", # Transformation for the dependent variable
  L = 2, # Number of Monte Carlo simulations
  na.rm = TRUE,
  MSE = TRUE
)

# small area = admin3 ~ city

#optimizer control
my_control <- nlme::lmeControl(maxIter = 100, msMaxIter = 100, msMaxEval = 100)

ebp_model_admin3 <- ebp(
  fixed = as.formula(model_formula_str),
  pop_data = ebp_data_small,
  pop_domains = "ADMIN3", # Replace with your domain variable
  smp_data = ebp_data_small,
  smp_domains = "ADMIN3", # Replace with your domain variable
  threshold = median(ebp_data_small$hv271), # Define a threshold if needed
  weights = "hv005",
  interval = c(0,5),
  transformation = "log", # Transformation for the dependent variable
  L = 2, # Number of Monte Carlo simulations
  na.rm = TRUE,
  MSE = TRUE,
  cpus = 4
)

# Summarize the results
summary(ebp_model_admin2)
ebp_model$model

# plotting using ggplot and povmap emdi extensions
library(povmap)
library(sf)

# gis datasets
gis_roor_dir = "F:\\Workspaces\\academics\\datasets\\cameroon\\gis\\geojson\\"

get_map_file = function(fname) {
  return(paste0(gis_roor_dir, fname))
}

a2m = get_map_file("gadm41_CMR_2.json")

cm_admin1_map_sf = sf::read_sf(get_map_file("gadm41_CMR_1.json"))
cm_admin2_map_sf = sf::read_sf(get_map_file("gadm41_CMR_2.json"))
cm_admin3_map_sf = sf::read_sf(get_map_file("gadm41_CMR_3.json"))

# rename NAME_2 TO Domain AS GENERATED BY EBP

map_cm_admin2_sf = cm_admin2_map_sf %>% mutate(
  Domain = NAME_2
)

# all names in shape file
names_cm_admin1 = cm_admin1_map_sf$NAME_1
names_cm_admin2 = cm_admin2_map_sf$NAME_2
names_cm_admin3 = cm_admin3_map_sf$NAME_3

# names present in survey
names_cm_admin1_survey = unique(ebp_data_small$hv024) # 10
names_cm_admin2_survey = unique(ebp_data_small$department) # 53
names_cm_admin3_survey = unique(ebp_data_small$ADMIN3) # 225


#save all the names of  all administrative levels (1,2,3) in cameroon
save(names_cm_admin1, names_cm_admin2, names_cm_admin3, file = "data/processed/names_of_all_admin_levels_cm.RData")

# Create map plot for mean indicator - point and MSE estimates but no CV

# ADJUSTING THE DOMAIN VARIABLE IN RESULT 

# map plot for admin 2

map_plot(
  object = ebp_model_admin2, MSE = TRUE, CV = FALSE,
  map_obj = map_cm_admin2_sf, indicator = c("Mean"),
  map_dom_id = "Domain",
  #map_tab = 
)

# model diagnostics plot

plot(ebp_model_admin2)

# map plot for admin 3

cm_admin3_map_sf = sf::read_sf(get_map_file("gadm41_CMR_3.json"))

# rename NAME_2 TO Domain AS GENERATED BY EBP

map_cm_admin3_sf = cm_admin3_map_sf %>% mutate(
  Domain = NAME_3
)
# map plot for admin 2

map_plot(
  object = ebp_model_admin3, MSE = TRUE, CV = FALSE,
  map_obj = map_cm_admin3_sf, indicator = c("Mean"),
  map_dom_id = "Domain",
  #map_tab = 
)

# model diagnostic plot

plot(ebp_model_admin3)


