library(tidyverse)
library(foreign)
library(survey)
library(sf)
library(emdi)
library(ggplot2)
library(stringr)
library(dplyr)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

# Load the data
#data <- read.csv("data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")
#data <- read.csv("data/processed/main_dhs_names_dhs_hr_gis_2018.csv")

data <- readRDS("data/processed/main_dhs_names_dhs_hr_gis_2018.rds")



dependent_var <- "hv271"


independent_vars <- c(
                      "hv005"
                      #, "hv023" # survey sampling related vars
                      ,"hv024" 
                      ,"hv025"#,"hv026" # location
                      #,"hv107_01" #
                      ,"hv106_01" # education
                      #,"hv204" # time to get water(mins + onprem)
                      , "hv201" #,"hv202" # health-water
                     ,"hv206", "hv209", "hv212", "hv226", "hv205" #"hv238a" # living standards
                      #,"hv270", "hv271" # wealth index quantiles and score respectively
                      ,"hv213"
                     #, "hv214", "hv215" # living standards - infrastructure
                      ,"sb118_01", "ha57_01" # health
                     ,"hv009", "hv216" # person-to-room ratio (ppr) = if(p/r) > 1 => overcrowed | ppr > 1.5 => severely overcrowed | ppr > 3 => critical overcrowding
                      #,"hv219", "hv220" # household main entity attributes
                      #,"hv206","hv209","hv213","hv201", "hv024", "hv025" #living_standards_index
                      #,"hv271" # wealth and employment index
                     ,"crowding" # hv009/hv216
)


# Fixed effect formula ----------------------------------------------------

model_formula_str = paste(dependent_var, "~", paste(independent_vars, collapse = " + "))


ivars = c("cluster_id_hv001","ultimate_area_code_hv004", "household_sampling_weight_hv005"
          ,"sample_strata_design_hv023" # NB remmoved because of correlation "sample_strata_error_hv022" 
          ,"region_hv024","residence_type_hv025", "place_of_residence_hv026", "cluster_altitude_hv040" # location attributes (region hv024, ...)
          ,"highest_edu_level_hv107", "highest_edu_attained_hv106", "edu_attainment_hv109", "school_attend_status_hv129" # educational attributes | watchout for high correlation
          ,"time_to_water_hv204", "source_water_drink_hv201","source_water_no_drink_hv202" # health attributes
          ,"has_electricity_hv206","has_refregerator_hv209", "has_car_hv212","cooking_fuel_hv226","toilet_facility_hv205", "toilet_location_hv238a" #living_standards_index
          ,"wealth_index_quantiles_hv270", "wealth_index_score_hv271" # classification
          ,"main_floor_material_hv213", "main_wall_material_hv214", "main_roof_material_hv215" #living_standards_index -> infrastructure                     
          ,"malaria_rdt_sb118_01","anemia_level_ha57_01" #health attributes
          ,"num_house_members_hv009","num_sleeping_rooms_hv216" # person-to-room ratio (ppr) = if(p/r) > 1 => overcrowed | ppr > 1.5 => severely overcrowed | ppr > 3 => critical overcrowding
          ,"sex_head_house_hv219", "age_head_house_hv220" #household attributes                     
)





#impute missing values

fn_most_common_str <- function(arr) {
  freq_table =  sort(table(arr[!is.na(arr)]), decreasing = TRUE)
  return(names(freq_table)[1])
}



# DATA IMPUTATION ---------------------------------------------------------

# determine columns with null values
null_column_count = function(df) {
  null_cols = list()
  my_map <- new.env(hash = TRUE)
  for (cname in colnames(df)) {
    null_cols[cname] = sum(is.na(df[cname]))
    #print(sum(is.na(data[cname])))
  }
  return(null_cols)
}

hv107 = data$hv107_01

# library(ggthemes)

# ggplot(data, aes(x = hv106_01)) + geom_bar() + theme_bw()
# # pie charts
# ggplot(data, aes(x = "", y = sb118_01, fill = sb118_01)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) +
#   theme_void() # Removes background grid and axes
# ggplot(data, aes(x = hv106_01)) + geom_bar(fill = "steelblue", color = "white") + theme_minimal() + labs(title = "School attendance", x = "Educational level",y = "Count")

# plot themes
 

# ggplot(ebp_data_small, aes(x = hv201)) + geom_bar(color="black", fill="#fdcb7c") + coord_flip() + theme_minimal() +
# theme(
#   text = element_text(color = "black"),
#   axis.text = element_text(color = "black"),
#   axis.title = element_text(color = "black"),
#   plot.title = element_text(color = "black", face = "bold"),
#   #panel.grid = element_line(color = "gray", alpha = 0.1),
#   #axis.line = element_line(color = "orange"),
#   legend.text = element_text(color = "black"),
#   legend.title = element_text(color = "black")
# )


#library(mice)

#imputed_data <- mice(data, method = 'pmm', m = 5)

get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

ebp_data_imputed <- data %>% mutate(
  #hv107_01 = factor(ifelse(is.na(hv107_01),get_mode(my_factor), as.character(my_factor))))
  hv107_01 = factor(ifelse(is.na(hv107_01),as.character(0), as.character(hv107_01))),
  hv129_01 = factor(ifelse(is.na(hv129_01),as.character("Don't know"), as.character(hv129_01))),
  hv202 = factor(ifelse(is.na(hv202),as.character("other"), as.character(hv202))),
  sb118_01 = factor(ifelse(is.na(sb118_01),as.character("other"), as.character(sb118_01))),
  ha57_01 = factor(ifelse(is.na(ha57_01),as.character("Not anemic"), as.character(ha57_01))),
  hv024 = recode(hv024, 
                         "adamawa" = "Adamaoua",
                         "centre (without yaounde)" = "Centre",
                         "douala" =  "Littoral", # ignoring population density
                         "east" = "Est",
                         "far-north" = "Extrême-Nord",
                         "littoral (without douala)" = "Littoral",
                         "north" = "Nord",
                         "north-west" = "Nord-Ouest",
                         "south" = "Sud",
                         "west" = "Ouest",                         
                         "south-west" = "Sud-Ouest",
                         "yaounde" = "Centre" # ignoring population density                                              
                         ),
    crowding = hv009 / hv216,
    hv213 = factor(ifelse(hv213 %in% c("parquet or polished wood",
                                       "dung","wood planks","palm/bamboo"),
                          as.character("wood"), as.character(hv213))), # because of little presence of attributes in some domains
    hv226 = factor(ifelse(hv226 %in% c("animal dung", 
                                       "agricultural crop",
                                       "straw/shrubs/grass",
                                       "biogas"), as.character("plant_based"), as.character(hv226) )),
    hv205 = factor(ifelse(hv205 %in% c("other",
                                       "bucket toilet",
                                       "composting toilet",
                                       "flush, don't know where",
                                       "flush to somewhere else"), as.character("other"), as.character(hv205))),
    hv201 = recode(hv201, 
                   "piped into dwelling"= "piped_home_vicinity",
                   "piped water" = "piped_home_vicinity",
                   "piped to yard/plot" = "piped_home_vicinity",
                   "piped to neighbor" = "piped_home_vicinity",
                   "public tap/standpipe" = "public tap/standpipe",
                   "tube well water" = "well",
                   "tube well or borehole" = "well",
                   "dug well (open/protected)" = "well",
                   "protected well" = "well",
                   "unprotected well" = "well",
                   "surface from spring" = "spring",
                   "protected spring" = "spring",
                   "unprotected spring" = "spring",
                   "river/dam/lake/ponds/stream/canal/irrigation channel" = "surface",
                   "rainwater" = "rainwater",
                   "tanker truck" = "other",
                   "cart with small tank" ="other",
                   "bottled water" = "",
                   "sachet water" = "other",
                   "other" = "other"
                   
                   )
  )




# Match geojson shapefiles requirement ------------------------------------


# Custom survey sampling inorder to satisfy the tool
# create a sample data for the survey
# ----------------
# dhs_cluster_size = 25
# no_admin2_cm = 58
# popul_size = nrow(ebp_data_imputed)

# sample_size = dhs_cluster_size * no_admin2_cm

# inclusion_prop = sample_size / popul_size # n/N 

# sampling_weight = 1 / inclusion_prop # 1 / pi_k => N/n

# samples_data = list()


# ----- end of custom survey sampling ------------

#inspired by booStraping using SRS with replacement and assuming similar distribution
# between the samples and the population
#for( i in 1:sample_size) {
# sampled_data = slice_sample(ebp_data_imputed, n = sample_size, replace = TRUE) # creating the sample <----
#samples_data[i] = sample
#}
# library(sampling)

# stratified_sampled_data = sampling::strata(ebp_data_imputed,
#                                            stratanames = c("hv023"),
#                                            #stratanames = as.list(ebp_data_imputed$hv023 %>% unique()),
#                                            size = sample_size, 
#                                            method = "srsor")





# REMOVING the department compound word spacing to match map NAME_2
library(stringr)


# MODIFY ADMIN2(department) NAMES TO MATCH THE GEOJSON DATASET GIS MAPS
ebp_data_imputed <- ebp_data_imputed  %>% mutate(
  department = gsub("-", "", department)
)

# MODIFY ADMIN3 NAMES TO MATCH THE GEOJSON DATASET GIS MAPS
ebp_data_imputed <- ebp_data_imputed  %>% mutate(
  ADMIN3 = gsub("-", "", ADMIN3)
)

ebp_data_imputed <- ebp_data_imputed  %>% mutate(
  ADMIN3 = gsub(" ", "", ADMIN3)
)

# checkpionting the working dataset <------

#write.csv(ebp_data_imputed, file =  "data/processed/ebp_model_analysis_db_dhs_vars_compatible_with_map_var.csv")
# saving as an r session object inorder not to loose transformation progress
#saveRDS(object = ebp_data_imputed, file = "data/processed/ebp_model_analysis_db_dhs_vars_compatible_with_map_var.rds")




# nco = null_column_count(ebp_data_imputed)

#fileter ebp_data to usefull set of columns

ebp_data_small <- ebp_data_imputed %>% select(
  #all_of(c(dependent_var, independent_vars,"hv202","hv005", "department","city", "region","hv000", "ADMIN3"))
  all_of(c(dependent_var, independent_vars,"hv202", "department","hv000", "ADMIN3"))
) %>% na.omit()


nco2 = null_column_count(ebp_data_small)




# View((ebp_data_small))

# tab = table(ebp_data_small$hv202)

# departs = unique(ebp_data_small$department)
# subdivs = unique(ebp_data_small$ADMIN3)

# ggplot(data = ebp_data_small, aes(x = hv202)) + geom_bar(aes(fill = hv202)) + labs(title = "Distribution of Source of non-drinking water", x = "Source of non-drinking water", y = "count" )





# Direct estimation
# ---------------------

# Head Count Ratio thresshold: 60% of median(hv271)

# y_thresshold = 0.6 * median(ebp_data_small$hv271)

# m = median(ebp_data_small$hv271)



# model_direct_admin3 <- emdi::direct(y = dependent_var,
#                                     smp_data = as.data.frame(ebp_data_small), 
#                                     weights = "hv005", 
#                                     #threshold = m,
#                                     smp_domains = "ADMIN3",
#                                     design = "hv023",
#                                     var = FALSE,
#                                     na.rm = TRUE
# )

# Fit the EBP model
# ------------------------


# small area = admin3 ~ city

#optimizer control
# my_control <- nlme::lmeControl(maxIter = 100, msMaxIter = 100, msMaxEval = 100)

#ols <- lm(model_formula_str, data = ebp_data_small)

# formula : "hv271 ~ hv005 + hv024 + hv025 + hv106_01 + hv206 + hv209 + hv213 + sb118_01 + ha57_01 + hv009 + hv216 + crowding"
# "hv271 ~ hv005 + hv023 + hv024 + hv025 + hv106_01 + hv201 + hv202 + hv206 + hv209 + hv212 + hv226 + hv205 + hv213 + hv214 + hv215 + sb118_01 + ha57_01 + hv009 + hv216 + crowding"

ebp_model_admin3 <- ebp(
  fixed = as.formula(model_formula_str),
  #fixed = hv271 ~ hv005 + hv024 + hv025 + hv106_01 + hv206 + hv209 + hv213 + sb118_01 + ha57_01 + hv009 + hv216 + crowding,
  pop_data = ebp_data_small,
  pop_domains = "ADMIN3", # Replace with your domain variable
  smp_data = ebp_data_small,
  smp_domains = "ADMIN3", # Replace with your domain variable
  #threshold = median(ebp_data_small$hv271), # Define a threshold if needed
  weights = "hv005",
  interval = c(0,5),
  transformation = "log", # Transformation for the dependent variable
  L = 1, # Number of Monte Carlo simulations
  B = 2,
  na.rm = TRUE,
  MSE = TRUE,
  cpus = 4
)

ebp_model_admin2 <- ebp(
  fixed = as.formula(model_formula_str),
  #fixed = hv271 ~ hv005 + hv024 + hv025 + hv106_01 + hv206 + hv209 + hv213 + sb118_01 + ha57_01 + hv009 + hv216 + crowding,
  pop_data = ebp_data_small,
  pop_domains = "department", # Replace with your domain variable
  smp_data = ebp_data_small,
  smp_domains = "department", # Replace with your domain variable
  #threshold = median(ebp_data_small$hv271), # Define a threshold if needed
  weights = "hv005",
  interval = c(0,5),
  transformation = "log", # Transformation for the dependent variable
  L = 1, # Number of Monte Carlo simulations
  B = 2,
  na.rm = TRUE,
  MSE = TRUE,
  cpus = 4
)

ebp_model_admin1 <- ebp(
  fixed = as.formula(model_formula_str),
  #fixed = hv271 ~ hv005 + hv024 + hv025 + hv106_01 + hv206 + hv209 + hv213 + sb118_01 + ha57_01 + hv009 + hv216 + crowding,
  pop_data = ebp_data_small,
  pop_domains = "hv024", # Replace with your domain variable
  smp_data = ebp_data_small,
  smp_domains = "hv024", # Replace with your domain variable
  #threshold = median(ebp_data_small$hv271), # Define a threshold if needed
  weights = "hv005",
  interval = c(0,5),
  transformation = "log", # Transformation for the dependent variable
  L = 1, # Number of Monte Carlo simulations
  B = 2,
  na.rm = TRUE,
  MSE = TRUE,
  cpus = 4
)

# ebp_model_admin0 <- ebp(
#   fixed = as.formula(model_formula_str),
#   #fixed = hv271 ~ hv005 + hv024 + hv025 + hv106_01 + hv206 + hv209 + hv213 + sb118_01 + ha57_01 + hv009 + hv216 + crowding,
#   pop_data = ebp_data_small,
#   pop_domains = "hv000", # Replace with your domain variable
#   smp_data = ebp_data_small,
#   smp_domains = "hv000", # Replace with your domain variable
#   #threshold = median(ebp_data_small$hv271), # Define a threshold if needed
#   weights = "hv005",
#   interval = c(0,5),
#   transformation = "log", # Transformation for the dependent variable
#   L = 1, # Number of Monte Carlo simulations
#   B = 2,
#   na.rm = TRUE,
#   MSE = TRUE,
#   cpus = 4
# )



# Summarize the results
summary(ebp_model_admin2)

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
merged_data = merge(ebp_model_admin2$ind,map_cm_admin2_sf, by = "Domain", all.x = TRUE)

saveRDS(merged_data, file = "data/processed/temp_merged_results_admin2_incators_map.rds")
write.csv(merged_data, file = "data/processed/temp_merged_results_admin2_incators_map.csv")
write.excel(ebp_model_admin2, file = "data/processed/temp_merged_results_admin2_incators_map.xlsx")
library(sf)
library(sp)




map_plot(
  object = ebp_model_admin3, MSE = TRUE, CV = FALSE,
  map_obj = map_cm_admin3_sf, indicator = c("Mean"),
  map_dom_id = "Domain",
  #map_tab = 
)

# model diagnostic plot

plot(ebp_model_admin3)









