# Prepare and preprocess gis data and covariates
library(foreign)
#require("./code/analysis/sae/_common_data_proc.r")
wd <- "F:/Workspaces/academics/masters_thesis/data/dataset/"  
setwd(wd)

dataset_root <- "F:/Workspaces/academics/masters_thesis/data/dataset/"
# G:\Workspaces\academics\datasets\dhs\cameroon\CM_2018_DHS_09092023_1923_174579\CMGE71FL
#dataset_root <- "G:/Workspaces/academics/masters_thesis/data/dataset/dhs/cameroon/CM_2018_DHS_09092023_1923_174579/CMGE71SV/"

p_geocoded_dhs_2018_raw <- paste0(dataset_root, "dhs/cameroon/CM_2018_DHS_09092023_1923_174579/CMGE71FL/geocoded_by_geoapify.csv")


df_geocoded_dhs_2018 <- read.csv(p_geocoded_dhs_2018_raw)
head(df_geocoded_dhs_2018)
View(df_geocoded_dhs_2018)
