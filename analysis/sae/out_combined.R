# In this file, we combine the ebp estimates and the smooth estimates to one dataframe
# by imputing missing ebp estimates using smooth estimates from the bayesian bym2 model

library(dplyr)

library(sf)

# Temp: restore models from filesystem ------------------------------------

ebp_model_admin1 = readRDS("data/processed/out/ebp_model_admin1.rds")
ebp_model_admin2 = readRDS("data/processed/out/ebp_model_admin2.rds")
ebp_model_admin3 = readRDS("data/processed/out/ebp_model_admin3.rds")


# direct

model_direct_admin1 = readRDS("data/processed/out/model_direct_admin1.rds")
model_direct_admin2 = readRDS("data/processed/out/model_direct_admin2.rds")
model_direct_admin3 = readRDS("data/processed/out/model_direct_admin3.rds")


load("data/processed/out/mdat.smooth.admin3.resulst.RData")
# Temp: end of model restore ----------------------------------------------

# indicator dataframes
ind3 <- ebp_model_admin3$ind[,c("Domain", "Mean", "Head_Count","Poverty_Gap","Gini")]


# combine all smooth estimates --------------------------------------------
shc = mdat.smooth.headcount$smooth[,c("region","mean")]
sgini = mdat.smooth.gini$smooth[,c("region","mean")]
spovgap = mdat.smooth.povgap$smooth[,c("region","mean")]
smean = mdat.smooth.wealthscore_mean$smooth[,c("region","mean")]

colnames(shc) <- c("Domain", "Head_Count")
colnames(sgini) <- c("Domain", "Gini")
colnames(spovgap) <- c("Domain", "Poverty_Gap")
colnames(smean) <- c("Domain", "Mean")

# Combine into a list
df_list <- list(smean
                ,shc
                ,spovgap
                ,sgini
                )

# Merge all coloumns by a custom reducing 
mds <- Reduce(function(x, y) merge(x, y, by = "Domain", all = TRUE), df_list)


new_subdivs = setdiff(mds$Domain,ind3$Domain)

new_rows = mds[mds$Domain %in% c(new_subdivs),]

combined_df = rbind(ind3,new_rows)

colnames(combined_df)[1] <- "subdivision"

saveRDS(combined_df,file = "data/processed/subdiv_result.rds")

write.csv(combined_df, file = "data/processed/subdiv_result.csv")



# Merge df1 and df2 by column "subdiv"
#merged_df <- combined_df %>% left_join(admin321  %>% distinct(subdivision, .keep_all = TRUE), by = "subdivision")



#gis <- read.csv("data/processed/gis_reverse_geo_data_admin3_corrected.csv")


# Using the map file to merge data ----------------------------------------





geo3 <- st_read("data/dataset/cameroon/gis/geojson/gadm41_CMR_3.json")


geo3_clone <- geo3

geo3_clone <- geo3_clone %>%  rename(
  subdivision = NAME_3,
  division = NAME_2,
  region = NAME_1
)

# merge geo3_clone with combined_df

agg_subdiv = merge(geo3_clone, combined_df, by = "subdivision", all.x = TRUE)

#save it to dis

saveRDS(agg_subdiv, file = "data/processed/out/result_and_geo.rds")
st_write(agg_subdiv,"data/processed/out/geojson/cm.subdivision.geojson")




# merge geo3_clone with combined_df



result3 = merge(geo3_clone, combined_df, by = "subdivision", all.x = TRUE)
agg_division = aggregate(x = cbind(Mean,Head_Count, Poverty_Gap, Gini) ~ division,
                  data = result3, FUN = mean)
agg_region = aggregate(x = cbind(Mean,Head_Count, Poverty_Gap, Gini) ~ region,
                  data = result3, FUN = mean)
agg_country = aggregate(x = cbind(Mean,Head_Count, Poverty_Gap, Gini) ~ COUNTRY,
                  data = result3, FUN = mean)


# merge with appropriate map data -----------------------------------------

# Divisional maps ---------------------------------------------------------

geo2 <- st_read("data/dataset/cameroon/gis/geojson/gadm41_CMR_2.json")
geo1 <- st_read("data/dataset/cameroon/gis/geojson/gadm41_CMR_1.json")

geo2 <- geo2 %>%  rename(
  division = NAME_2,
  region = NAME_1
)

geo1 <- geo1 %>% rename(
  region = NAME_1
)


geojson_division = merge(geo2,agg_division, by = "division", all.x = TRUE)
st_write(geojson_division,"data/processed/out/geojson/cm.division.geojson")

geojson_region = merge(geo1,agg_region, by = "region", all.x = TRUE)
st_write(geojson_region,"data/processed/out/geojson/cm.region.geojson")

