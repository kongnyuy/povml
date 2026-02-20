# In this file, we combine the ebp estimates and the smooth estimates to one dataframe
# by imputing missing ebp estimates using smooth estimates from the bayesian bym2 model

library(dplyr)

library(sf)

# Temp: restore models from filesystem ------------------------------------

ebp_model_admin1 = readRDS("data/processed/out/ebp_model_admin1.rds")
ebp_model_admin2 = readRDS("data/processed/out/ebp_model_admin2.rds")
ebp_model_admin3 = readRDS("data/processed/out/ebp_model_admin3.rds")

df = ebp_model_admin1$ind

colnames(df)[2] = "Division count"
colnames(df)[3] = "Wealth score mean"
colnames(df)[4] = "Head count Ration / Poverty incidence"


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



# Establishment of wealth index quintiles for generated aggregates --------

subdiv_aggs = combined_df

wmean = sort(subdiv_aggs$Mean, decreasing = TRUE)

quintile_len = 5 # -> 20%

wmean_max = wmean[1]
wmean_min = wmean[360]
tmargin = (wmean_max - wmean_min) / quintile_len # 272893.4

#quintiles: Richest, Richer, Middle, Poorer, Poorest


fun_calc_quintile_range <- function(i,n = 5) {
  l = wmean_min + ((n - i) * tmargin ) # + (n - i)
  h = l + tmargin
  return (c(l,h))
}


q_Richest = fun_calc_quintile_range(1)
q_Richer = fun_calc_quintile_range(2)
q_Middle = fun_calc_quintile_range(3)
q_Poorer = fun_calc_quintile_range(4)
q_Poorest = fun_calc_quintile_range(5)

# METHOD 2

aug4 = subdiv_aggs$quintile <- cut(
  subdiv_aggs$Mean,
  breaks = quantile(subdiv_aggs$Mean, probs = seq(0, 1, 0.2), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest")
)

# method 2 end




# Quintiles end --------------


# update subdiv_aggs with quintile information ----------------------------
aug3 = list()
aug5 = subdiv_aggs %>% mutate(
#aug3 = aug3 %>% mutate(
  wealthQuintileBinned = case_when(
    Mean > q_Richest[1] & Mean <= q_Richest[2] ~ "Richest", #>=
    Mean > q_Richer[1] & Mean <= q_Richer[2] ~ "Richer",
    Mean > q_Middle[1] & Mean <= q_Middle[2] ~ "Middle",
    Mean > q_Poorer[1] & Mean <= q_Poorer[2] ~ "Poorer",
    Mean > q_Poorest[1] & Mean <= q_Poorest[2] ~ "Poorest"
  )
  
)

saveRDS(combined_df,file = "data/processed/subdiv_result_wealth_binned.rds")
write.csv(combined_df, file = "data/processed/out/subdiv_result_wealth_binned.csv")

# End of quantiles / binning augmenteation

# 1. Define the labels in order from lowest value to highest
quintile_labels <- c("Poorest", "Poorer", "Middle", "Richer", "Richest")

subdiv_aggs$Quintile <- cut(subdiv_aggs$Mean, 
                            breaks = 5, 
                            labels = quintile_labels,
                            include.lowest = TRUE)

a = table(subdiv_aggs$Quintile)


# Wealth quantiles --------------------------------------------------------

qs <- quantile(combined_df$Mean, probs = seq(0, 1, 0.2), na.rm = TRUE)

combined_df <- combined_df %>%
  mutate(
    wealthQuantile = case_when(
      Mean <= qs[2] ~ "Poorest",
      Mean <= qs[3] ~ "Poorer",
      Mean <= qs[4] ~ "Middle",
      Mean <= qs[5] ~ "Richer",
      TRUE          ~ "Richest"
    )
  )




#qs <- quantile(combined_df$Mean, probs = seq(0, 1, 0.2), na.rm = TRUE)
qs <- quantile(combined_df$Mean, probs = seq(0, 1,0.142), na.rm = TRUE)

combined_df <- combined_df %>%
  mutate(
    wealthQuantile = case_when(
      Mean <= qs[2] ~ "Poorest",
      Mean <= qs[3] ~ "Poorer",
      Mean <= qs[4]
      Mean <= qs[5] ~ "Middle",
      Mean <= qs[6] ~ "Rich",
      Mean <= qs[7] ~ "Richer",
      TRUE          ~ "Richest"
    )
  )


saveRDS(combined_df,file = "data/processed/subdiv_result.rds")

write.csv(combined_df, file = "data/processed/subdiv_result.csv")
write.csv(combined_df, file = "data/processed/out/subdiv_result.csv")



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


# poor condition ----------------------------------------------------------

agg_subdiv_cond = agg_subdiv %>% mutate(
  condition = Mean < 89610
)

View(head(agg_subdiv))

saveRDS(agg_subdiv_cond, file = "data/processed/out/result_and_geo_cond.rds")
st_write(agg_subdiv_cond,"data/processed/out/geojson/cm.subdivision_cond.geojson",quiet = TRUE)

#-----------------------




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

