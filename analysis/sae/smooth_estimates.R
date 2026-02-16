library(rgdal)
library(haven)
library(SUMMER)
library(gridExtra)
library(ggplot2)
library(dplyr)



root <- "F:/Workspaces/academics/masters_thesis/"
setwd(root)

# Load the data
#data <- read.csv("data/processed/main_meaningfull_names_dhs_hr_gis_2018.csv")
#data <- read.csv("data/processed/main_dhs_names_dhs_hr_gis_2018.csv")

data <- readRDS("data/processed/main_dhs_names_dhs_hr_gis_2018.rds")
geo1 <- readOGR("data/dataset/cameroon/gis/geojson/gadm41_CMR_1.json")
geo2 <- readOGR("data/dataset/cameroon/gis/geojson/gadm41_CMR_2.json")
geo3 <- readOGR("data/dataset/cameroon/gis/geojson/gadm41_CMR_3.json")
geo1s <- readOGR("data/dataset/dhs/cameroon/spatial_data/GADM/gadm41_CMR_shp/gadm41_CMR_1.shp")

amat <- getAmat(geo = geo1s, geo1s$NAME_1)
amat1 <- getAmat(geo = geo1, geo1$NAME_1)
amat2 <- getAmat(geo = geo1, geo1$NAME_2)
amat3 <- getAmat(geo = geo3, geo3$NAME_3)



# Save adjacency matrix ---------------------------------------------------

save(amat, amat2, amat3, file = "data/processed/adjencency_matrix_admin123.Rdata")
write.csv(amat3, file = "data/processed/adjencency_matrix_admin3.csv")

# point (lat,long) to region name
loc <- readOGR("data/dhs_cmr/CM_2018_DHS_09092023_1923_174579/CMGE71FL/CMGE71FL.shp")


loc.dat <- data.frame(cluster = loc$DHSCLUST, long = loc$LONGNUM, lat = loc$LATNUM)
gps1 <- mapPoints(loc.dat, geo = geo1, long = "long", lat = "lat", names = c("NAME_1"))
gps2 <- mapPoints(loc.dat, geo = geo2, long = "long", lat = "lat", names = c("NAME_2"))
gps3 <- mapPoints(loc.dat, geo = geo3, long = "long", lat = "lat", names = c("NAME_3"))

# rename the columns
colnames(gps1)[4] <- "region"
colnames(gps2)[4] <- "division"
colnames(gps3)[4] <- "subdivision"

save(gps1, gps2, gps3, file = "data/processed/reverse_geo_admin123.Rdata")

# merge gis data

# merge(subdiv,division)
admin32 <- merge(gps3,gps2[,c("cluster","division")], by = "cluster", all.x = TRUE)
# merge((subdiv, division),division)
admin321 <- merge(admin32,gps1[,c("cluster","region")], by = "cluster", all.x = TRUE)

# save newly merged dataset for geo_reversed fields
write.csv(admin321, file = "data/processed/reverse_geo_admin321.csv")
saveRDS(admin321, file = "data/processed/reverse_geo_admin321.rds")


regions <- gps1 %>% select(region) %>% unique()
divisions <- gps2 %>% select(division) %>% unique()
subdivs <- gps3 %>% select(subdivision) %>% unique()

write.csv(gps3, file = "data/processed/reverse_geo_admin3.csv")

# Estimating the poverty prevalence

cmhr <- readRDS("data/processed/main_dhs_names_dhs_hr_gis_2018.rds")

# simple workable format for out dataset
# format (clusterid, id, weight, strata,age, wealth, region, division, subdiv)

data <- data.frame(
  cluster = cmhr$hv001,
  id = cmhr$hv002,
  weight = cmhr$hv005,
  strata = cmhr$hv023,
  age = cmhr$hv220,
  wealth = cmhr$hv270,
  wealthscore = cmhr$hv271
  
)

# merge with gis data

datagis <- merge(data, admin321[,c("cluster","region","division","subdivision")], by ="cluster", all.x = TRUE)
datagis_clean <- na.omit(datagis)


# Undesigned aggregation --------------------------------------------------



direct.region.score = aggregate(wealthscore ~ region, data = datagis_clean, FUN = mean)
direct.division.score = aggregate(wealthscore ~ division, data = datagis_clean, FUN = mean)
direct.subdiv.score = aggregate(wealthscore ~ division, data = datagis_clean, FUN = mean)

# fitGeneric
direct.region.smoothed <- smoothSurvey(data = datagis_clean, geo = geo1, Amat = amat1,
                       responseType = "gaussian", responseVar = "wealthscore", regionVar = "region",
                       strataVar = NULL, weightVar = NULL, clusterVar = NULL, CI = 0.95)

# End undesigned aggregation ----------------------------------------------



# Weighted estimates ------------------------------------------------------

library(survey)

direct.design <- svydesign(ids = ~cluster + id, weights = ~weight, strata = ~strata,
                    data = datagis_clean)

dd.region.mean <- svyby(~wealthscore, ~region, design = direct.design, svymean)

head(dd.region.mean)

# smoothing model
# fitGeneric -> smoothSurvey

dd.region.smooth <- smoothSurvey(data = datagis_clean, geo = geo1, Amat = amat1,
                             responseType = "gaussian", responseVar = "wealthscore", regionVar = "region",
                             strataVar = "strata", weightVar = "weight", clusterVar = "~cluster+id",
                             CI = 0.95)


# End weighted estimates --------------------------------------------------


# Plots -------------------------------------------------------------------



# By region ---------------------------------------------------------------


prev <- NULL
prev <- rbind(prev, data.frame(region = direct.region.smoothed$HT$region,
                               mean = direct.region.smoothed$HT$HT.est,
                               var = direct.region.smoothed$HT$HT.var,
                               type = "Design free"))
prev <- rbind(prev, data.frame(region = direct.region.smoothed$smooth$region,
                               mean = direct.region.smoothed$smooth$mean,
                               var = direct.region.smoothed$smooth$var,
                               type = "Smoothed design free"))
prev <- rbind(prev, data.frame(region = dd.region.smooth$HT$region,
                               mean = dd.region.smooth$HT$HT.est,
                               var = dd.region.smooth$HT$HT.var,
                               type = "Design-Weighted"))
prev <- rbind(prev, data.frame(region = dd.region.smooth$smooth$region,
                               mean = dd.region.smooth$smooth$mean,
                               var = dd.region.smooth$smooth$var,
                               type = "Smooth Design-Weighted"))

g1 <- mapPlot(prev, geo = geo1, by.data = "region", by.geo = "NAME_1",
              variables = "type", values = "mean", is.long = TRUE, legend.label = "Estimates",
              ncol = 4)
g2 <- mapPlot(prev, geo = geo1, by.data = "region", by.geo = "NAME_1",
              variables = "type", values = "var", is.long = TRUE, legend.label = "Variance",
              ncol = 4)
grid.arrange(g1, g2, ncol = 1)
