library(haven)     # DHS .dta files
library(dplyr)
library(lme4)      # Mixed models
library(emdi)
#library(saebp)     # EBP estimation

#require("../_common_data_proc.r")
#

setwd("F:/Workspaces/academics/masters_thesis")
dhs_HR_db_path = "data/processed/main_dhs_names_dhs_hr_gis_2018.csv"


# loading of the dhs 2018 HR dataset
#CMHR71FL <- read_dta("data/raw/dhs/CMHR71FL.DTA") # Cameroon 2018 HR file
data <- read.csv(dhs_HR_db_path)

#dhs <- CMHR71FL %>% select(c("hv024","hv106", "hv201","hv205", "hv012", "hv216", "hv226", "hv271"))
dhs <- data %>% select(c("hv024", "hv025","hv106_01", "hv201","hv205","hv206", "hv216", "hv226", "hv271"))

View(head(dhs))

colnames(dhs %>% filter())


#Variable Renaming & Construction

dhs <- dhs %>%
  mutate(
    area = hv024,  # region
    urban = ifelse(hv025 == 1, 1, 0),

    # Education
    head_education = hv106_01,
    deprived_educ = ifelse(head_education < 2, 1, 0),

    # Health
    improved_water = ifelse(hv201 %in% c(11,12,13), 1, 0),
    improved_san = ifelse(hv205 %in% c(11,12), 1, 0),
    deprived_health = ifelse(improved_water == 0 | improved_san == 0, 1, 0),

    # Income / Economic Status Dimension
    deprived_income = ifelse(hv271 < 0, 1, 0),
    electricity = hv206,

    # Living standards
    crowding = hv012 / hv216, #number of dejure members / number of rooms
    deprived_living = ifelse(crowding > 3 | hv226 > 5, 1, 0),

    # MPI
    mpi = 0.25 * deprived_educ +
          0.25 * deprived_health +
          0.25 * deprived_income +
          0.25 * deprived_living,

    poor = ifelse(mpi >= 0.33, 1, 0)
  )

dhs_cleaned <- na.omit(dhs)

data <- dhs
clean_data <- dhs_cleaned


View(head(dhs))
summary(clean_data)

# Fit Unit-Level Mixed Model

lmerControl(autoscale = TRUE) # autoscale the covariates

model <- lmer(
  mpi ~ head_education + hv271 + improved_water + improved_san +
        electricity + crowding + urban + (1 | area),
  data = dhs,
  control = lmerControl(autoscale = TRUE)
)


# EPB

ebp_results <- ebp(
  fixed = mpi ~ head_education + hv271 + improved_water +
                improved_san + electricity + crowding + urban,
  random = ~ 1 | area,
  data = dhs,
  MSE = TRUE
)

povEbp <- emdi::ebp(
  mpi ~ head_education + hv271 + improved_water +
    improved_san + electricity + crowding + urban,
  pop_data = dhs,
  smp_data = dhs,
  pop_domains = "adminl3",
  smp_domains = "adminl3"
)


# Small area poverty estimates

area_poverty <- aggregate(
  ebp_results$ebp ~ dhs$area,
  mean
)

colnames(area_poverty) <- c("region", "estimated_mpi")


# Department-Level R Implementation

## Variable Construction (DHS HR – Cameroon)

library(haven)
library(dplyr)
library(lme4)
library(saebp)

dhs <- read_dta("CMHR71FL.DTA")

dhs <- dhs %>%
  mutate(
    dept = hv026,
    urban = ifelse(hv025 == 1, 1, 0),

    # Education
    depr_educ = ifelse(hv106 < 2, 1, 0),

    # Health
    improved_water = ifelse(hv201 %in% c(11,12,13), 1, 0),
    improved_san = ifelse(hv205 %in% c(11,12), 1, 0),
    depr_health = ifelse(improved_water == 0 | improved_san == 0, 1, 0),

    # Economic
    depr_income = ifelse(hv271 < median(hv271, na.rm = TRUE), 1, 0),

    # Living standards
    crowding = hv012 / hv216,
    depr_living = ifelse(crowding > 3 | hv226 > 5, 1, 0),

    mpi = 0.25 * (depr_educ + depr_health + depr_income + depr_living),
    poor = ifelse(mpi >= 0.33, 1, 0)
  )


## Department-Level EBP

ebp_mpi <- ebp(
  fixed = mpi ~ hv106 + hv271 + improved_water + improved_san +
                crowding + urban,
  random = ~ 1 | dept,
  data = dhs,
  MSE = TRUE
)

## Department Poverty Estimates

dept_estimates <- aggregate(
  ebp_mpi$ebp,
  by = list(department = dhs$dept),
  FUN = mean
)

colnames(dept_estimates)[2] <- "MPI_EBP"

# -----------------
# Production of map plots for the poverty estimates
# -----------------
## Code to Join EBP Results and Plot Maps

library(sf)
library(tmap)     # thematic maps
library(dplyr)

# Load department shapefile
departments <- st_read("cameroon_departments.shp")  # shapefile from geodata repository

# Join EBP MPI estimates by 'dept' code
map_data <- departments %>%
  left_join(dept_estimates, by = c("ADM2_CODE" = "department"))

# Map MPI
tm_shape(map_data) +
  tm_polygons("MPI_EBP",
              palette = "RdYlBu",
              title = "Multidimensional Poverty Index (EBP)",
              style = "quantile") +
  tm_layout(title = "Cameroon Department-level Multidimensional Poverty")



# -------------
# Bayesian mpi
# -------------

library(brms)

bayes_model <- brm(
  formula = mpi ~ hv106 + hv271 + improved_water + improved_san +
                      crowding + urban + (1 | dept),
  data = dhs,
  family = gaussian(),
  prior = c(
    prior(normal(0, 10), class = "b"),
    prior(cauchy(0, 2), class = "sd"),
    prior(cauchy(0, 2), class = "sigma")
  ),
  iter = 4000, warmup = 1000, chains = 4, cores = 4
)

# Extract department posterior means
dept_bayes <- as_draws_df(bayes_model)

