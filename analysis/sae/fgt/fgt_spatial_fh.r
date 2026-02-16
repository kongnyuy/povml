# Poverty is spatially correlated. Ignoring this leads to: Oversmoothing, Biased uncertainty estimates

# -> Constructing spatial weights matrix
## Using queen contiguity

library(sf)
library(spdep)
library(sae)

# Sub-division polygons
subdiv_sf <- subdiv %>% arrange(sub_div_id)

# Neighbors
nb <- poly2nb(subdiv_sf, queen = TRUE)

# Row-standardized weights
W <- nb2listw(nb, style = "W")


# -> Fit Spatial Fay–Herriot model

#install.packages("sae")
#library(sae)

sfh_model <- eblupSFH(
  fgt0 ~ urban_share + nightlights + pop_density,
  vardir = sae_data$vardir,
  data = sae_data, # import file
  W = W,
  method = "REML"
)


# -> Extract spatial eblups and MSE

sae_data$FGT0_SFH <- sfh_model$eblup
sae_data$MSE_SFH  <- sfh_model$mse

# Spatial diagnostic
## intepretation: 𝜌 ≈ 0
## ρ≈0: spatial correlation negligible
## 𝜌 > 0.4  strong spatial clustering

sfh_model$rho

# -> W-orldBank-recommended diagnostics
## Compare direct vs SAE

plot(
  sae_data$fgt0,
  sae_data$FGT0_EBLUP,
  xlab = "Direct FGT0",
  ylab = "EBLUP FGT0"
)
abline(0, 1, col = "red") # package is errorneous

# Coefficient of variation (CV)
## judgement: WB: CV < 25% acceptable

sae_data$CV <- sqrt(sae_data$mse_total) / sae_data$FGT0_EBLUP


# Model diagnostics

summary(fh_model)

# World Bank–style uncertainty reporting
# For each sub-division:

sae_data <- sae_data %>%
  mutate(
    CI_L = FGT0_EBLUP - 1.96 * sqrt(mse_total),
    CI_U = FGT0_EBLUP + 1.96 * sqrt(mse_total)
  )


# Mapping-ready output

poverty_map <- subdiv_sf %>%
  left_join(sae_data, by = "sub_div_id")
