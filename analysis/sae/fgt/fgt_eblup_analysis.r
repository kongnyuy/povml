# --------------------
# using the area level Fay-Herriot model
# --------------------

install.packages("sae")
library(sae)

# Step 1: Direct poverty estimates + variances


direct_fgt0 <- svyby(
  ~ I(welfare < z),
  ~ sub_div_id,
  dhs_design,
  svymean,
  vartype = "se"
)

direct_fgt0 <- direct_fgt0 %>%
  mutate(
    vardir = se^2,
    fgt0 = `I(welfare < z)`
  ) %>%
  select(sub_div_id, fgt0, vardir)


# auxillary data that contains the covariates

# [TODO] wrangle the data to only contain useful covariates: pop_density, school_rate (education coverage), nightlights
aux <- read.csv("aux_subdivision.csv")


# Merge direct + auxiliary data
sae_data <- direct_fgt0 %>%
  left_join(aux, by = "sub_div_id")


# Fit Fay–Herriot model (EBLUP)

fh_model <- eblupFH(
  fgt0 ~ urban_share + nightlights + pop_density,
  vardir = sae_data$vardir,
  data = sae_data,
  method = "REML"
)

# Extract EBLUP estimates and MSE

sae_data$FGT0_EBLUP <- fh_model$eblup
sae_data$MSE_EBLUP  <- fh_model$mse


# Map-ready output

subdiv_map <- subdiv %>%
  left_join(sae_data, by = "sub_div_id")


# the above dataset contains enough data for doing the following: Map smoothed poverty, Rank sub-divisions, Produce confidence intervals

## Producing corressponding FGT1 and FGT2 model and estimates

# @[TODO] FGT1 and FGT2



# As required by small area theory, model validation is necessary.
# Down below, we report MSE estimates decomposed as required by statistics offices

#library(sae)

# FGT0

fh_model <- eblupFH(
  fgt0 ~ urban_share + nightlights + pop_density,
  vardir = sae_data$vardir,
  data = sae_data,
  method = "REML"
)


# extract MSE components

mse_comp <- mseFH(
  fh_model,
  method = "PR"  # Prasad–Rao (1990)
)

sae_data$mse_total <- mse_comp$mse
sae_data$g1 <- mse_comp$g1
sae_data$g2 <- mse_comp$g2
sae_data$g3 <- mse_comp$g3


# Relative standard error as required by world bank standards

sae_data$RSE <- sqrt(sae_data$mse_total) / sae_data$FGT0_EBLUP

# descesion criteria: 
## WB guideline: RSE < 20% is considered reliable
## RSE < 10% is excellent