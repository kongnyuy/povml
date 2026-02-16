# ---------------------
# BAYESIAN SPATIAL SAE USING INLA
# --------------------
## this is a Modern alternative to FH


# Data preparation

library(INLA)

sae_data$idx <- as.numeric(as.factor(sae_data$sub_div_id))


# Adjacency matrix

nb <- poly2nb(subdiv_sf)
W <- nb2mat(nb, style = "B")


# INLA model

formula <- fgt0 ~ urban_share + nightlights +
  f(idx, model = "besag", graph = W)

result <- inla(
  formula,
  family = "gaussian",
  data = sae_data,
  control.predictor = list(compute = TRUE),
  control.compute = list(dic = TRUE, waic = TRUE)
)


# Posterior estimates

sae_data$FGT0_INLA <- result$summary.fitted.values$mean
sae_data$SD_INLA  <- result$summary.fitted.values$sd


# ---------------
# Model checking and validation
# ---------------

# 1. Direct vs SAE

cor(sae_data$fgt0, sae_data$FGT0_EBLUP)

# 2. coefficient of Variation (CV) threshold. remember: WB rule: CV < 25% for good estimates

sae_data$CV <- sqrt(sae_data$mse_total) / sae_data$FGT0_EBLUP

# -> Poverty map creation

poverty_map <- subdiv_sf %>%
  left_join(sae_data, by = "sub_div_id")
