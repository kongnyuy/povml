library(randomForest)
library(dplyr)

# Train on DHS
dhs_train <- dhs %>%
  select(FGT0, hhsize, urban, education, toilet, water, floor, nightlights, pop_density)

rf_model <- randomForest(FGT0 ~ ., data = dhs_train, ntree = 1000)

# Predict on Census
census_pred <- census %>%
  select(hhsize, urban, education, toilet, water, floor, nightlights, pop_density)

census$FGT0_pred <- predict(rf_model, newdata = census_pred)

# Aggregate at divisional level
poverty_division <- census %>%
  group_by(sub_div_id) %>%
  summarise(FGT0_div = mean(FGT0_pred))

# Aggregate at regional level
poverty_region <- census %>%
  group_by(region_id) %>%
  summarise(FGT0_reg = mean(FGT0_pred))


# -> UNCERTAINTY QUANTIFICATION
# Bootstrap DHS samples → train multiple ML models → generate prediction distribution

# Monte Carlo aggregation → compute FGT indicators at divisional / regional levels

# Quantile regression forests → confidence intervals for predictions


#library(quantregForest)

# qrf_model <- quantregForest(X, y, ntree = 1000)
# pred_q <- predict(qrf_model, newdata = X_new, what = c(0.05, 0.5, 0.95))
