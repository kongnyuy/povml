# Gradient boosting (XGBoost / LightGBM)

library(xgboost)

X <- as.matrix(dhs_train %>% select(-FGT0))
y <- dhs_train$FGT0

xgb_model <- xgboost(data = X, label = y, nrounds = 500, max_depth = 6, eta = 0.05)
