# Neural network model using keras

library(keras)

model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = ncol(X)) %>%
  layer_dropout(0.3) %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = "adam",
  loss = "mse"
)

model %>% fit(X, y, epochs = 50, batch_size = 32, validation_split = 0.2)
