library('quantmod')
library('caret')
library('reticulate')
library('listarrays')
library('keras')
library('deepviz')
library('magrittr')
#setwd('/home/goach/Documents/workspace/R_Folder/trading/')
#install_keras()
#R316QG2CEEQW3T0U
#tensorflow::tf$compat$v1$disable_eager_execution()
np <- import("numpy", convert=FALSE)
message('Get datas Alpha Vantage')
#getSymbols("ATO", from=Sys.Date(), src="av", api.key="R316QG2CEEQW3T0U", periodicity="intraday", interval="1min", output.size="compact")
getSymbols("ATO", from="2020-01-01", src="av", api.key="R316QG2CEEQW3T0U", periodicity="daily", output.size="full")
head(ATO)

history_points <- 50

message('Normalise datas')

datas <- as.data.frame(ATO)
data_normaliser <- preProcess(datas[,1:5], method=c("range"))
data_normalised <- predict(data_normaliser, ATO[,1:5])



message('Get the dataset ready for model consumption.')
ohlcv_histories_normalised <- array()
next_day_open_values_normalised <- array()
next_day_open_values <- array()



for(i in seq(1:(nrow(data_normalised) - history_points))) {

  ohlcv_histories <- np$array(array(data_normalised[i  : i + history_points]))
  (ohlcv_histories_r <- py_to_r(ohlcv_histories))
  ohlcv_histories_normalised <- rbind(ohlcv_histories_normalised, ohlcv_histories_r)
  
  next_day <- np$array(array(data_normalised[,1:5][i  : i + history_points]))
  (next_day_r <- py_to_r(next_day))
  next_day_open_values_normalised <- rbind(next_day_open_values_normalised, next_day_r)

}

for(i in seq(1:(nrow(datas) - history_points))) {
  next_day_values <- np$array(array(ATO[,1:5][i  : i + history_points]))
  (next_day_values_r <- py_to_r(next_day_values))
  next_day_open_values <- rbind(next_day_open_values, next_day_values_r) 
}



message('-> Expand_dims')
next_day_open_values_normalised <- expand_dims(next_day_open_values_normalised , -1)
next_day_open_values = expand_dims(next_day_open_values_normalised, -1)

next_day_open_values_normalised[is.na(next_day_open_values_normalised)] <- 0
next_day_open_values[is.na(next_day_open_values)] <- 0
ohlcv_histories_normalised[is.na(ohlcv_histories_normalised)] <- 0

y_normaliser <- preProcess(as.data.frame(next_day_open_values), method=c("range"))
y_normaliser <- predict(y_normaliser, as.data.frame(next_day_open_values))

if (nrow(ohlcv_histories_normalised) == nrow(next_day_open_values_normalised)) {
  message("the number of x’s == the number of y’s")
}
ohlcv_histories <- ohlcv_histories_normalised
unscaled_y <- next_day_open_values

test_split = 0.8
n = as.integer(nrow(ohlcv_histories) * test_split)

ohlcv_train = array(data = ohlcv_histories, dim=c(5, ohlcv_histories, 2))
y_train = array(data = next_day_open_values,dim=c(5, next_day_open_values, 1))

ohlcv_test = ohlcv_histories[n:nrow(ohlcv_histories)]
y_test = next_day_open_values[n:nrow(ohlcv_histories)]

unscaled_y_test = unscaled_y[n:nrow(unscaled_y)]

#https://github.com/tensorflow/tensorflow/issues/4078
model <- keras_model_sequential()
model %>% layer_lstm (units = 50,
                      input_shape = c(history_points, 5),batch_size = 1, activation = 'linear', name='lstm_input', return_sequences = TRUE,
                      stateful = TRUE)
model %>% layer_dropout(rate = 0.2, name='lstm_dropout_0')
model %>% layer_dense(units = 64, activation = 'sigmoid', name='dense_0')
model %>% layer_dense(units = 1, name='dense_1')



summary(model)
#model %>% layer_activation(activation = "linear")
adam <- optimizer_adam(lr = 0.0005)

model %>% compile(
  loss = 'mse',
  optimizer = adam,
  metrics = 'accuracy'
)
dim(ohlcv_train) <- c(length(ohlcv_train), 1,1)
history <- model %>% fit(
  x = ohlcv_train,
  y = y_train,
  epochs = 50,
  batch_size = 1,
  validation_split = 0.1,
  verbose = 2
)
#https://www.pugetsystems.com/labs/hpc/How-to-install-CUDA-9-2-on-Ubuntu-18-04-1184/


plot(history)

