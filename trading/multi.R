library('quantmod')
library('caret')
library('reticulate')
library('listarrays')
library('keras')
library('data.table')
library('dplyr')
library('tidyverse')
set.seed(4)
message('>> Get datas Alpha Vantage')

getSymbols("ATO.PAR", from="2020-01-01", src="av", api.key="R316QG2CEEQW3T0U", periodicity="daily", output.size="full")
head(ATO.PAR)

n_timesteps <- 12
n_predictions <- n_timesteps
n_features <- 1
batch_size <- 6

epochs <- 200

datas <- as.data.frame(ATO.PAR)

message(">> Gestion des dates")
d <- as.data.frame(ATO.PAR)
date <- as.data.frame(rownames(d))



scale_data <- function(train, test, feature_range = c(0,1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = (x - min(x)) / (max(x) - min(x))
  std_test = (test - min(x)) / (max(x) - min(x))
  
  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_test = std_test * (fr_max - fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}



reverse_scaling <- function(scaled, scaler, feature_range = c(0,1)) {
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for(i in 1:t) {
    X = (scaled[i] - mins) / (maxs - mins)
    rawValues = X * (max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps - 1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}
message('>> split datas')
n_ <- round(nrow(datas) * .9, digits = 0)



train <- datas[1:n_, ]
test <- datas[(n_+1):nrow(datas),]
close <- SMA(train[4], n_timesteps)
close[is.na(close)] <- 0

volume <- EVWMA(train[4], train[5], n_timesteps)
volume[is.na(volume)] <- 0

closetest <- SMA(datas[4], n_timesteps)
closetest <- closetest[(n_+1):nrow(datas),]

volumetest <- EVWMA(datas[4],datas[5], n_timesteps)
volumetest <- volumetest[(n_+1):nrow(datas),]

train <- train[1]
test <- test[1]

date_train <- date[1:n_,1 ]
date_test <- date[(n_+1):nrow(date),1]

message(">> normalisation des datas")
Scaled <- scale_data(as.data.frame(train), as.data.frame(test), c(0,1))
x_train <- Scaled$scaled_train[,1]
x_test <- Scaled$scaled_test[,1]

Scaledclose <- scale_data(as.data.frame(close), as.data.frame(closetest), c(0,1))
x_close <- Scaledclose$scaled_train[,1]
x_test_close <- Scaledclose$scaled_test[,1]

Scaledvolume <- scale_data(as.data.frame(volume), as.data.frame(volumetest), c(0,1))
x_volume <- Scaledclose$scaled_train[,1]
x_test_volume <- Scaledclose$scaled_test[,1]

train_matrix <- build_matrix(x_train, n_timesteps + n_predictions)
test_matrix <- build_matrix(x_test, n_timesteps + n_predictions)
close_matrix <- build_matrix(x_close, n_timesteps + n_predictions)
close_test_matrix <- build_matrix(x_test_close, n_timesteps + n_predictions)
volume_matrix <- build_matrix(x_volume, n_timesteps + n_predictions)
volume_test_matrix <- build_matrix(x_test_volume, n_timesteps + n_predictions)
  
X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_close <- close_matrix[, 1:n_timesteps]
y_close <- close_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_volume <- volume_matrix[, 1:n_timesteps]
y_volume <- volume_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_test_close <- close_test_matrix[, 1:n_timesteps]
y_test_close <- close_test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_test_volume <- volume_test_matrix[, 1:n_timesteps]
y_test_volume <- volume_test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_test_close <-  reshape_X_3d(X_test_close)
y_test_close <- reshape_X_3d(y_test_close)

X_test_volume <-  reshape_X_3d(X_test_volume)
y_test_volume <- reshape_X_3d(y_test_volume)

X_close <-  reshape_X_3d(X_close)
y_close <-  reshape_X_3d(y_close)

X_volume <-  reshape_X_3d(X_volume)
y_volume <-  reshape_X_3d(y_volume)

X_train <- reshape_X_3d(X_train)
y_train <- reshape_X_3d(y_train)

X_test <- reshape_X_3d(X_test)
y_test <- reshape_X_3d(y_test)



message(">> creation du model")



inputsLSTM <- layer_input(shape = c(n_timesteps, n_features), name='lstm_input')

outputLSTM <- inputsLSTM  %>%
  layer_lstm (units = 100, name='lstm_0') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0')

modelLSTM <- keras_model(
  inputs = inputsLSTM,
  outputs = outputLSTM
)

inputs_auxilliaire <- layer_input (shape = c(dim(X_close)[2]), name='close_input') 

outputAuxilliaire <- inputs_auxilliaire %>%
  layer_dense(units = 20, name='close_dense_0') %>% 
  layer_activation(activation = 'relu', name='close_relu_0') %>% 
  layer_dropout(rate = 0.2, name='close_dropout_0')

modelAuxilliaire <- keras_model(
  inputs = inputs_auxilliaire,
  outputs = outputAuxilliaire
)

inputs_volume <- layer_input (shape = c(dim(X_volume)[2]), name='volume_input') 

outputVolume <- inputs_volume %>%
  layer_dense(units = 20, name='volume_dense_0') %>% 
  layer_activation(activation = 'relu', name='volume_relu_0') %>% 
  layer_dropout(rate = 0.2, name='volume_dropout_0')

modelVolume <- keras_model(
  inputs = inputs_volume,
  outputs = outputVolume
)

combined <- layer_concatenate(c(outputLSTM, outputAuxilliaire, outputVolume), name='concatenate') %>%
            layer_dense(units = 64, name='combined_dense_0') %>%
            layer_activation( activation = 'sigmoid',  name='sigmoid_0') %>% 
            layer_dense(units = 1, name='dense_1', activation="linear")

model <- keras_model(
  inputs = c(modelLSTM$input, modelAuxilliaire$input, modelVolume$input),
  outputs = c(combined)
)

summary(model)

adam <- optimizer_adam(lr = 0.0005)
 
message(">> compilation du model")
model %>% compile(
  loss = 'mse',
  optimizer = 'adam'
)




message(">> Training")

history <- model %>% fit(x = list(X_train, X_close, X_volume),
                         y = y_train,
                         epochs = epochs,
                         batch_size=batch_size,
                         verbose = 1,
                         validation_split = 0.1,
                         shuffle=TRUE
                         )

plot(history, metrics="loss")

message(">> Prédiction des tests")

pred_test <- model %>% predict(list(X_test, X_test_close, X_test_volume), verbose = 1, batch_size=batch_size)

pred_test <- reverse_scaling (pred_test, Scaled$scaler)

dftest <- as.data.frame(pred_test)
dfdatetest <-  as.data.frame(date_test)
dftest <- cbind(as.data.frame(pred_test), as.data.frame(date_test) %>% slice(n_timesteps:(n_timesteps + nrow(dftest) - 1)))
names(dftest)[2] <- 'date'
dfReal <- cbind(datas, date)
dfReal$ATO.PAR.High <- NULL
dfReal$ATO.PAR.Low <- NULL
dfReal$ATO.PAR.Close <- NULL
dfReal$ATO.PAR.Volume <- NULL
names(dfReal)[1] <- 'open'
names(dfReal)[2] <- 'date'
dplr <- left_join(dfReal, dftest, by=c("date"))
dfGraph <- na.omit(dplr)

b <- ggplot(dfGraph, aes(x=as.Date(date)))
b <- b + geom_line(aes(y = open))
b <- b + geom_line(aes(y = pred_test), color = "red")
print(b)

eval <- model %>% evaluate(list(X_test, X_test_close, X_test_volume), y_test,verbose = 0)
message(paste('Pourcentage de fiabilité sur la pérode : ', (100 - (eval*100))))


