library('quantmod')
library('caret')
library('reticulate')
library('listarrays')
library('keras')
library('data.table')
library('dplyr')
library('tidyverse')

message('>> Get datas Alpha Vantage')

getSymbols("AIR.PAR", from="2020-01-01", src="av", api.key="R316QG2CEEQW3T0U", periodicity="daily", output.size="full")
head(AIR.PAR)

n_timesteps <- 12
n_predictions <- n_timesteps
n_features <- 1
batch_size <- 1

datas <- as.data.frame(AIR.PAR)

message(">> Gestion des dates")
d <- as.data.frame(AIR.PAR)
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
train <- train[1]
test <- test[1]

date_train <- date[1:n_,1 ]
date_test <- date[(n_+1):nrow(date),1]

message(">> normalisation des datas")
Scaled <- scale_data(as.data.frame(train), as.data.frame(test), c(0,1))
x_train <- Scaled$scaled_train[,1]
x_test <- Scaled$scaled_test[,1]


train_matrix <- build_matrix(x_train, n_timesteps + n_predictions)
test_matrix <- build_matrix(x_test, n_timesteps + n_predictions)

X_train <- train_matrix[, 1:n_timesteps]
y_train <- train_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_test <- test_matrix[, 1:n_timesteps]
y_test <- test_matrix[, (n_timesteps + 1):(n_timesteps * 2)]

X_train <- reshape_X_3d(X_train)
y_train <- reshape_X_3d(y_train)

X_test <- reshape_X_3d(X_test)
y_test <- reshape_X_3d(y_test)

message(">> creation du model")
model <- keras_model_sequential()

model %>% layer_lstm (units = 100,
                      input_shape = c(n_timesteps, n_features), name='lstm_input')
model %>% layer_dropout(rate = 0.2, name='lstm_dropout_0')
model %>% layer_dense(units = 64, name='dense_0')
model %>% layer_activation( activation = 'sigmoid',  name='sigmoid_0')
model %>% layer_dense(units = 1, name='dense_1')
summary(model)
adam <- optimizer_adam(lr = 0.0005)

message(">> compilation du model")
model %>% compile(
  loss = 'mse',
  optimizer = 'adam'
)




message(">> Training")

history <- model %>% fit(x = X_train,
                         y = y_train,
                         epochs = 200,
                         bach_size=32,
                         verbose = 1
                         ,validation_split = 0.1,
                         shuffle=TRUE)

plot(history, metrics="loss")

message(">> Prédiction des tests")

pred_test <- model %>% predict(X_test, verbose = 1, bach_size=32)

pred_test <- reverse_scaling (pred_test, Scaled$scaler)

dftest <- as.data.frame(pred_test)
dfdatetest <-  as.data.frame(date_test)
dftest <- cbind(as.data.frame(pred_test), as.data.frame(date_test) %>% slice(n_timesteps:(n_timesteps + nrow(dftest) - 1)))
names(dftest)[2] <- 'date'
dfReal <- cbind(datas, date)
dfReal$AIR.PAR.High <- NULL
dfReal$AIR.PAR.Low <- NULL
dfReal$AIR.PAR.Close <- NULL
dfReal$AIR.PAR.Volume <- NULL
names(dfReal)[1] <- 'open'
names(dfReal)[2] <- 'date'
dplr <- left_join(dfReal, dftest, by=c("date"))
dfGraph <- na.omit(dplr)

b <- ggplot(dfGraph, aes(x=as.Date(date)))
b <- b + geom_line(aes(y = open))
b <- b + geom_line(aes(y = pred_test), color = "red")
print(b)


