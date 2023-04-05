library('quantmod')
library('caret')
library('reticulate')
library('listarrays')
library('keras')
use_condaenv("tensorflow")
np <- import("numpy", convert=FALSE)

message('Get datas Alpha Vantage')

getSymbols("ATO", from="2020-01-01", src="av", api.key="R316QG2CEEQW3T0U", periodicity="daily", output.size="full")
head(ATO)

history_points <- 50

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

message('Normalise datas')
K <- backend()
datas <- as.data.frame(ATO)
diff <- diff(datas$ATO.Open, lag = history_points)
supervised <- as.data.frame(cbind(lag(datas$ATO.Open,1), datas$ATO.Open + diff))
newdatas <- cbind(datas, supervised[2])


n_ <- round(nrow(newdatas) * .85, digits = 0)
train <- newdatas[1:n_, ]
test <- newdatas[(n_+1):nrow(newdatas),]

message("normalisation des datas")
Scaled <- scale_data(train, test, c(0,1))
x_train <- Scaled$scaled_train[,1:5]
y_train <- Scaled$scaled_train[,6]
message(dim(x_train))
x_train <- K$eval(x_train)
message('test2')
# train_id <- dateID[1:n_]
# test_id <- dateID[(n_+1):nrow(datas)]

model <- keras_model_sequential()

model %>% layer_lstm (units = 50,
                      input_shape = c(history_points, 5), name='lstm_input')
model %>% layer_dropout(rate = 0.2, name='lstm_dropout_0')
model %>% layer_dense(units = 64, name='dense_0')
model %>% layer_activation( activation = 'sigmoid',  name='sigmoid_0')
model %>% layer_dense(units = 1, name='dense_1')
summary(model)
adam <- optimizer_adam(lr = 0.0005)

model %>% compile(
  loss = 'mse',
  optimizer = 'adam',
  metrics = 'accuracy'
)




cat('Training\n')

  history <- model %>% fit(x = x_train,
                           y = y_train,
                           batch_size=32,
                           epochs = 50,
                           verbose = 1,
                           validation_split = 0.1,
                           shuffle = TRUE)
  

  

