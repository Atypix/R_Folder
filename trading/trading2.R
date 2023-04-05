
library('quantmod')
library('caret')
library('reticulate')
library('listarrays')
library('keras')
library('deepviz')
library('magrittr')


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

getSymbols("ATO", from="2020-01-01", src="av", api.key="R316QG2CEEQW3T0U", periodicity="daily", output.size="full")
head(ATO)

datas <- as.data.frame(ATO)
d <- as.data.frame(ATO)
date <- rownames(d)
rownames(d) <- NULL
datas <- cbind(date,d)
dateID <- datas$date[2:nrow(datas)]
diff <- diff(datas$ATO.Open, differences = 1)
supervised <- as.data.frame(cbind(lag(datas$ATO.Open,1), datas$ATO.Open + diff))

n_ <- round(nrow(datas) * .85, digits = 0)
train <- supervised[1:n_, ]
test <- supervised[(n_+1):nrow(supervised),]
train_id <- dateID[1:n_]
test_id <- dateID[(n_+1):nrow(supervised)]

Scaled <- scale_data(train, test, c(-1,1))

x_train <- Scaled$scaled_train[,1]
y_train <- Scaled$scaled_train[,2]

x_test <- Scaled$scaled_test[,1]
y_test <- Scaled$scaled_test[,2]

dim(x_train) <- c(length(x_train), 1,1)
X_shape2 <- dim(x_train)[2]
X_shape3 <- dim(x_train)[3]

batch_size <- 2
units <- 100
n_timesteps <- 12
n_predictions <- n_timesteps

model <- keras_model_sequential()
model %>% layer_lstm (units, 
                      batch_input_shape = c(batch_size, X_shape2, X_shape3), 
                      name='lstm_input')
model %>% layer_dropout(rate = 0.2, name='lstm_dropout_0')
model %>% layer_dense(units = 64, activation = 'sigmoid', name='dense_0')
model %>% layer_dense(units = 1, name='dense_1',activation = 'linear')



summary(model)
#model %>% layer_activation(activation = "linear")
adam <- optimizer_adam(lr = 0.0005)

model %>% compile(
  loss = 'mse',
  optimizer = 'adam',
  metrics = 'accuracy'
)

cat('Training\n')
epochs <- 200
for (i in 1:epochs) {
  history <- model %>% fit(x = x_train,
                           y = y_train,
                           batch_size = batch_size,
                           epochs = 1,
                           verbose = 1,
                           validation_split = 0.1,
                           shuffle = FALSE)
  
  model %>% reset_states()
  
}


pred_out <- model %>% predict (x_train, batch_size = 2)

test <- reverse_scaling (pred_out, Scaled$scaler, c(-1, 1))

View(test)