library('tidyverse')
library('tsibble')
library('lubridate')
library('keras')
library('janitor')
library('purrr')
library('reticulate')
library('tidyr')


n_timesteps <- 7
n_predictions <- 7
n_features <- 1
batch_size <- 1

epochs <- 2000
deleteCol <-function (df, name) {
  if (name == 'euro') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro2') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro3') {
    df[1:2] <- NULL
    df[2] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro4') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro5') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  if (name == 'euro6') {
    df[1:2] <- NULL
    df[2:3] <- NULL
    df[9:ncol(df)] <- NULL 
  }
  names(df)[1] <- 'date'
  names(df)[2] <- 'boule_1'
  names(df)[3] <- 'boule_2'
  names(df)[4] <- 'boule_3'
  names(df)[5] <- 'boule_4'
  names(df)[6] <- 'boule_5'
  names(df)[7] <- 'etoile_1'
  names(df)[8] <- 'etoile_2'
  if(name == 'euro') {
    df$date <- sapply(df$date, function(x) format(as.Date(x, "%Y%m%d"), "%Y-%m-%d"))
  } else {
    df$date <- sapply(df$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))
  }
  df$date[df$date=='16-09-23'] <- '2016-09-23' 
  return (df)
}

euro <- read.csv(file = 'euros/euromillions.csv',header = FALSE, sep=';')
euro <- euro[-1,]
euro <- deleteCol (euro, 'euro')
euro <- euro %>% gather(euro, boule, 2:8)


euro2 <- read.csv(file = 'euros/euromillions_2.csv',header = FALSE, sep=';')
euro2 <- euro2[-1,]
euro2 <- deleteCol (euro2, 'euro2')
euro2 <- euro2 %>% gather(euro, boule, 2:8)

euro3 <- read.csv(file = 'euros/euromillions_3.csv',header = FALSE, sep=';')
euro3 <- euro3[-1,]
euro3 <- deleteCol (euro3, 'euro3')
euro3 <- euro3 %>% gather(euro, boule, 2:8)

euro4 <- read.csv(file = 'euros/euromillions_4.csv',header = FALSE, sep=';')
euro4 <- euro4[-1,]
euro4 <- deleteCol (euro4, 'euro4')
euro4 <- euro4 %>% gather(euro, boule, 2:8)

euro5 <- read.csv(file = 'euros/euromillions_5.csv',header = FALSE, sep=';')
euro5 <- euro5[-1,]
euro5 <- deleteCol (euro5, 'euro5')
euro5 <- euro5 %>% gather(euro, boule, 2:8)

euro6 <- read.csv(file = 'euros/euromillion_6.csv',header = FALSE, sep=';')
euro6 <- euro6[-1,]
euro6 <- deleteCol (euro6, 'euro6')
euro6 <- euro6 %>% gather(euro, boule, 2:8)

df <- rbind(euro, euro2)
df <- rbind(df, euro3)
df <- rbind(df, euro4)
df <- rbind(df, euro5)
df <- rbind(df, euro6)

row.names(df) <- seq(1:nrow(df))

df$boule <- sapply(df$boule, function(x) as.numeric(as.character(x)))

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))
datas <- df
n_ <- round(nrow(datas) - (7*n_timesteps), digits = 0)
train <- datas[1:n_, ] 
test <- datas[(n_+1):nrow(datas),] 
row.names(train) <- seq(1:nrow(train))
row.names(test) <- seq(1:nrow(test))

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

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), function(x) 
    tseries[x:(x + overall_timesteps -1)]))
}

reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}

date_train <- train$date
date_test <- test$date
boule <- train$boule
boule_test <- test$boule

ScaledDate <- scale_data(as.data.frame(date_train), as.data.frame(date_test), c(0,1))
x_train_date <- ScaledDate$scaled_train[,1]
x_test_date <- ScaledDate$scaled_test[,1]

ScaledBoule <- scale_data(as.data.frame(boule), 
                            as.data.frame(boule_test),
                            c(0,1))

x_train_boule <- ScaledBoule$scaled_train[,1]
x_test_boule <- ScaledBoule$scaled_test[,1]

train_matrix_date <- build_matrix(x_train_date, n_timesteps + n_predictions)
test_matrix_date <- build_matrix(x_test_date, n_timesteps + n_predictions)

X_train_date <- train_matrix_date[, 1:n_timesteps]
y_train_date <- train_matrix_date[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_date <- test_matrix_date[, 1:n_timesteps]
y_test_date <- test_matrix_date[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_date <- X_train_date[1:(nrow(X_train_date) %/% batch_size * batch_size),]
y_train_date <- y_train_date[1:(nrow(y_train_date) %/% batch_size * batch_size),]
X_test_date <- X_test_date[1:(nrow(X_test_date) %/% batch_size * batch_size),]
y_test_date <- y_test_date[1:(nrow(y_test_date) %/% batch_size * batch_size),]

train_matrix_boule <- build_matrix(x_train_boule, n_timesteps + n_predictions)
train_matrix_boule_test <- build_matrix(x_test_boule, n_timesteps + n_predictions)



X_train_boule <- train_matrix_boule[, 1:n_timesteps]
y_train_boule <- train_matrix_boule[, (n_timesteps + 1):(n_timesteps * 2)]
X_test_boule <- train_matrix_boule_test[, 1:n_timesteps]
y_test_boule <- train_matrix_boule_test[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule <- X_train_boule[1:(nrow(X_train_boule) %/% batch_size * batch_size),]
y_train_boule <- y_train_boule[1:(nrow(y_train_boule) %/% batch_size * batch_size),]
X_test_boule <- X_test_boule[1:(nrow(X_test_boule) %/% batch_size * batch_size),]
y_test_boule <- y_test_boule[1:(nrow(y_test_boule) %/% batch_size * batch_size),]

X_train_date <-  reshape_X_3d(X_train_date)
y_train_date <-  reshape_X_3d(y_train_date)

X_train_boule <- reshape_X_3d(X_train_boule)
y_train_boule <- reshape_X_3d(y_train_boule)
X_test_boule <- reshape_X_3d(X_test_boule)
y_test_boule <- reshape_X_3d(y_test_boule)

message(">> creation du model")

model <- keras_model_sequential()

model %>%
  layer_lstm (units = 100, input_shape= c(n_timesteps, 1), name='lstm_0_boule1', activation='linear') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule1') %>%
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))

adam <- optimizer_adam(lr = 0.0005)


#summary(model)


message(">> compilation du model")
model %>% compile(
  loss = 'mse',
  optimizer = adam
)

# history <- model %>% fit(x = X_train_boule,
#                          y = y_train_boule,
#                          epochs = epochs,
#                          batch_size=batch_size,
#                          verbose = 1,
#                          validation_split = 0.1,
#                          shuffle=FALSE
# )

for (i in 1:epochs) {
  model %>% fit(X_train_boule, y_train_boule,batch_size = batch_size,
                epochs = 1, verbose = 1, shuffle = FALSE)
  
  model %>% reset_states()
}

pred_test <- model %>% predict(X_test_boule, verbose = 1, batch_size=batch_size)

#date_predict <- unlist(reverse_scaling(as.data.frame(pred_test[1]), ScaledDate$scaler))
boule <- unlist(reverse_scaling(as.data.frame(pred_test), ScaledBoule$scaler))

view_test <- boule

view_test <- as.data.frame(view_test)

names(view_test)[1] <- 'boule_test'

#view_test$date <- sapply(view_test$date, function(x) format(as.Date(as.numeric(as.character(x)), origin="1970-01-01"), "%Y-%m-%d"))