library('reticulate')
library('listarrays')
library('keras')
library('data.table')
library('dplyr')
library('tidyverse')
library('writexl')
library('caret')
#setwd('/home/goach/Documents/workspace/R_Folder/EUROMILLIONS/')
set.seed(3033)

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

euro5 <- read.csv(file = 'euros/euromillions_5.csv',header = FALSE, sep=';')
euro5 = euro5[-1,]
euro5 = deleteCol (euro5, 'euro5')

euro6 <- read.csv(file ='euros/euromillions_202002.csv',header = FALSE, sep=';')
euro6 = euro6[-1,]
euro6 = deleteCol (euro6, 'euro6')

df <- rbind(euro5, euro6)


df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))
df <- df[114:184,]



df$boule_3 <- sapply(df$boule_3, function(x) as.numeric(as.character(x)))

boule = as.data.frame(df$boule_3)
names(boule)[names(boule) == "boule_3"] <- "boule"
training_size=floor(0.9*length(boule))
boule.train <- boule[1:training_size]
boule.test <- boule[training_size+1:length(boule)-training_size]

data_preprocess_value=preProcess(boule.train, method=c("center", "scale"))
boule.train.scaled = predict(data_preprocess_value, boule.train)
boule.test.scaled = predict(data_preprocess_value, boule.test)

build_matrix <- function(tseries, overall_timesteps) {
  t(sapply(1:(length(tseries) - overall_timesteps + 1), 
           function(x) tseries[x:(x + overall_timesteps - 1)]))
}
reshape_X_3d <- function(X) {
  dim(X) <- c(dim(X)[1], dim(X)[2], 1)
  X
}


n_timesteps <- 34
n_predictions <- 34
n_features <- 1
batch_size <- 1
epochs <- 2000000

train_matrix_boule <- build_matrix(as.matrix(boule.train.scaled), n_timesteps + n_predictions)
test_matrix_boule <- build_matrix(as.matrix(boule.test.scaled), n_timesteps + n_predictions)

X_train_boule <- train_matrix_boule[, 1:n_timesteps]
y_train_boule <- train_matrix_boule[, (n_timesteps + 1):(n_timesteps * 2)]

X_test_boule <- test_matrix_boule[, 1:n_timesteps]
y_test_boule <- test_matrix_boule[, (n_timesteps + 1):(n_timesteps * 2)]

X_train_boule <- X_train_boule[1:(nrow(X_train_boule) %/% batch_size * batch_size),]
y_train_boule <- y_train_boule[1:(nrow(y_train_boule) %/% batch_size * batch_size),]
X_test_boule <- X_test_boule[1:(nrow(X_test_boule) %/% batch_size * batch_size),]
y_test_boule <- y_test_boule[1:(nrow(y_test_boule) %/% batch_size * batch_size),]

X_train_boule <- reshape_X_3d(X_train_boule)
y_train_boule <- reshape_X_3d(y_train_boule)
X_test_boule <- reshape_X_3d(X_test_boule)
y_test_boule <- reshape_X_3d(y_test_boule)

model <- keras_model_sequential()

model  %>%
  layer_lstm (units = 100, name='lstm_0') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout') %>% 
  layer_dense(n_timesteps) %>%
  layer_reshape(c(n_timesteps, 1))%>% 
  layer_dense(1) 

adam <- optimizer_adam(lr = 0.0005)

message(">> compilation du model")
model %>% compile(
  loss = 'mse',
  optimizer = adam
)
model <- load_model_hdf5('boule_3.h5', custom_objects = NULL, compile = TRUE)
history <- model %>% fit(x = X_train_boule,
                         y = y_train_boule,
                         epochs = epochs,
                         batch_size=batch_size,
                         initial_epoch = 500000,
                         verbose = 1,
                         validation_split= 0.0001,
                         shuffle=FALSE
)

pred_test <- model %>% predict(y_test_boule, verbose = 1, batch_size=batch_size)
pred <- as.matrix(pred_test)
pred <- as.data.frame(pred)
names(pred)[1]<-paste("df$boule_3")
result <- as.data.frame(revPredict(data_preprocess_value, as.data.frame(pred)))
result



