library('tidyverse')
library('tsibble')
library('lubridate')
library('keras')
library('janitor')
library('purrr')
library('reticulate')









# c(scale_value, descale_value) %<-% make_scaler(
#   window(sunspot.month, 1749, c(1992, 12))
# )





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
euro = euro[-1,]
euro = deleteCol (euro, 'euro')

euro2 <- read.csv(file = 'euros/euromillions_2.csv',header = FALSE, sep=';')
euro2 = euro2[-1,]
euro2 = deleteCol (euro2, 'euro2')

euro3 <- read.csv(file = 'euros/euromillions_3.csv',header = FALSE, sep=';')
euro3 = euro3[-1,]
euro3 = deleteCol (euro3, 'euro3')

euro4 <- read.csv(file = 'euros/euromillions_4.csv',header = FALSE, sep=';')
euro4 = euro4[-1,]
euro4 = deleteCol (euro4, 'euro4')


euro5 <- read.csv(file = 'euros/euromillions_5.csv',header = FALSE, sep=';')
euro5 = euro5[-1,]
euro5 = deleteCol (euro5, 'euro5')

euro6 <- read.csv(file = 'euros/euromillions_6.csv',header = FALSE, sep=';')
euro6 = euro6[-1,]
euro6 = deleteCol (euro6, 'euro6')

df <- rbind(euro, euro2)
df <- rbind(df, euro3)
df <- rbind(df, euro4)
df <- rbind(df, euro5)
df <- rbind(df, euro6)

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
row.names(df) <- seq(1:nrow(df))
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))


df$boule_1 <- sapply(df$boule_1, function(x) as.numeric(as.character(x)))
df$boule_2 <- sapply(df$boule_2, function(x) as.numeric(as.character(x)))
df$boule_3 <- sapply(df$boule_3, function(x) as.numeric(as.character(x)))
df$boule_4 <- sapply(df$boule_4, function(x) as.numeric(as.character(x)))
df$boule_5 <- sapply(df$boule_5, function(x) as.numeric(as.character(x)))

df$etoile_1 <- sapply(df$etoile_1, function(x) as.numeric(as.character(x)))
df$etoile_2 <- sapply(df$etoile_2, function(x) as.numeric(as.character(x)))


dfboule1 <- data.frame(index=seq(1:nrow(df)))
#dfboule1$index <- seq(1:nrow(df))
dfboule1$boule_1 <- df$boule_1






train_range <- 1:1000
validation_range <- 1001:1300
testing_range <- 1301:nrow(dfboule1)
timesteps <- 12

data <- dfboule1 %>%
  as_data_frame() %>%
  mutate(key = case_when(
    index %in% train_range ~ "train",
    index %in% validation_range ~ "validation",
    index %in% testing_range ~ "testing"
  ))

scale_data <- function(train, validation, test, feature_range = c(0,1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = (x - min(x)) / (max(x) - min(x))
  std_validation = (validation - min(x)) / (max(x) - min(x))
  std_test = (test - min(x)) / (max(x) - min(x))

  scaled_train = std_train * (fr_max - fr_min) + fr_min
  scaled_validation = std_validation * (fr_max - fr_min) + fr_min
  scaled_test = std_test * (fr_max - fr_min) + fr_min

  return( list(scaled_train = as.vector(scaled_train), scaled_validation = as.vector(scaled_validation), scaled_prediction = as.vector(scaled_test) ,scaler= c(min =min(x), max = max(x))) )
}

training_data <- data %>%
  filter(key == "train")

validation_data <- data %>%
  filter(key == "validation")

prediction_data <- data %>%
  filter(key == "testing")

ScaledData <- scale_data(as.data.frame(training_data$boule_1), as.data.frame(validation_data$boule_1), as.data.frame(prediction_data$boule_1),c(0,1))
x_train_data <- as.data.frame(ScaledData$scaled_train[,1])
x_validation_data <- as.data.frame(ScaledData$scaled_validation[,1])
x_prediction_data <- as.data.frame(ScaledData$scaled_prediction[,1])
names(x_train_data)[1] <- 'scaled_test'
names(x_validation_data)[1] <- 'scaled_test'
names(x_prediction_data)[1] <- 'scaled_test'

scale_val <- rbind(x_train_data[1], x_validation_data[1])
scale_val <- rbind(scale_val[1], x_prediction_data[1])

data <- cbind(data,scale_val)
data <- as.data.frame(data)
names(data)[4] <- 'scaled_value'


make_series <- function(column, start_offset, end_offset) {

  purrr::map(seq_along(column),
             function(x) {
               start <- max(0, x + start_offset)
               end <- max(0, x + end_offset)
               column[start:end]
             })

}

data <- data %>% as.vector() %>% mutate(lookback = as.array(make_series(scaled_value, -timesteps, -1)),
                                        target = as.array(make_series(scaled_value, 0, timesteps - 1)),
                                        target_lag = as.array(make_series(scaled_value, -1, timesteps - 2)))

make_keras_data <- function(data) {
  x <- data$lookback %>%
    array_reshape(c(length(data$lookback), timesteps, 1))
  y_sequence <- data$target %>%
    array_reshape(c(length(data$target), timesteps, 1))
  y_sequence_lag <- data$target_lag %>%
    array_reshape(c(length(data$target_lag), timesteps, 1))
  y <- data$target %>%
    sapply(first)
  list(x = x, y = y, y_sequence = y_sequence,
       y_sequence_lag = y_sequence_lag)
}

training_data <- data %>%
  filter(index > timesteps, key == "train") %>%
  make_keras_data()

validation_data <- data %>%
  filter(key == "validation") %>%
  make_keras_data()

full_training_data <- data %>%
  filter(index > timesteps) %>%
  make_keras_data()

prediction_data <- data %>%
  filter(key == "testing") %>%
  make_keras_data()

model <- keras_model_sequential()

batch_size = 1
epochs <- 1000

model %>%
  layer_lstm (units = 100, input_shape= c(timesteps, 1), name='lstm_0_boule1') %>% 
  layer_dropout(rate = 0.2, name='lstm_dropout_0_boule1') %>%
  layer_dense(timesteps) %>%
  layer_reshape(c(timesteps, 1))

summary(model)

adam <- optimizer_adam(lr = 0.0005)

model %>% compile(
  loss = 'mse',
  optimizer = adam,
  metrics = 'accuracy'
)

history <- model %>% fit(x = full_training_data$x,
                         y = full_training_data$y,
                         epochs = epochs,
                         batch_size=batch_size,
                         verbose = 1,
                         validation_split = 0.1,
                         
                         
)



pred_test <- model %>% predict (prediction_data$x, batch_size = batch_size) 
prediction <- reverse_scaling(as.data.frame(pred_test), ScaledData$scaler)
prediction <- unlist(prediction)

prediction

