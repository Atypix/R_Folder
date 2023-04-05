library(tidyverse)
library(tsibble)

make_series <- function(column, start_offset, end_offset) {

  purrr::map(seq_along(column),
             function(x) {
               start <- max(0, x + start_offset)
               end <- max(0, x + end_offset)
               column[start:end]
             })
}

make_scaler <- function(x) {
  scaled <- scale(x)
  center_value <- attr(scaled, "scaled:center")
  scale_value <- attr(scaled, "scaled:scale")
  list(
    scaler = function(x) {
      force(x)
      scale(x, center_value, scale_value)
    },
    descaler = function(x) {
      force(x)
      x * scale_value + center_value
    }
  )
}

# c(scale_value, descale_value) %<-% make_scaler(
#   window(sunspot.month, 1749, c(1992, 12))
# )

train_range <- 1749:1992
validation_range <- 1993:2002
testing_range <- 2003:2012
timesteps <- 120

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

deleteCol <-function (df, name) {
  
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
  df$date <- sapply(df$date, function(x) format(as.Date(x, "%d/%m/%Y"), "%Y-%m-%d"))
  df$date[df$date=='16-09-23'] <- '2016-09-23' 
  return (df)
}

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

df <- rbind(euro2, euro3)
df <- rbind(df, euro4)
df <- rbind(df, euro5)
df <- rbind(df, euro6)

df <- df[order(as.Date(df$date, format="%Y-%m-%d")),]
df$date <- sapply(df$date, function(x) as.numeric(as.Date(as.character(x), origin="1970-01-01")))


df$boule_1 <- sapply(df$boule_1, function(x) as.numeric(as.character(x)))
df$boule_2 <- sapply(df$boule_2, function(x) as.numeric(as.character(x)))
df$boule_3 <- sapply(df$boule_3, function(x) as.numeric(as.character(x)))
df$boule_4 <- sapply(df$boule_4, function(x) as.numeric(as.character(x)))
df$boule_5 <- sapply(df$boule_5, function(x) as.numeric(as.character(x)))

df$etoile_1 <- sapply(df$etoile_1, function(x) as.numeric(as.character(x)))
df$etoile_2 <- sapply(df$etoile_2, function(x) as.numeric(as.character(x)))