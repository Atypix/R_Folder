library(lubridate)
library(keras)
make_series <- function(column, start_offset, end_offset) {
  # Given a time series, return a list
  #  where each element is a vector representing a window
  #  of the time series determined by the offsets
  purrr::map(seq_along(column),
             function(x) {
               start <- max(0, x + start_offset)
               end <- max(0, x + end_offset)
               column[start:end]
             })
}
make_scaler <- function(x) {
  message(paste(x,'\n'))
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
c(scale_value, descale_value) %<-% make_scaler(
  window(sunspot.month, 1749, c(1992, 12))
)
train_range <- 1749:1992
validation_range <- 1993:2002
testing_range <- 2003:2012
timesteps <- 120
data <- sunspot.month %>%
  as_tsibble() %>%
  as_data_frame() %>%
  mutate(key = case_when(
    year(index) %in% train_range ~ "train",
    year(index) %in% validation_range ~ "validation",
    year(index) %in% testing_range ~ "testing"
  )) %>%
  mutate(scaled_value = scale_value(value))

data <- data %>% mutate(lookback = make_series(scaled_value, -timesteps, -1),
               target = make_series(scaled_value, 0, timesteps - 1),
               target_lag = make_series(scaled_value, -1, timesteps - 2))

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
  filter(year(index) > 1758,
         key == "train")

View(training_data)
