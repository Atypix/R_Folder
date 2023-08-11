library(tensorflow)
library(keras)
library(readr)




# Préparation des données d'entraînement

data <- read.csv("data_all.csv", sep=",", head=FALSE)
data <- tail(data, n = 500)

# Preprocessing des données
data_scaled <- scale(data[,1:5])


## Split en entrées et cibles pour l'entrainement
data_input <- data_scaled[1:(nrow(data_scaled)-1),]
data_target <- data_scaled[2:nrow(data_scaled),1:5]

# Definition du modele
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 50, activation = "relu", input_shape = c(5)) %>% 
  layer_dense(units = 5)

model %>% compile(
  optimizer = "adam", 
  loss = "mean_squared_error", 
  metrics = c("accuracy")
)

# Entrainement
model %>% fit(data_input, data_target, epochs = 100, batch_size = 1)

# Prediction
next_input <- tail(data_scaled, n = 1)
next_output <- model %>% predict(next_input)


# Inverser la normalisation pour obtenir les valeurs en dehors de l'echelle
next_output_rescaled <- round(as.numeric(next_output) * 50)
print(next_output_rescaled)
