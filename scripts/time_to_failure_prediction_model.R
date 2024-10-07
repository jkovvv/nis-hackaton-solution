#Ucitavamo potrebne pakete
library(ranger)
library(Metrics)

#Ucitavamo uredjene podatke
data <- read.csv("data/preprocessed_data.csv")

# Uklonite redove gde ne postoji budući kvar
data <- data %>%
  filter(time_to_failure >= 0)  

# Podela podataka na trening i test skup
set.seed(1)
train_index <- createDataPartition(data$time_to_failure, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
rm(data)

#Model
ttf_ranger <- ranger(time_to_failure ~ ., 
                       data = train_data, num.trees = 100)

# Predikcija na test podacima
predictions <- predict(ttf_ranger, test_data)$predictions

# Dodavanje predviđenog vremena do kvara poslednjem dostupnom vremenskom pečatu
test_data$predicted_failure_date <- predictions

# Evaluacija modela
mae_value <- mae(test_data$time_to_failure, predictions)
rmse_value <- rmse(test_data$time_to_failure, predictions)

print(paste("Mean Absolute Error: ", mae_value))
print(paste("Root Mean Square Error: ", rmse_value))