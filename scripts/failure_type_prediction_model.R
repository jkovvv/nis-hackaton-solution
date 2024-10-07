# Učitavanje potrebnih paketa
library(dplyr)
library(lubridate)
library(data.table)
library(caret)
library(randomForest)
library(pROC)
library(ranger)

#Ucitavanje pripremljenih podataka iz .csv fajla
data <- read.csv("data/preprocessed_data.csv")
str(data)
data$esp_type <- as.factor(data$esp_type)
data$label <- as.factor(data$label)

# Podela podataka na trening i test set
set.seed(123)

train_index <- createDataPartition(data$label, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


#Model
label_ranger <- ranger(label ~ . -label -time_to_failure, data = train_data, num.trees = 50, importance = "impurity",
                   probability = TRUE)

# Ispitivanje važnosti promenljivih
var_importance<- label_ranger$variable.importance
print(var_importance)

# Predikcija na test podacima
type_predictions <- predict(label_ranger, test_data)$predictions

# Dobijamo predvidjeni tip otkaza po najvecoj verovatnoci u redu
predicted_labels <- apply(type_predictions, 1, function(x) {
  colnames(type_predictions)[which.max(x)]
})

# Pretvaramo u faktor koji se poklapa sa faktorima u datasetu
predicted_labels <- factor(predicted_labels, levels = levels(test_data$label))


# Evaluacija modela pomoću confusion matrix-a
conf_matrix <- confusionMatrix(predicted_labels, test_data$label)
print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 4)))

# Metrike po klasama
class_stats <- conf_matrix$byClass
print("Metrike po klasama:")
print(class_stats[,c('Precision','Recall','F1')])

# Dobijanje lista klasa
actual_labels <- test_data$label
classes <- levels(actual_labels)

# Inicijalizacija liste za čuvanje ROC objekata
roc_list <- list()

# Generisanje ROC kriva za svaku klasu
for (class in classes) {
  # Binarne stvarne oznake: 1 za trenutnu klasu, 0 za ostale
  actual_binary <- ifelse(actual_labels == class, 1, 0)
  
  # Verovatnoće za trenutnu klasu
  predicted_probs <- type_predictions[, class]
  
  # Generisanje ROC krive
  roc_obj <- roc(actual_binary, predicted_probs)
  roc_list[[class]] <- roc_obj
}

# Postavljanje boja za svaku klasu
colors <- rainbow(length(classes))

# # Crtanje ROC kriva
for (i in 1:length(classes)) {
  plot(roc_list[[i]], col = colors[i], main = "ROC krive po klasama", add = TRUE, lwd = 2)
}

# Dodavanje dijagonale i legende
abline(a = 0, b = 1, lty = 2, col = "gray")
legend("bottomright", legend = classes, col = colors, lwd = 2)

# Izračunavanje AUC vrednosti
auc_values <- sapply(roc_list, function(roc_obj) {
  pROC::auc(roc_obj)
})
print("AUC vrednosti po klasama:")
print(auc_values)