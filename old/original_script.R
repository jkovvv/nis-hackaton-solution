#SCRIPT USED FOR HACKATON - OUTDATED

# Učitavanje potrebnih paketa
library(dplyr)
library(lubridate)
library(data.table)

# Učitavanje podataka
telemetry_data <- read.csv("data/NIS_ESP_telemetry_anonymized.csv")
failure_data <- read.csv("data/NIS_ESP_events_anonymized.csv")

telemetry_data <- as.data.table(telemetry_data)
failure_data <- as.data.table(failure_data)

library(tidyr)

find_outliers <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)  # Prvi kvartil (Q1)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)  # Treći kvartil (Q3)
  IQR_value <- IQR(column, na.rm = TRUE)      # Interkvartilni raspon (IQR)
  
  # Definišemo granice za outliere
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  # Pronalazimo indekse outliera
  outlier_indices <- which(column < lower_bound | column > upper_bound)
  
  return(outlier_indices)
}

# Primer: Pronalaženje outliera za kolonu pritisak_na_prijemu_pumpe
outliers_indices <- find_outliers(telemetry_data$pritisak_na_prijemu_pumpe)

# Prikaz indeksa outliera
print(outliers_indices)

# Uklanjanje outliera iz kolone pritisak_na_prijemu_pumpe
telemetry_data <- telemetry_data[-outliers_indices, ]


prag_ab <- quantile(telemetry_data$napon_ab, probs = 0.03, na.rm = TRUE)
prag_bc <- quantile(telemetry_data$napon_bc, probs = 0.03, na.rm = TRUE)
prag_ca <- quantile(telemetry_data$napon_ca, probs = 0.03, na.rm = TRUE)
prag_esfa <- quantile(telemetry_data$elektricna_struja_fazaa, probs = 0.03, na.rm = TRUE)
prag_esfb <- quantile(telemetry_data$elektricna_struja_fazab, probs = 0.03, na.rm = TRUE)
prag_esfc <- quantile(telemetry_data$elektricna_struja_fazac, probs = 0.03, na.rm = TRUE)
prag_kk <- quantile(telemetry_data$koeficijent_kapaciteta, probs = 0.03, na.rm = TRUE)
prag_f <- quantile(telemetry_data$frekvencija, probs = 0.03, na.rm = TRUE)
prag_ro <- quantile(telemetry_data$radno_opterecenje, probs = 0.03, na.rm = TRUE)
prag_as <- quantile(telemetry_data$aktivna_snaga, probs = 0.03, na.rm = TRUE)
prag_pnpp <- quantile(telemetry_data$pritisak_na_prijemu_pumpe, probs = 0.03, na.rm = TRUE)
prag_tm <- quantile(telemetry_data$temperatura_motora, probs = 0.03, na.rm = TRUE)
prag_tub <- quantile(telemetry_data$temperatura_u_busotini, probs = 0.03, na.rm = TRUE)


#Kako NA vrednosti znace da nema promen, popunjavamo sva NA polja sa poslednjom poznatom
#vrednoscu
telemetry_data <- telemetry_data %>%
  fill(everything(), .direction = "down")

telemetry_data <- telemetry_data %>%
  filter(napon_ab > prag_ab, 
         napon_bc > prag_bc, 
         napon_ca > prag_ca,
         elektricna_struja_fazaa > prag_esfa, 
         elektricna_struja_fazab > prag_esfb, 
         elektricna_struja_fazac > prag_esfc,
         koeficijent_kapaciteta > prag_kk, 
         frekvencija > prag_f, 
         radno_opterecenje > prag_ro,
         aktivna_snaga > prag_as, 
         pritisak_na_prijemu_pumpe > prag_pnpp, 
         temperatura_motora > prag_tm,
         temperatura_u_busotini > prag_tub)

prag_ab <- quantile(telemetry_data$napon_ab, probs = 0.98, na.rm = TRUE)
prag_bc <- quantile(telemetry_data$napon_bc, probs = 0.98, na.rm = TRUE)
prag_ca <- quantile(telemetry_data$napon_ca, probs = 0.98, na.rm = TRUE)
prag_esfa <- quantile(telemetry_data$elektricna_struja_fazaa, probs = 0.98, na.rm = TRUE)
prag_esfb <- quantile(telemetry_data$elektricna_struja_fazab, probs = 0.98, na.rm = TRUE)
prag_esfc <- quantile(telemetry_data$elektricna_struja_fazac, probs = 0.98, na.rm = TRUE)
prag_kk <- quantile(telemetry_data$koeficijent_kapaciteta, probs = 0.98, na.rm = TRUE)
prag_f <- quantile(telemetry_data$frekvencija, probs = 0.98, na.rm = TRUE)
prag_ro <- quantile(telemetry_data$radno_opterecenje, probs = 0.98, na.rm = TRUE)
prag_as <- quantile(telemetry_data$aktivna_snaga, probs = 0.98, na.rm = TRUE)
prag_pnpp <- quantile(telemetry_data$pritisak_na_prijemu_pumpe, probs = 0.98, na.rm = TRUE)
prag_tm <- quantile(telemetry_data$temperatura_motora, probs = 0.98, na.rm = TRUE)
prag_tub <- quantile(telemetry_data$temperatura_u_busotini, probs = 0.98, na.rm = TRUE)


telemetry_data <- telemetry_data %>%
  filter(napon_ab < prag_ab, 
         napon_bc < prag_bc, 
         napon_ca < prag_ca,
         elektricna_struja_fazaa < prag_esfa, 
         elektricna_struja_fazab < prag_esfb, 
         elektricna_struja_fazac < prag_esfc,
         koeficijent_kapaciteta < prag_kk, 
         frekvencija < prag_f, 
         radno_opterecenje < prag_ro,
         aktivna_snaga < prag_as, 
         pritisak_na_prijemu_pumpe < prag_pnpp, 
         temperatura_motora < prag_tm,
         temperatura_u_busotini < prag_tub)

#Prve 22 observacije imaju NA vrednosti, uklanjamo ih
telemetry_data <- telemetry_data %>%
  slice(-1:-22)

# Pretpostavljamo da zaustavljanje pumpe može biti povezano sa podacima neposredno pre tog događaja
telemetry_data$measure_date <- as.POSIXct(telemetry_data$measure_date, format="%Y-%m-%d %H:%M:%S")
failure_data$date_time <- as.POSIXct(failure_data$date_time, format="%Y-%m-%d")


telemetry_data <- telemetry_data %>%
  filter(napon_ab >= 50, napon_bc >= 50, napon_ca >= 50, frekvencija >=0, temperatura_motora>=70, temperatura_u_busotini>=70)


# Spajanje na osnovu bušotine i datuma zaustavljanja
merged_data <- telemetry_data %>%
  left_join(failure_data, by = "well")

merged_data <- as.data.table(merged_data)

merged_data$X.x<- NULL
merged_data$X.y<- NULL
table(merged_data$well)
merged_data$well<- NULL
merged_data$esp_type<- as.factor(merged_data$esp_type)


# Ispis strukture spojenog seta podataka
str(merged_data)

# Kreiranje novih kolona koje predstavljaju promene u ključnim parametrima
merged_data <- merged_data %>%
  mutate(
    napon_change = napon_ab - lag(napon_ab),
    struja_change = elektricna_struja_fazaa - lag(elektricna_struja_fazaa),
    frekvencija_change = frekvencija - lag(frekvencija),
    pritisak_change = pritisak_na_prijemu_pumpe - lag(pritisak_na_prijemu_pumpe),
    temperatura_motora_change = temperatura_motora - lag(temperatura_motora),
    temperatura_busotine_change = temperatura_u_busotini - lag(temperatura_u_busotini)
  )
merged_data<- merged_data %>%
  slice(-1:-1)  

# Brisanje redova sa NA vrednostima (koji su rezultat lag funkcije)
merged_data <- na.omit(merged_data)

# Pretvaranje ciljne promenljive 'label' u faktor
merged_data$label <- as.factor(merged_data$label)

# Podela podataka na trening i test set
set.seed(123)

library(caret)
library(randomForest)
library(pROC)

train_index <- createDataPartition(merged_data$label, p = 0.7, list = FALSE)
train_data <- merged_data[train_index, ]
test_data <- merged_data[-train_index, ]


#Model
library(ranger)
 
rf_model <- ranger(label ~ napon_ab + elektricna_struja_fazaa + koeficijent_kapaciteta + frekvencija + 
                     radno_opterecenje + temperatura_motora, data = train_data, num.trees = 50, importance = "impurity", 
                   probability = TRUE)

# Ispitivanje važnosti promenljivih
importance_ranger <- rf_model$variable.importance

# Prikaz važnosti promenljivih
print(importance_ranger)

# Predikcija na test podacima
type_predictions <- predict(rf_model, test_data)
type_predictions <- type_predictions$predictions


# Combine predictions with the test data (optional)
type_df <- cbind(test_data, type_predictions)


# Get the predicted class labels by selecting the class with the highest probability
predicted_labels <- apply(type_predictions, 1, function(x) {
  colnames(type_predictions)[which.max(x)]
})

# Convert to factor and ensure levels match actual labels
predicted_labels <- factor(predicted_labels, levels = levels(test_data$label))


# Evaluacija modela pomoću confusion matrix-a
conf_matrix <- confusionMatrix(predicted_labels, test_data$label)
print(conf_matrix)

accuracy <- conf_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 4)))

# Izvlačenje statistika po klasama
class_stats <- conf_matrix$byClass

# Metrike po klasama
print("Statistike po klasama:")
print(class_stats[,c('Precision','Recall','F1')])

# Predikcija na test podacima sa verovatnoćama
predictions <- predict(rf_model, data = test_data)
probabilities <- predictions$predictions  # Matrica verovatnoća za svaku klasu
#probabilities <- data.frame(probabilities)
# Stvarne oznake
actual_labels <- test_data$label
# Dobijanje lista klasa
classes <- levels(actual_labels)

# Inicijalizacija liste za čuvanje ROC objekata
roc_list <- list()

# Generisanje ROC kriva za svaku klasu
for (class in classes) {
  # Binarne stvarne oznake: 1 za trenutnu klasu, 0 za ostale
  actual_binary <- ifelse(actual_labels == class, 1, 0)
  
  # Verovatnoće za trenutnu klasu
  predicted_probs <- probabilities[, class]
  
  # Generisanje ROC krive
  roc_obj <- roc(actual_binary, predicted_probs)
  
  # Čuvanje ROC objekta sa nazivom klase
  roc_list[[class]] <- roc_obj
}

# Postavljanje boja za svaku klasu
colors <- rainbow(length(classes))

# Crtanje prve ROC krive
plot(roc_list[[1]], col = colors[1], main = "ROC krive po klasama", lwd = 2)

# Dodavanje ostalih ROC kriva
for (i in 2:length(classes)) {
  plot(roc_list[[i]], col = colors[i], add = TRUE, lwd = 2)
}

# Dodavanje dijagonale
abline(a = 0, b = 1, lty = 2, col = "gray")

# Dodavanje legende
legend("bottomright", legend = classes, col = colors, lwd = 2)

# Izračunavanje AUC vrednosti
auc_values <- sapply(roc_list, function(roc_obj) {
  pROC::auc(roc_obj)
})

# Prikaz AUC vrednosti
print("AUC vrednosti po klasama:")
print(auc_values)

####################################################################
##########################PREDIKCIJA DATUMA#########################
####################################################################

merged_data <- merged_data[order(merged_data$measure_date), ]

# Kreiranje ciljne promenljive 'time_to_failure'
merged_data <- merged_data %>%
  mutate(time_to_failure = as.numeric(difftime(date_time, measure_date, units = "days")))

# Uklonite redove gde ne postoji budući kvar
merged_data <- merged_data %>%
  filter(time_to_failure >= 0)  

# Zadržavamo samo one redove gde postoji budući kvar
merged_data <- merged_data %>%
  filter(time_to_failure <=45)

# Podela podataka na trening i test skup
set.seed(123)
train_index <- createDataPartition(merged_data$time_to_failure, p = 0.7, list = FALSE)
date_train_data <- merged_data[train_index, ]
date_test_data <- merged_data[-train_index, ]

library(ranger)

#Model
rf_model_ttf <- ranger(time_to_failure ~ napon_ab + elektricna_struja_fazaa + koeficijent_kapaciteta + 
                         frekvencija + radno_opterecenje + temperatura_motora -date_time -label, 
                       data = date_train_data, num.trees = 100)


# Predikcija na test podacima
predictions <- predict(rf_model_ttf, date_test_data)
ttf_predictions <- predictions$predictions


# Combine predictions with the test data (optional)
final_df <- cbind(date_test_data, ttf_predictions)

# Upoređivanje stvarnih i predviđenih vremena do kvara
#print(data.frame(actual = date_test_data$time_to_failure, predicted = ttf_predictions))

# Dodavanje predviđenog vremena do kvara poslednjem dostupnom vremenskom pečatu
date_test_data$predicted_failure_date <- date_test_data$measure_date + (ttf_predictions * 24 * 60 * 60) # Dodajemo dane u sekunde

# Pregled predikcija datuma kvara
#print(data.frame(measure_date = date_test_data$measure_date, 
                 # predicted_failure_date = date_test_data$predicted_failure_date, 
                 # actual_failure_date = date_test_data$date_time))

library(Metrics)

# Evaluacija modela
mae_value <- mae(date_test_data$time_to_failure, ttf_predictions)
rmse_value <- rmse(date_test_data$time_to_failure, ttf_predictions)

print(paste("Mean Absolute Error: ", mae_value))
print(paste("Root Mean Square Error: ", rmse_value))

#############################################################
# Pretpostavljamo da je 'numeric_data' deo tvog dataframe-a koji sadrži numeričke kolone
numeric_columns <- sapply(test_data, is.numeric)  # Odabir numeričkih kolona
test_data_scaled <- test_data  # Kreiramo kopiju dataset-a

plot.new()

#Grafici

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = napon_change, color = "Promena napona")) +
  geom_line(aes(y = struja_change, color = "Promena struje")) +
  #geom_line(aes(y = pritisak_change, color = "Promena pritiska")) +
  geom_line(aes(y = temperatura_motora_change, color = "Promena temperature motora")) +
  #geom_point(aes(y = date_time)) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Simptomi pre otkaza", x = "Vreme", y = "Promena parametara") +
  scale_color_manual(values = c("blue", "green", "orange", "red", "yellow")) +
  theme_minimal() +
  ylim(-40, 40)

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = aktivna_snaga, color = "Aktivna snaga")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Aktivna snaga i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("blue", "green", "orange", "red", "yellow")) +
  theme_minimal()

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = temperatura_motora, color = "Temperatura motora")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Temperatura motora i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("green", "orange", "red", "yellow")) +
  theme_minimal()

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = temperatura_u_busotini, color = "Temperatura u busotini")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Temperatura u busotini i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("orange", "red", "yellow")) +
  theme_minimal()

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = radno_opterecenje, color = "Radno opterecenje")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Radno opterecenje i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("purple", "red" )) +
  theme_minimal()

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = napon_ab, color = "Napon")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Napon i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("purple", "red" )) +
  theme_minimal()

ggplot(test_data_scaled, aes(x = measure_date)) +
  geom_line(aes(y = elektricna_struja_fazaa, color = "El. struja")) +
  geom_vline(aes(xintercept = as.numeric(test_data_scaled$date_time)), color = "red", linetype = "solid", size = 0.4) +
  labs(title = "Struja i otkazi", x = "Vreme", y = "Promena parametra") +
  scale_color_manual(values = c("purple", "red" )) +
  theme_minimal()


