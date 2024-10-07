# Učitavanje potrebnih paketa
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(corrplot)
library(here)

# Učitavanje podataka
data1 <- read.csv(here("data/telemetry_data_part1.csv"))
data2 <- read.csv(here("data/telemetry_data_part2.csv"))
data3 <- read.csv(here("data/telemetry_data_part3.csv"))

telemetry_data <- rbind(data1, data2, data3)
failure_data <- read.csv("data/NIS_ESP_events_anonymized.csv")

rm(data1)
rm(data2)
rm(data3)

telemetry_data <- as.data.table(telemetry_data)
failure_data <- as.data.table(failure_data)

#Provera postojanja i broja ouliera
sum(apply(telemetry_data[, -c(1:3)],2, FUN = function(x) length(boxplot.stats(x)$out)))

find_and_remove_outliers <- function(column) {
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

#Definisemo promenljivu u kojoj cemo cuvati brojeve redova sa outlierima
all_outlier_indices <- c()

# Prolaz kroz numericke kolone i provera outliera
for (col in colnames(telemetry_data[, -c(1:3)])) {
  outliers_indices <- find_and_remove_outliers(telemetry_data[[col]])
  all_outlier_indices <- unique(c(all_outlier_indices, outliers_indices))  # Dodavanje redova sa outlierima
}

#Izbacivanje outliera iz dataseta
cleaned_telemetry_data <- telemetry_data[-all_outlier_indices, ]
rm(telemetry_data)
rm(all_outlier_indices)
rm(col)
rm(outliers_indices)

#Kako NA vrednosti znace da nema promene u merenju, popunjavamo sva NA polja sa poslednjom poznatom
#vrednoscu
cleaned_telemetry_data <- cleaned_telemetry_data %>%
  fill(everything(), .direction = "down")

#Prve 42071 observacije imaju NA vrednosti tako da nemamo sve informacije sa merenja, uklanjamo ih
cleaned_telemetry_data <- cleaned_telemetry_data %>%
  slice(-1:-42071)

# Pretpostavljamo da zaustavljanje pumpe može biti povezano sa podacima neposredno pre tog događaja
cleaned_telemetry_data$measure_date <- as.Date(cleaned_telemetry_data$measure_date, format="%Y-%m-%d")
failure_data$failure_date <- as.Date(failure_data$date_time)
failure_data$date_time <- NULL

# Spajanje datasetova na osnovu bušotine i datuma zaustavljanja
merged_data <- cleaned_telemetry_data %>%
  left_join(failure_data, by = "well") %>%
  filter(failure_date > measure_date) %>%
  group_by(well, measure_date) %>%
  slice_min(failure_date, n = 1) 

#Uklanjanje nepotrebnih kolona
merged_data$X.x<- NULL
merged_data$X.y<- NULL
table(merged_data$well)
merged_data$well<- NULL

#Pretvaranje odgovarajucih varijabli u faktorske
length(table(merged_data$esp_type))
merged_data$esp_type<- as.factor(merged_data$esp_type)
merged_data$label<- as.factor(merged_data$label)

# Ispis strukture spojenog seta podataka
str(merged_data)

# Smanjivanje broja merenja u istom danu
merged_data <- merged_data %>%
  group_by(measure_date) %>%              # Grupisemo ih po datumu
  sample_n(size = min(n(), 100), replace = FALSE) %>%  # Sample od 100 merenja u danu, ako postoji toliko
  ungroup()  

#Provera korelacije
cor.mat <- cor(merged_data[, -c(1,15,16,17)])

corrplot.mixed(cor.mat)

#Korelacija izmedju 3 napona, 3 faze struje, temperatura u motoru i u busotini
merged_data$napon_bc <-NULL
merged_data$napon_ca <-NULL

merged_data$elektricna_struja_fazab <-NULL
merged_data$elektricna_struja_fazac <-NULL

merged_data$temperatura_u_busotini <-NULL

#Dodajemo time_to_failure kao broj dana izmedju merenja i sledeceg kvara
merged_data <- merged_data %>%
  mutate(time_to_failure = as.numeric(difftime(failure_date, measure_date, units = "days")))

merged_data$measure_date <- NULL
merged_data$failure_date <- NULL

#Ponovna provera korelacija, vise nema prvelikih koeficijenata
second.cor.mat <- cor(merged_data[, -c(9,10)])
corrplot.mixed(second.cor.mat)

write.csv(merged_data, "data/preprocessed_data.csv", row.names = FALSE)