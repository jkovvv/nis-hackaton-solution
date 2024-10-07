#This script is used only to make smaller .csv files, bypassing GitHub restrictions on file size

library(here)
data <- read.csv(here("data/NIS_ESP_telemetry_anonymized.csv"))

# Calculating the number of rows in each third
total_rows <- nrow(data)
third <- ceiling(total_rows / 3)

# Splitting the data into thirds
data1 <- data[1:third, ]                # First third
data2 <- data[(third + 1):(2 * third), ] # Second third
data3 <- data[(2 * third + 1):total_rows, ] # Third third

# Writing each part to a new CSV file
write.csv(data1, "data/telemetry_data_part1.csv", row.names = FALSE)
write.csv(data2, "data/telemetry_data_part2.csv", row.names = FALSE)
write.csv(data3, "data/telemetry_data_part3.csv", row.names = FALSE)