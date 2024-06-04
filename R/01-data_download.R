library(googlesheets4)
library(readr)
data_import <- read_sheet("https://docs.google.com/spreadsheets/d/13kTu7PBCxq7WzKF8NNL12WuSISommmARIwPv0ZrIgQE/edit#gid=0")
write_csv(data_import, "data/raw/litter_waste.csv")
