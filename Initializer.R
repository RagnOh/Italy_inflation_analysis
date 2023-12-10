#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("cli", dependencies = TRUE)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)



benzina_data <- read.csv("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/benzina.csv")
gasolio_data <- read.csv("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/gasolio.csv")
corrente_data<-read.csv("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/european_wholesale_electricity_price_data.csv")
Interest_data <- read_xlsx("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/Tassi_Interesse_Europa.xlsx")
Nic_data <- read_xlsx("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/NIC_Italia.xlsx")
Pearson_correlation_values <- read_xlsx("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/Indici_correlazione.xlsx")
Gains_data<- read_xlsx("/Users/ragno/Documents/Progetti/Health/Analisi/Italy_inflation_analysis/datasets/Index_performaces.xlsx")
