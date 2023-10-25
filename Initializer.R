#install.packages("readxl")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("tidyr")
#install.packages("cli", dependencies = TRUE)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)



benzina_data <- read.csv("/Users/ragno/Documents/Progetti/Health/benzina.csv")
gasolio_data <- read.csv("/Users/ragno/Documents/Progetti/Health/gasolio.csv")
corrente_data<-read.csv("/Users/ragno/Documents/Progetti/Health/european_wholesale_electricity_price_data.csv")
Interest_data <- read_xlsx("/Users/ragno/Documents/Progetti/Health/Tassi_Interesse_Europa.xlsx")
Nic_data <- read_xlsx("/Users/ragno/Documents/Progetti/Health/NIC_Italia.xlsx")
