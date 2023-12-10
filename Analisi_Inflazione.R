names(Nic_data)
print(Nic_data)

#allineo dati carburanti
carburante<-carburante[-c(1,2),]
corrente_italia<-corrente_italia[-c(1,2),]

#elimino righe non utili 
Nic_data <-Nic_data[-c(1:4), ]
Nic_data <-Nic_data[-c(2:2), ]
print(Nic_data)
Nic_data <-Nic_data[-c(3:3), ]
print(Nic_data)


#Nomi elementi da analizzare
u_food_name <- "FOODUNP: unprocessed food"
a_food_name <- "FOODXT: food including alcohol"
ap_food_name <- "FOODPROCXT: processed food including alcohol"
energy_name <- "ENRGY: energy"
tobacco_name <- "TOBAC: tobacco"
nenergy_industrial_name <- "IGOODSXE: non-energy industrial goods"
h_service_name<- "SERVHOUSE: services related to housing"
com_service_name <- "SERVCOMM: services related to communication"
rec_service_name <- "SERVRP: services related to recreation, including repairs and personal care"
transport_service_name <- "SERVTRANS: services related to transport"
soap_goods_name <- "FOODHPC: food, goods related to household cleaning and maintenance and personal care"
low_freq_purchase_name <-"LOWFRP:  -- low-frequency purchases"


# Crea nuove tabelle in base al nome elemento
unprocessed_food<- Nic_data[Nic_data$...1 == u_food_name, -2]
alcol_food <- Nic_data[Nic_data$...1 == a_food_name, -2]
processed_food <- Nic_data[Nic_data$...1 == ap_food_name, -2]
energy<-Nic_data[Nic_data$...1 == energy_name, -2]
tobacco<-Nic_data[Nic_data$...1 == tobacco_name, -2]
non_energy_industrial<-Nic_data[Nic_data$...1 == nenergy_industrial_name, -2]
housing_services<-Nic_data[Nic_data$...1 == h_service_name, -2]
com_service<-Nic_data[Nic_data$...1 == com_service_name, -2]
recreation_service<-Nic_data[Nic_data$...1 == rec_service_name, -2]
transport_service<-Nic_data[Nic_data$...1 == transport_service_name, -2]
cleaning_goods<-Nic_data[Nic_data$...1 == soap_goods_name, -2]
low_freq_goods<-Nic_data[Nic_data$...1 == low_freq_purchase_name, -2]

#Trasformo in dataframe
unprocessed_food <- as.data.frame(t(unprocessed_food))
alcol_food <- as.data.frame(t(alcol_food))
processed_food <- as.data.frame(t(processed_food))
energy <- as.data.frame(t(energy))
tobacco <- as.data.frame(t(tobacco))
non_energy_industrial <- as.data.frame(t(non_energy_industrial))
housing_services <- as.data.frame(t(housing_services))
com_service <- as.data.frame(t(com_service))
recreation_service <- as.data.frame(t(recreation_service))
transport_service <- as.data.frame(t(transport_service))
cleaning_goods <- as.data.frame(t(cleaning_goods))
low_freq_goods <- as.data.frame(t(low_freq_goods))

#Trasformazioni sulle tabelle per poter creare i grafici
unprocessed_food=unprocessed_food[-1,1]
unprocessed_food=as.numeric(unprocessed_food)
alcol_food=alcol_food[-1,1]
alcol_food=as.numeric(alcol_food)
processed_food=processed_food[-1,1]
processed_food=as.numeric(processed_food)
energy=energy[-1,1]
energy=as.numeric(energy)
tobacco=tobacco[-1,1]
tobacco=as.numeric(tobacco)
non_energy_industrial=non_energy_industrial[-1,1]
non_energy_industrial=as.numeric(non_energy_industrial)
housing_services=housing_services[-1,1]
housing_services=as.numeric(housing_services)
com_service=com_service[-1,1]
com_service=as.numeric(com_service)
recreation_service=recreation_service[-1,1]
recreation_service=as.numeric(recreation_service)
transport_service=transport_service[-1,1]
transport_service=as.numeric(transport_service)
cleaning_goods=cleaning_goods[-1,1]
cleaning_goods=as.numeric(cleaning_goods)
low_freq_goods=low_freq_goods[-1,1]
low_freq_goods=as.numeric(low_freq_goods)

#creo tabella 

goods_prices<-data.frame(
  u_food = unprocessed_food
)

mesi=prezzi_annuali_benzina$mese[-c(1,2)]
goods_prices$date = mesi

goods_prices$alcol_food=alcol_food
goods_prices$processed_food=processed_food
goods_prices$energy=energy
goods_prices$tobacco=tobacco
goods_prices$non_energy_industrial=non_energy_industrial
goods_prices$housing_services=housing_services
goods_prices$com_service=com_service
goods_prices$recreation_service=recreation_service
goods_prices$transport_service=transport_service
goods_prices$cleaning_goods=cleaning_goods
goods_prices$low_freq_goods=low_freq_goods

#stampo grafico per visualizzare i comportamenti di ciascun elemento
ggplot() +
  geom_line(data =goods_prices, aes(x = date, y = unprocessed_food ,group = 1 ), color = "blue", size = 1) +  
  geom_line(data =goods_prices, aes(x = date, y = alcol_food, group = 1), color = "red", size = 1) +
  #geom_line(data =goods_prices, aes(x = date, y = processed_food,group = 1 ), color = "orange", size = 1) +
 # geom_line(data =goods_prices, aes(x = date, y = energy, group = 1 ), color = "violet", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = tobacco, group = 1 ), color = "pink", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = non_energy_industrial, group = 1 ), color = "lightblue", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = housing_services, group = 1 ), color = "purple", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = com_service, group = 1 ), color = "yellow", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = recreation_service, group = 1 ), color = "brown", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = transport_service, group = 1 ), color = "black", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = cleaning_goods, group = 1 ), color = "white", size = 1) +
  geom_line(data =goods_prices, aes(x = date, y = low_freq_goods, group = 1 ), color = "grey", size = 1) +

  
  labs(
    title = "Trend of the idexes during the considered period",
    x = "Date",
    y = "Value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

normalized_carburante <- (carburante$Media_Prezzo - min(carburante$Media_Prezzo)) / (max(carburante$Media_Prezzo) - min(carburante$Media_Prezzo))
normalized_corrente <- (corrente_italia$Prezzo - min(corrente_italia$Prezzo)) / (max(corrente_italia$Prezzo) - min(corrente_italia$Prezzo))

scaled_corrente<-corrente_italia$Prezzo
scaled_carburante<-carburante$Media_Prezzo

#Grafico correlazioni indici con carburanti
ggplot(data=Pearson_correlation_values, aes(x = ...1 , y = Fuels )) +
  geom_bar(stat = "identity") +
  labs(title = "Index value of correlation with fuels price",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))


#Grafico correlazione indici con elettricità
ggplot(data=Pearson_correlation_values, aes(x = ...1 , y = Electricity )) +
  geom_bar(stat = "identity") +
  labs(title = "Index value of correlation with electricity price",
       x = "Category",
       y = "Value") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

goods_prices$ANNO <- sub(".*\\s(\\d{4})", "\\1", goods_prices$date)

# Converte la nuova colonna in formato numerico
goods_prices$ANNO <- as.numeric(goods_prices$ANNO)

#Analisi performances
Index_gains <-Gains_data[1:(nrow(Gains_data)-3),]
Index_gains$colore <- ifelse(Index_gains$pandemic < 0, "red", "green")
Energy_gains <-Gains_data[11:(nrow(Gains_data)),]

#Analisi energia
ggplot(data=Energy_gains, aes(x = name , y = pandemic )) +
  geom_bar(stat = "identity",fill="red") +
  labs(title = "Energy elements performances during pandemic period",
       x = "Category",
       y = "Change %") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

ggplot(data=Energy_gains, aes(x = name , y = top )) +
  geom_bar(stat = "identity",fill="darkgreen") +
  labs(title = "Energy elements gains from September 2019 to April 2023 ",
       x = "Category",
       y = "Change %") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Analisi indici
ggplot(data=Index_gains, aes(x = name , y = pandemic, fill = colore )) +
  geom_bar(stat = "identity") +
  labs(title = "Indexes performances during pandemic period",
       x = "Category",
       y = "Change %") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))+
  scale_fill_manual(values = c("darkgreen", "red"))

ggplot(data=Index_gains, aes(x = name , y = top )) +
  geom_bar(stat = "identity", fill="darkgreen") + 
  labs(title = "Indexes gains from september 2019 to April 2023 ",
       x = "Category",
       y = "Change %") +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))
