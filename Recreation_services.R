#Analisi dell'indice relativo ai servizi inerenti al divertimento

normalized_recreation <- (goods_prices$recreation_service - min(goods_prices$recreation_service)) / (max(goods_prices$recreation_service) - min(goods_prices$recreation_service))

scaled_recreation<-scale(goods_prices$recreation_service)


ggplot(goods_prices, aes(x = date, y=recreation_service)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Recreation services index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_recreation, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between fuels and recreation services", x = "Normalized recreation services", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$recreation_service, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

correlation_coefficient <- cor(goods_prices$recreation_service, carburante$Media_Prezzo,method = "kendall")
print(paste("Il coefficiente di correlazione di Kendall è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati= normalized_recreation), aes(x = seq_along(normalized_recreation), y=normalized_recreation ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of recreation services and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_recreation, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between electricity and recreation services", x = "Normalized recreation services", y = "Normalized electricity price")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$recreation_service, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_recreation), aes(x = seq_along(normalized_recreation), y=normalized_recreation ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of recreation services and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore durante anno
ggplot(goods_prices, aes(factor(ANNO), recreation_service)) + geom_boxplot() +
  labs(
    title= "Index value variation of recreation services from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
