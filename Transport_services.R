#Analisi dell'indice relativo ai servizi relativi ai trasporti

normalized_transport <- (goods_prices$transport_service - min(goods_prices$transport_service)) / (max(goods_prices$transport_service) - min(goods_prices$transport_service))



ggplot(goods_prices, aes(x = date, y=transport_service)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Transport related services index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_transport, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between fuels and Transport related services", x = "Normalized Transport related services", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$transport_service, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_transport), aes(x = seq_along(normalized_transport), y=normalized_transport ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of Transport related services and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_transport, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between electricity and Transport related services", x = "Normalized Transport related services", y = "Normalized electricity price")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$transport_service, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_transport), aes(x = seq_along(normalized_transport), y=normalized_transport ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of Transport related services and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore durante anno
ggplot(goods_prices, aes(factor(ANNO), transport_service)) + geom_boxplot() +
  labs(
    title= "Index value variation of transport service from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
