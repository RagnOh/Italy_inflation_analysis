#Analisi dell'indice relativo ai servizi inerenti le telecomunicazioni e poste

normalized_com_service <- (goods_prices$com_service - min(goods_prices$com_service)) / (max(goods_prices$com_service) - min(goods_prices$com_service))

scaled_comServices<-scale(goods_prices$com_service)


ggplot(goods_prices, aes(x = date, y=com_service)) + geom_point()+ 
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Communicatoin services index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_com_service, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm) +
  labs(title = "Correlation between fuels and communication services", x = "Normalized communication services", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(scaled_comServices, scaled_carburante)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_com_service), aes(x = seq_along(normalized_com_service), y=normalized_com_service ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of communication services and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_com_service, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm) +
  labs(title = "Correlation between electricity and communication services", x = "Normalized communication services", y = "Normalized electricity prices")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$com_service, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_com_service), aes(x = seq_along(normalized_com_service), y=normalized_com_service ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of communication services and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore indice durante l'anno
ggplot(goods_prices, aes(factor(ANNO), com_service)) + geom_boxplot() +
  labs(
    title= "Index value variation of com_services from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
