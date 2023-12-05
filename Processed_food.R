#Analisi dell'indice relativo ai servizi inerenti le telecomunicazioni e poste

normalized_processed_food <- (goods_prices$processed_food - min(goods_prices$processed_food)) / (max(goods_prices$processed_food) - min(goods_prices$processed_food))



ggplot(goods_prices, aes(x = date, y=processed_food)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Processed food index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_processed_food, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between fuels and processed food", x = "Normalized processed food", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$processed_food, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_processed_food), aes(x = seq_along(normalized_processed_food), y=normalized_processed_food ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of processed food and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_processed_food, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between electricity and processed food", x = "Normalized processed food", y = "Normalized electricity price")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$processed_food, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_processed_food), aes(x = seq_along(normalized_processed_food), y=normalized_processed_food ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of processed food and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore durante anno
ggplot(goods_prices, aes(factor(ANNO), processed_food)) + geom_boxplot() +
  labs(
    title= "Index value variation of processed from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
