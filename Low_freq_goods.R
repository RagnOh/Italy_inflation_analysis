#Analisi dell'indice relativo ai servizi inerenti le telecomunicazioni e poste

normalized_low_freq <- (goods_prices$low_freq_goods - min(goods_prices$low_freq_goods)) / (max(goods_prices$low_freq_goods) - min(goods_prices$low_freq_goods))



ggplot(goods_prices, aes(x = date, y=low_freq_goods)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Low frequency purchased goods index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_low_freq, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm) +
  labs(title = "Correlation between fuels and Low frequency purchased goods", x = "Normalized Low frequency purchased goods", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$low_freq_goods, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_low_freq), aes(x = seq_along(normalized_low_freq), y=normalized_low_freq ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of Low frequency purchased goods and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_low_freq, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() +  stat_smooth(method = lm) +
  labs(title = "Correlation between electricity and Low frequency purchased goods", x = "Normalized Low frequency purchased goods", y = "Normalized electricity")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$low_freq_goods, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_low_freq), aes(x = seq_along(normalized_low_freq), y=normalized_low_freq ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of Low frequency purchased good and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore indice durante l'anno
ggplot(goods_prices, aes(factor(ANNO), low_freq_goods)) + geom_boxplot() +
  labs(
    title= "Index value variation of low frequency goods from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
