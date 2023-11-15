#Analisi dell'indice relativo al tabacco

normalized_tobacco <- (goods_prices$tobacco - min(goods_prices$tobacco)) / (max(goods_prices$tobacco) - min(goods_prices$tobacco))



ggplot(goods_prices, aes(x = date, y=tobacco)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Tobacco index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_tobacco, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between fuels and tobacco", x = "Normalized tobacco", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$tobacco, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_tobacco), aes(x = seq_along(normalized_tobacco), y=normalized_tobacco ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of tobacco and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_tobacco, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between electricity and tobacco", x = "Normalized tobacco", y = "Normalized electricity price")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$tobacco, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_tobacco), aes(x = seq_along(normalized_tobacco), y=normalized_tobacco ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of tobacco and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 
