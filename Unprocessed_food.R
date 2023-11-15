#Analisi dell'indice relativo ai cibi non ancora processati

normalized_unprocessed_food <- (goods_prices$u_food - min(goods_prices$u_food)) / (max(goods_prices$u_food) - min(goods_prices$u_food))



ggplot(goods_prices, aes(x = date, y=u_food)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Unprocessed_food index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_unprocessed_food, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between fuels and unprocessed_food", x = "Normalized unprocessed_food", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$u_food, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_unprocessed_food), aes(x = seq_along(normalized_unprocessed_food), y=normalized_unprocessed_food ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of unprocessed_food and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_unprocessed_food, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between electricity and unprocessed_food", x = "Normalized unprocessed_food", y = "Normalized electricity")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$u_food, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_unprocessed_food), aes(x = seq_along(normalized_unprocessed_food), y=normalized_unprocessed_food ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  # Grafico dell'IVA
  labs(
    title = "Comparison of unprocessed_food and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 
