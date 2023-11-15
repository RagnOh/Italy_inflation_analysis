#Analisi dell'indice relativo ai servizi legati agli alloggi

normalized_housing <- (goods_prices$housing_services - min(goods_prices$housing_services)) / (max(goods_prices$housing_services) - min(goods_prices$housing_services))



ggplot(goods_prices, aes(x = date, y=housing_services)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "housing services index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_housing, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between fuels and housing services", x = "Normalized housing services", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$housing_services, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_housing), aes(x = seq_along(normalized_housing), y=normalized_housing ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of housing services and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_housing, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Correlation between electricity and housing services", x = "Normalized housing services", y = "Normalized electricity")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$housing_services, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_housing), aes(x = seq_along(normalized_housing), y=normalized_housing ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of housing services and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 
