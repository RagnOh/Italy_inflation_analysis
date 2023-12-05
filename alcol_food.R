#Analisi dell'indice relativo al cibo incluso l'alcol

normalized_alcol <- (goods_prices$alcol_food - min(goods_prices$alcol_food)) / (max(goods_prices$alcol_food) - min(goods_prices$alcol_food))

scaled_alcol<-scale(goods_prices$alcol_food)


ggplot(goods_prices, aes(x = date, y=alcol_food)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Food_including_alcol index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_alcol, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() +  stat_smooth(method = lm)+
  labs(title = "Correlation between fuels and food including alcol", x = "Normalized food_including_alcol", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$alcol_food, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

# Calcola il coefficiente di correlazione di Kendall
correlation_coefficient <- cor(goods_prices$alcol_food, carburante$Media_Prezzo,method = "kendall")
print(paste("Il coefficiente di correlazione di Kendall è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_alcol), aes(x = seq_along(normalized_alcol), y=normalized_alcol ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of food including alcol and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_alcol, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between electricity and food including alcol", x = "Normalized food including alcol", y = "Normalized electricity")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(scaled_alcol, scaled_corrente)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_alcol), aes(x = seq_along(normalized_alcol), y=normalized_alcol ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of food including alcol and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore durante anno
ggplot(goods_prices, aes(factor(ANNO), alcol_food)) + geom_boxplot() +
  labs(
    title= "Index value variation of food including alcol from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
