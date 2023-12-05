#Analisi dell'indice relativo a non energy industrial goods

normalized_non_energy <- (goods_prices$non_energy_industrial - min(goods_prices$non_energy_industrial)) / (max(goods_prices$non_energy_industrial) - min(goods_prices$non_energy_industrial))



ggplot(goods_prices, aes(x = date, y=non_energy_industrial)) + geom_point()+ geom_smooth() +
  xlab("Month/Year") +
  ylab("Index value") +
  labs(title = "Non energy industrial goods index trend during 2019/2023")+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Correlation with fuels

ggplot(data.frame(X = normalized_non_energy, Y = normalized_carburante), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between fuels and Non energy industrial goods", x = "Normalized Non energy industrial goods", y = "Normalized fuels")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$non_energy_industrial, carburante$Media_Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))

ggplot() +
  geom_line(data = data.frame(Dati=normalized_non_energy), aes(x = seq_along(normalized_non_energy), y=normalized_non_energy ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_carburante) , aes(x = seq_along(normalized_carburante), y = normalized_carburante), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of Non energy industrial goods and fuels trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 



#Correlation with electricity
ggplot(data.frame(X = normalized_non_energy, Y = normalized_corrente), aes(x = X, y = Y)) +
  geom_point() + stat_smooth(method = lm)+
  labs(title = "Correlation between electricity and Non energy industrial goods", x = "Normalized Non energy industrial goods", y = "Normalized electricity")

# Calcola il coefficiente di correlazione di Pearson
correlation_coefficient <- cor(goods_prices$non_energy_industrial, corrente_italia$Prezzo)
print(paste("Il coefficiente di correlazione di Pearson è:", correlation_coefficient))



ggplot() +
  geom_line(data = data.frame(Dati=normalized_non_energy), aes(x = seq_along(normalized_non_energy), y=normalized_non_energy ), color = "red", size = 1) +
  geom_line(data = data.frame(Dati=normalized_corrente) , aes(x = seq_along(normalized_carburante), y = normalized_corrente), color = "blue", size = 1) +  # Grafico dei prezzi
  labs(
    title = "Comparison of Non energy industrial goods and electricity trends during the 2019/2023 period",
    x = "Month",
    y = "Normalized value"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) 

#Variazione valore indice durante l'anno
ggplot(goods_prices, aes(factor(ANNO), non_energy_industrial)) + geom_boxplot() +
  labs(
    title= "Index value variation of non energy industrial goods from 2019 to 2023",
    x = "Year", 
    y = "Value")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 
