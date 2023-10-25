names(Nic_data)
print(Nic_data)
#elimino righe non utili 
Nic_data <-Nic_data[-c(1:4), ]
Nic_data <-Nic_data[-c(2:2), ]
print(Nic_data)
Nic_data <-Nic_data[-c(3:3), ]
print(Nic_data)

carburante <- media_prezzi_carburante[media_prezzi_carburante$ANNO >= 2019, ]

# Crea un grafico a linea
grafico_linea <- ggplot(data = carburante, aes(x = 1:nrow(carburante), y = Media_Prezzo)) +
  geom_line(color = "blue") +
  labs(x = "Osservazioni", y = "Media Prezzo") +
  theme_minimal()

# Stampa il grafico a linea
print(grafico_linea)

nome_cercato <- "FOODUNP: unprocessed food"

# Crea una nuova tabella con le righe che soddisfano il criterio
unprocessed_food<- Nic_data[Nic_data$...1 == nome_cercato, -2]
unprocessed_food<-unprocessed_food[-2,]




# Crea un grafico a punti
dati_tidy <- as.data.frame(t(unprocessed_food))

# Crea un grafico a punti
grafico_punti <- ggplot(data = dati_tidy, aes(y = 1:nrow(dati_tidy), x = V1)) +
  geom_point(color = "red")  +
  labs(x = "Elemento", y = "Valore") +
  theme_minimal()

# Stampa il grafico a punti
print(grafico_punti)

# Combina i due grafici in uno solo
grafici_sovrapposti <- grafico_linea + grafico_punti

ggplot() +
  geom_line(data = carburante, aes(x = ANNO, y = Media_Prezzo), color = "blue", size = 1) +  # Grafico dei prezzi
  geom_line(data = unprocessed_food, aes(y = 1:nrow(dati_tidy), x = ), color = "red", size = 1) +  # Grafico dell'IVA
  labs(
    title = "Confronto Prezzo e IVA medi per Anno",
    x = "Anno",
    y = "Valore"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) +
  guides(color = guide_legend(title = "Legenda"))

# Stampa i grafici sovrapposti
print(grafici_sovrapposti)


