


names(benzina_data)




media_prezzi_annuali <- aggregate(PREZZO ~ ANNO, benzina_data, mean)

# Crea il grafico a linea per la media dei prezzi annuali
ggplot(media_prezzi_annuali, aes(x = ANNO, y = PREZZO)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Media dei Prezzi per Anno",
    x = "Anno",
    y = "Prezzo Medio Annuale"
  )+theme_minimal()

media_iva_annuale <- aggregate(NETTO ~ ANNO, benzina_data, mean)

# Crea il grafico a linea per la media dei prezzi annuali
ggplot(media_iva_annuale, aes(x = ANNO, y = NETTO)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Media dell'IVA per Anno",
    x = "Anno",
    y = "IVA Media Annuale"
  )+theme_minimal()

ggplot() +
  geom_line(data = media_prezzi_annuali, aes(x = ANNO, y = PREZZO), color = "blue", size = 1) +  # Grafico dei prezzi
  geom_line(data = media_iva_annuale, aes(x = ANNO, y = NETTO), color = "red", size = 1) +  # Grafico dell'IVA
  labs(
    title = "Confronto Prezzo e IVA medi per Anno",
    x = "Anno",
    y = "Valore"
  ) +
  theme_minimal()



names(gasolio_data)

media_prezzoG_annuale <- aggregate(PREZZO ~ ANNO, gasolio_data, mean)
ggplot(media_prezzoG_annuale, aes(x = ANNO, y = PREZZO)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Media dei Prezzi per Anno",
    x = "Anno",
    y = "Prezzo Medio Annuale"
  )+theme_minimal()

ggplot() +
  geom_line(data = media_prezzi_annuali, aes(x = ANNO, y = PREZZO), color = "blue", size = 1) +  # Grafico dei prezzi
  geom_line(data = media_prezzoG_annuale, aes(x = ANNO, y = PREZZO), color = "red", size = 1) +  # Grafico dell'IVA
  labs(
    title = "Confronto Prezzo e IVA medi per Anno",
    x = "Anno",
    y = "Valore"
  ) +
  scale_color_manual(values = c("Prezzo" = "blue", "IVA" = "red")) +
  guides(color = guide_legend(title = "Legenda"))

names(corrente_data)

corrente_italia<-subset(corrente_data,Country=="Italy")

print(corrente_italia)

corrente_italia$Date <- as.Date(corrente_italia$Date, format = "%Y-%m-%d")
ggplot(corrente_italia, aes(x = Date, y = Price..EUR.MWhe.,group = 1)) +
  geom_line() +
  labs(
    title = "Prezzo in Italia MWh",
    x = "Data",
    y = "Prezzo"
  ) +
  scale_x_date(limits = c(as.Date("2019-01-01"), max(corrente_italia$Date))) +
  
  theme_minimal()+theme(axis.text.x = element_text(angle = 270, hjust = 1))

media_costo_carburante<- bind_rows(benzina_data, gasolio_data)

merged_data <- media_costo_carburante %>% 
  mutate(Mese_Anno = paste(NOME_MESE, ANNO, sep = "_"))
 
  

# Calcola la media dei prezzi per ogni mese e anno
media_prezzi <- merged_data %>%
  group_by(Mese_Anno) %>%
  summarise(Media_Prezzo = mean(PREZZO))


media_prezzi <- media_prezzi %>%
  separate(Mese_Anno, into = c("Mese", "ANNO"), sep = "_", remove = FALSE)

# Rimuovi la colonna "Mese_Anno" se non è più necessaria
media_prezzi <- media_prezzi %>%
  select(-Mese_Anno)

media_prezzi <- media_prezzi %>%
  arrange(ANNO)

ordine_mesi <- c("Gennaio", "Febbraio", "Marzo", "Aprile", "Maggio", "Giugno", "Luglio", "Agosto", "Settembre", "Ottobre", "Novembre", "Dicembre")

# Ordina la tabella in base all'ordine dei mesi
media_prezzi_carburante <- media_prezzi %>%
  arrange(ANNO,factor(Mese, levels = ordine_mesi))

# Media dei prezzi per ogni mese e anno, mantenendo l'ordine dei mesi
print(media_prezzi_carburante)

ggplot(data = media_prezzi_carburante, aes(x = factor(ANNO), y = Media_Prezzo, fill = Mese)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Anno", y = "Prezzo") +
  theme_minimal()

