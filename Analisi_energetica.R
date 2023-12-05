


names(benzina_data)

#Analisi andamento prezzo benzina

prezzi_annuali_benzina <- benzina_data[benzina_data$ANNO >= 2019, ]
prezzi_annuali_benzina$mese<-paste(prezzi_annuali_benzina$NOME_MESE,prezzi_annuali_benzina$ANNO, sep = " ")
prezzi_annuali_benzina$mese <- factor(prezzi_annuali_benzina$mese,levels = unique(prezzi_annuali_benzina$mese))


ggplot(prezzi_annuali_benzina, aes(factor(ANNO), PREZZO)) + geom_boxplot() +
 labs(
  title= "Price variation of petrol from 2019 to 2023",
  x = "Year", 
  y = "Price")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 


# Crea il grafico a linea per la media dei prezzi annuali
ggplot(prezzi_annuali_benzina, aes(x = mese, y = PREZZO,group = 1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Price variation of petrol from 2019 to 2023",
    x = "Date",
    y = "Price"
  )+theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Analisi impatto tasse su prezzo attuale

ggplot(prezzi_annuali_benzina) +
  geom_line(aes(x = mese, y = PREZZO,group = 1), color = "blue", size = 1) +  # Grafico dei prezzi
  geom_line(aes(x = mese, y = NETTO,group = 1), color = "red", size = 1) +  # Grafico dell'IVA
  labs(
    title = "Price of petrol without taxes and with taxes",
    x = "Date",
    y = "Price"
  ) +
  scale_color_manual(values=c("Prezzo"="blue", "IVA" = "red"))+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dati <- data.frame(
  categoria = c("Taxes", "Net value"),
  valore = c(1040.51, 690.29)
)

dati$percentuale <- scales::percent(dati$valore / sum(dati$valore))

ggplot(dati, aes(x = "", y = valore, fill = categoria, label = percentuale)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Grafico a Torta", fill = "Category") +
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())


#Riassunto prezzo benzina
ggplot(prezzi_annuali_benzina, aes(factor(ANNO), PREZZO)) + geom_boxplot() +
  labs(
    title= "Price variation of petrol from 2019 to 2023",
    x = "Year", 
    y = "Price")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 

#######

#Analisi gasolio

names(gasolio_data)

prezzoG_annuale <- gasolio_data[gasolio_data$ANNO >= 2019, ]
prezzoG_annuale$mese<-paste(prezzoG_annuale$NOME_MESE,prezzoG_annuale$ANNO, sep = " ")
prezzoG_annuale$mese <- factor(prezzoG_annuale$mese,levels = unique(prezzoG_annuale$mese))

ggplot(prezzoG_annuale, aes(factor(ANNO), PREZZO)) + geom_boxplot() +
  labs(
    title= "Price variation of diesel from 2019 to 2023",
    x = "Year", 
    y = "Price")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 

ggplot(prezzoG_annuale, aes(x = mese, y = PREZZO,group=1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Price variation of diesel from 2019 to 2023",
    x = "Date",
    y = "Price"
  )+theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#Analisi impatto tasse su gasolio
ggplot(prezzoG_annuale) +
  geom_line(aes(x = mese, y = PREZZO,group = 1), color = "blue", size = 1) +  # Grafico dei prezzi
  geom_line(aes(x = mese, y = NETTO,group = 1), color = "red", size = 1) +  # Grafico dell'IVA
  labs(
    title = "Comparison between diesel price without taxes and including taxes",
    x = "Date",
    y = "Price"
  ) +
  scale_color_manual(values=c("Prezzo"="blue", "IVA" = "red"))+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

dati <- data.frame(
  categoria = c("Taxes", "Net value"),
  valore = c(904.06, 685.59)
)

dati$percentuale <- scales::percent(dati$valore / sum(dati$valore))

ggplot(dati, aes(x = "", y = valore, fill = categoria, label = percentuale)) +
  geom_bar(stat = "identity") +
  geom_text(position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  labs(title = "Grafico a Torta", fill = "Categoria") +
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.text.x = element_blank())

#Confronto prezzo benzina e gasolio
ggplot() +
  geom_line(data = prezzi_annuali_benzina, aes(x = mese, y = PREZZO,group=1), color = "blue", size = 1) +  
  geom_line(data = prezzoG_annuale, aes(x = mese, y = PREZZO,group=1), color = "red", size = 1) +  
  labs(
    title = "Comparison between petrol price and diesel price",
    x = "Date",
    y = "Price"
  ) +theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))


#Analisi prezzo corrente
names(corrente_data)

corrente_italia<-subset(corrente_data,Country=="Italy")
corrente_italia$Anno <- substr(corrente_italia$Date, 1, 4)
corrente_italia<-corrente_italia[corrente_italia$Date >= 2019, ]
corrente_italia$mese_anno<-prezzi_annuali_benzina$mese

corrente_italia$Prezzo <- corrente_italia$Price..EUR.MWhe.
corrente_italia$Price..EUR.MWhe. <- NULL 

ggplot(corrente_italia, aes(x = Date, y = Prezzo,group=1)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Price variation of electricity from 2019 to 2023 in Italy (MWh)",
    x = "Date",
    y = "Price"
  ) +
  theme_minimal()+theme(axis.text.x = element_text(angle = 270, hjust = 1))

corrente_italia$Prezzo2<-corrente_italia$Prezzo+1200

ggplot() +
  geom_line(data = prezzi_annuali_benzina, aes(x = mese, y = PREZZO,group=1), color = "blue", size = 1) +  
  geom_line(data = prezzoG_annuale, aes(x = mese, y = PREZZO,group=1), color = "red", size = 1) +  
  geom_line(data = corrente_italia, aes(x = mese_anno, y = Prezzo2,group=1), color = "green", size = 1) + 
  labs(
    title = "Comparison between price trend of petrol,diesel and electricity from 2019 to 2023",
    x = "Date",
    y = "Price"
  ) +theme_minimal()+
  theme(axis.text.x = element_text(angle = 270, hjust = 1))

#riassunto andamento prezzi su periodo
ggplot(corrente_italia, aes(factor(Anno), Prezzo)) + geom_boxplot() +
  labs(
    title= "Price variation of electricity from 2019 to 2023 (Mwh)",
    x = "Year", 
    y = "Price")+
  theme(axis.text.x = element_text(angle = 270, hjust = 0.5)) 


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

carburante <- media_prezzi_carburante[media_prezzi_carburante$ANNO >= 2019, ]




