
#Fix data with the creation of an appropriate table

country <- "Italy"
Italy_interest_rates<- Interest_data[Interest_data$...2 == country, -2]
Italy_interest_rates <- Italy_interest_rates[, -1]

Italy_interest_rates <- Italy_interest_rates[-c(1, 3), ]
Italy_interest_rates <- Italy_interest_rates[-c(1, 1), ]

Italy_interest_rates_reverse <- t(Italy_interest_rates)

