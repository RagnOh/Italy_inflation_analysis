
#Fix data with the creation of an appropriate table

country <- "Italy"
Italy_interest_rates<- Interest_data[Interest_data$...2 == country, -2]
Italy_interest_rates <- Italy_interest_rates[, -1]

Italy_interest_rates <- Italy_interest_rates[-c(1, 3), ]
Italy_interest_rates <- Italy_interest_rates[-c(1, 1), ]

print(rev(Italy_interest_rates))

inversione <- data.frame(
  value = c(1, 2, 3, 4, 5)
)

inversione[1, ] <- rev(Italy_interest_rates[1, ])
