
# a

compensation <- c(1045887, 995424, 988591, 627104, 455690, 349563, 256380)
total_revenue <- c(429, 645, 107, 237, 3300, 33, 41)*10^6
gov_t <- c(9, 0, 41, 10, 125, 11, 9)*10^6
# Розрахунок співвідношення "зарплата гендиректора до загального доходу"
ceo_ratio <- (compensation/total_revenue)*100
ceo_ratio

# Розрахунок частки урядового фінансування
govt_ratio <- (gov_t/total_revenue)*100
govt_ratio




# b

names <- c("J Bezos", "E Musk", "B Arnault", "B Gates", "M Zuckerberg", "W Buffett", "L Ellison", "L Page", "S Brin", "M Ambani")
net_worth <- c(177, 151, 150, 124, 97, 96, 93, 92, 89, 85)
age <- c(57, 49, 72, 65, 36, 90, 76, 48, 47, 63)

data <- data.frame(names, net_worth, age)
# Виведення результатів
print(data)

library(ggplot2)

ggplot(data, aes(x = names, y = net_worth)) +
  geom_bar(stat = "identity", fill = "blue", width = 0.6) +
  labs(title = "Net Worth of the Richest Individuals",
       x = "Names",
       y = "Net Worth (in billion USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


boxplot(net_worth, main = "Стан особистих активів найбагатших осіб")

plot(age, net_worth, xlab = "Вік", ylab = "Стан особистих активів")









mean_net_worth <- mean(net_worth)
median_net_worth <- median(net_worth)
sd_net_worth <- sd(net_worth)
quantiles_net_worth <- quantile(net_worth)


mean_net_worth
median_net_worth
sd_net_worth
quantiles_net_worth

