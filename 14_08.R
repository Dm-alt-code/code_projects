

# Голоси за кандидатів
votes <- c(130008, 85506, 51059, 27349, 17356, 8470, 6289, 3444, 2757)

(factorial(45)/(factorial(19)*factorial(45-19)))*((0.39)^(19))*((0.61)^(45-19))


library(extRemes)

fit_g <- fevd(votes, type = "Gumbel")
# Відображення параметрів підгонки
cat("Параметри встановленого розподілу:\n")
print(fit_g)
plot(fit_g)

fit_gev <- fevd(votes, type = "GEV")
# Відображення параметрів підгонки
cat("Параметри встановленого розподілу:\n")
print(fit_gev)
plot(fit_gev)

total_votes <- sum(votes)
# Ймовірність другого туру
probability_runoff <- sum(votes/total_votes<0.4)/length(votes)
votes/total_votes
cat("Ймовірність другого туру:", probability_runoff, "\n")


