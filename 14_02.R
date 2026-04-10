library(HSAUR2)
library(extRemes)

data_planet <- planets


fit_g <- fevd(mass_data, type = "Gumbel")
# Відображення параметрів підгонки
cat("Параметри встановленого розподілу:\n")
print(fit_g)
plot(fit_g)

fit_gev <- fevd(mass_data, type = "GEV")
# Відображення параметрів підгонки
cat("Параметри встановленого розподілу:\n")
print(fit_gev)
plot(fit_gev)


hist(data_planet$mass, main = "Histogram of Exoplanet Masses", 
     xlab = "Mass", col = "lightblue", border = "black", breaks=50)

mass_data <- planets$mass

# Побудова графіку для візуального огляду розподілу мас
plot(mass_data, main = "Exoplanet Mass Distribution", xlab = "Planet Index", ylab = "Mass", type = "l")

# Пошук екстремальних значень за допомогою функції "fevd"
extreme_fit <- fevd(mass_data, type = "GEV")
extreme_fit
# Визначення порогу для визначення екстремальних значень
threshold <- quantile(mass_data, probs = 0.95)

# Визначення екстремальних значень
extreme_values <- mass_data[mass_data > threshold]

# Виведення результатів
cat("Кількість екстремальних значень:", length(extreme_values), "\n")
cat("Мінімальне значення екстремальних мас:", min(extreme_values), "\n")
cat("Максимальне значення екстремальних мас:", max(extreme_values), "\n")

# Візуалізація екстремальних значень
points(which(mass_data > threshold), mass_data[mass_data > threshold], col = "red", pch = 19)


# Емпірична функція розподілу (ECDF) для мас екзопланет
ecdf_mass <- ecdf(mass_data)

# Теоретична функція розподілу (нормальний розподіл для порівняння)
cdf_normal <- pnorm(sort(mass_data), mean = mean(mass_data), sd = sd(mass_data))

# Графік ECDF та теоретичної CDF
plot(ecdf_mass, xlab = "Mass", ylab = "Cumulative Probability", col = "blue")
lines(sort(mass_data), cdf_normal, col = "red")

# Визначення різниці між ECDF та теоретичною CDF
ks_statistic <- max(abs(ecdf_mass(sort(mass_data)) - cdf_normal))
cat("Статистика =", ks_statistic, "\n")




qqnorm(data_planet$mass)
qqline(data_planet$mass)


shapiro.test(data_planet$mass)
# Значення мале, тому ймовірність помилитися, якщо відхилити 
# нульову гіпотезу велика, тому відхиляємо нульову гіпотезу.







model <- lm(mass ~ period + eccen, data_planet)
summary(model)








