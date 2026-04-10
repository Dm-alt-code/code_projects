
library(openxlsx)
library(forecast)
library(caschrono)

#1
data <- read.xlsx("D:\\5 курс\\Аналіз часових рядів і прогнозування\\Лаб№2\\Furnace_temp.xlsx")
print(data)
furn_t <- as.double(data[,"Furn_t"])
str(furn_t)
furn_t_ts <- ts(furn_t)
plot(furn_t_ts)

#BoxCox.lambda(furn_t_ts)

lambda1 <- BoxCox.lambda(furn_t_ts, lower = 0)
fit <- Arima(furn_t_ts, order = c(2,0,0), lambda = lambda1)
plot(forecast(fit))

furn_t_ts %>% BoxCox(lambda = lambda1) %>% autoplot()


furn_ts_diff = diff(furn_t_ts)
plot(furn_ts_diff)

acf(furn_ts_diff)
pacf(furn_ts_diff)

# Якщо acf затухає дуже повільно, а pcf обривається
# lag 1 - це індикатор необхідності диференціювання
acf(furn_t_ts)
pacf(furn_t_ts) # наша модель схожа на AR(2)
auto.arima(furn_t_ts, ic = "aicc")
auto.arima(furn_t_ts, ic = "bic")
auto.arima(furn_t_ts, ic = "aicc", max.d = 3)
# AR(p)
# ACF - затухає експоненційно або затухаючою синусоїдою
# PACF - обривається після lag p

armaselect(furn_t_ts, max.p = 15, max.q = 15, nbmod = 10)
# В даному випадку оптимальною є AR(2) (1-й порядок) - для неї SBC мінімальне

auto.arima(furn_t_ts, max.p = 4, max.q = 4, ic = "aicc")
auto.arima(furn_t_ts, max.p = 4, max.q = 4, ic = "bic")
library(astsa)
sarima(furn_t_ts, 2,0,0) # aic = 0.9700016
sarima(furn_t_ts, 1,0,1) # aic = 0.9932457
sarima(furn_t_ts, 2,0,1) # aic = 0.9949207
sarima(furn_t_ts, 0,0,2) # aic = 0.9965598
sarima(furn_t_ts, 1,0,2) # aic = 1.001282
# Рівняння:
# (1 - fi_1 B - fi_2 B^2) Z_t_dot = a_t

#2
data <- read.xlsx("D:\\5 курс\\Аналіз часових рядів і прогнозування\\Лаб№2\\US_month_unempl.xlsx")
unemploy_ts <- ts(data[,1], start = c(1980,1), frequency = 12)
summary(unemploy_ts)
print(unemploy_ts)
plot(unemploy_ts)

acf(unemploy_ts)
pacf(unemploy_ts)

# diff 1
unemploy_ts_diff = diff(unemploy_ts)
plot(unemploy_ts_diff)

acf(unemploy_ts_diff)
pacf(unemploy_ts_diff)

# Врахуємо сезонність + 1 diff
unemploy_ts_diff_s = diff(unemploy_ts_diff, lag = 12)
plot(unemploy_ts_diff_s)

acf(unemploy_ts_diff_s)
pacf(unemploy_ts_diff_s)

# автоматичний метод
armaselect(unemploy_ts_diff, max.p = 5, max.q = 9, nbmod = 20) # nbmod - an integer, the number of models that will be returned
auto.arima(unemploy_ts, max.p = 5, max.q = 5, max.d = 2, max.D = 5, seasonal = TRUE)
# ARIMA(1,1,3)(2,0,1)

# ARMA(3,1) sbc = -1367.132
# ARMA(3,2) sbc = -1365.258
# ARMA(2,1) sbc = -1363.904

arima(unemploy_ts, order = c(3, 1, 5)) # aic = -313.02
arima(unemploy_ts, order = c(3, 1, 6)) # aic = -311.04
arima(unemploy_ts, order = c(1, 1, 3)) # aic = -298.75
arima(unemploy_ts, order = c(2, 0, 1)) # aic = -287.68

arima(unemploy_ts, order = c(3, 1, 5), seasonal = list(order=c(1,0,0), period=12)) # aic = -295.12
arima(unemploy_ts, order = c(3, 1, 5), seasonal = list(order=c(0,1,0), period=12)) # aic = -89.76
arima(unemploy_ts, order = c(3, 1, 5), seasonal = list(order=c(0,0,1), period=12)) # aic = -305.96

sarima(unemploy_ts, order = c(3, 1, 5), seasonal = list(order=c(0,0,1), period=12))

arima(unemploy_ts, order = c(1, 1, 3), seasonal = list(order=c(1,0,0), period=12)) # aic = -298.79
arima(unemploy_ts, order = c(1, 1, 3), seasonal = list(order=c(0,1,0), period=12)) # aic = -41.62
arima(unemploy_ts, order = c(1, 1, 3), seasonal = list(order=c(0,0,1), period=12)) # aic = -300.17

# astsa
library(astsa)
sarima(unemploy_ts, 3,1,3)
sarima(unemploy_ts, 3,2,3) #
sarima(unemploy_ts, 1,1,3) #
sarima(unemploy_ts, 2,0,1)

ar313=sarima(unemploy_ts, 3,1,3)
(ar313$AIC)*length(unemploy_ts)

sarima(unemploy_ts, 3,2,3, P = 1, D = 0, Q = 1, S = 12)
sarima(unemploy_ts, 3,2,3, P = 0, D = 0, Q = 0, S = 12)
sarima(unemploy_ts, 3,2,3, P = 0, D = 2, Q = 1, S = 12)

sarima(unemploy_ts, 1,1,3, P = 0, D = 2, Q = 1, S = 12)
sarima(unemploy_ts, 1,1,3, P = 0, D = 1, Q = 1, S = 12)

sarima(unemploy_ts, 2,0,1, P = 0, D = 1, Q = 1, S = 12)
sarima(unemploy_ts, 2,0,1, P = 0, D = 2, Q = 1, S = 12)

ar113=sarima(unemploy_ts, 1,1,3, P = 0, D = 1, Q = 1, S = 12)
(ar113$AIC)*length(unemploy_ts)

ar201=sarima(unemploy_ts, 2,0,1, P = 0, D = 2, Q = 1, S = 12)
(ar201$AIC)*length(unemploy_ts)

