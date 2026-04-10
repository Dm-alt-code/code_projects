library(forecast)

data <- read.table(file = "D:\\5 курс\\Аналіз часових рядів і прогнозування\\Лабор. No 1\\Maine.dat", header = TRUE)
print(data[,1])
unemploy_ts <- ts(data[,1], start = c(1996,1), frequency = 12)
summary(unemploy_ts)
print(unemploy_ts)
plot(unemploy_ts)
# наявна сезонність (періодичність)

unemploy_ts_subset_1 <- window(unemploy_ts, start=c(1997,1), end=c(1997,12))
plot(unemploy_ts_subset_1, type = "o")
unemploy_ts_subset_2 <- window(unemploy_ts, start=c(1999,1), end=c(1999,12))
plot(unemploy_ts_subset_2, type = "o")
unemploy_ts_subset_3 <- window(unemploy_ts, start=c(2003,1), end=c(2003,12))
plot(unemploy_ts_subset_3, type = "o")


ggseasonplot(unemploy_ts)
# 
# 

unemploy_ts_subset_task_2 <- window(unemploy_ts, start=c(2000,1), end=c(2004,12))
print(unemploy_ts_subset_task_2)
plot(unemploy_ts_subset_task_2, type="o")
# 
ggseasonplot(unemploy_ts_subset_task_2)
# 
plot(unemploy_ts_subset_task_2[max(unemploy_ts_subset_task_2)], type="o")

stl_un <- stl(unemploy_ts, s.window = "periodic")
print(stl_un)
plot(stl_un)

# 
# 
# 

#### --- MIN --- MAX ---
min_i <- which.min(unemploy_ts_subset_task_2)
year_min <- floor(time(unemploy_ts_subset_task_2)[min_i])
month_min <- cycle(unemploy_ts_subset_task_2)[min_i]
min_date <- paste(month_min, ".", year_min, sep = '')

max_i <- which.max(unemploy_ts_subset_task_2)
year_max <- floor(time(unemploy_ts_subset_task_2)[max_i])
month_max <- cycle(unemploy_ts_subset_task_2)[max_i]
max_date <- paste(month_max, ".", year_max, sep = '')

plot(unemploy_ts_subset_task_2, ylab = "unemploy")
points(x = time(unemploy_ts_subset_task_2)[min_i], y = min(unemploy_ts_subset_task_2),
       col = "red", pch = 1, cex = 1.25)
points(x = time(unemploy_ts_subset_task_2)[max_i], y = max(unemploy_ts_subset_task_2),
       col = "blue", pch = 1, cex = 1.25)
grid()
legend("bottomright", c(min_date, max_date), col = c("red","blue"), pch = 1)







# 2
library(openxlsx)
data_2 <- read.xlsx("D:\\5 курс\\Аналіз часових рядів і прогнозування\\Лабор. No 1\\TUI_2004_2007.xlsx")
print(data_2)
closed <- as.double(data_2[, "Close"])
str(closed)
# 
closed_new <- closed[1:716]
# 
closed_new_ts <- ts(closed_new, start = c(2004, 1, 1), frequency = 365)
plot(closed_new_ts)
Z <- log(closed_new_ts)
plot(Z, type = "o")

Z_diff <- diff(Z)
plot(Z_diff)
acf(Z_diff)
pacf(Z_diff)
hist(Z_diff, breaks=50)

qqnorm(Z_diff)
qqline(Z_diff)

shapiro.test(Z_diff)
# 


