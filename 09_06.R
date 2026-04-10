
data <- data.frame(
  Company = c("Adobe", "Amazon", "Apple", "Cisco", "Dell", "eBay", "Google", "Hewlett_Packard", "Intel", "Microsoft", "Oracle", "Qualcomm", "Symantec", "Texas_Instruments", "Yahoo"),
  Market_cap = c(17254, 66336, 252664, 125246, 24153, 31361, 153317, 90280, 105625, 219195, 127578, 67370, 11793, 30560, 19132),
  Net_cash = c(1370, 5070, 45800, 23700, 8800, 6720, 30300, 6400, 22100, 38450, 9914, 2870, 1040, 3557, 7230),
  cash_flow = c(1118, 3293, 10159, 10173, 3906, 2908, 9316, 13379, 11170, 24073, 8681, 7172, 1693, 2643, 1310),
  Cash_flow_as_percent_of_cap = c(6.48, 4.96, 4.02, 8.12, 16.17, 9.27, 6.08, 14.82, 10.58, 10.98, 6.80, 10.65, 14.36, 8.65, 6.85),
  Cash_and_cash_flow_as_percent_of_cap = c(14.42, 12.61, 22.15, 27.05, 52.61, 30.70, 25.84, 21.91, 31.50, 28.52, 14.58, 14.91, 23.17, 20.29, 44.64),
  Current_dividend_rate = c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.8, 3.4, 2.1, 0.8, 1.8, 0.0, 1.9, 0.0),
  Dividend_at_60_percent_payout = c(3.9, 3.0, 2.4, 4.9, 9.7, 5.6, 3.6, 8.9, 6.3, 6.6, 4.1, 6.4, 8.6, 5.2, 4.1)
)

data$CashFlow_to_MarketCap_Ratio <- data$cash_flow/data$Market_cap

data_copy <- data

library(ggplot2)

# ïîòî÷í³ äèâ³äåíäè
ggplot(data, aes(x = Market_cap, y = Current_dividend_rate)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = Market_cap, y = Current_dividend_rate)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)

# ìàéáóòí³ äèâ³äåíäè
ggplot(data, aes(x = Market_cap, y = Dividend_at_60_percent_payout)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

ggplot(data, aes(x = Market_cap, y = Dividend_at_60_percent_payout)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE)



library(glmnet)
# Ñòàíäàğòèçàö³ÿ çì³ííèõ
standardized_data <- as.matrix(scale(data[, c("Market_cap", "Net_cash", "cash_flow", "CashFlow_to_MarketCap_Ratio")]))


current_div_response <- data$Current_dividend_rate
future_div_response <- data$Dividend_at_60_percent_payout

# Ridge ğåãğåñ³ÿ äëÿ ïîòî÷íî¿ ñòàâêè äèâ³äåíä³â
ridge_current_div_model <- glmnet(standardized_data, current_div_response, alpha = 0)
plot(ridge_current_div_model)
#summary(ridge_current_div_model)
# Ridge ğåãğåñ³ÿ äëÿ ìàéáóòí³õ äèâ³äåíä³â ïğè 60% âèïëàò³
ridge_future_div_model <- glmnet(standardized_data, future_div_response, alpha = 0)
plot(ridge_future_div_model)
#summary(ridge_future_div_model)


# Lasso regression for current dividend rate
lasso_current_div_model <- glmnet(standardized_data, current_div_response, alpha = 1)
plot(lasso_current_div_model)
#summary(lasso_current_div_model)
# Lasso regression for future dividend at 60% payout
#lasso_future_div_model <- glmnet(standardized_data, future_div_response, alpha = 1)
#plot(lasso_future_div_model)



data <- data_copy

summary(data)
cor(data[, -1])

cor(data[, c("Current_dividend_rate", "Dividend_at_60_percent_payout", "Market_cap", 
             "Net_cash", "cash_flow", "CashFlow_to_MarketCap_Ratio")])


model <- lm(Current_dividend_rate ~ Market_cap + Net_cash + cash_flow + CashFlow_to_MarketCap_Ratio, data)
summary(model)
plot(model)






# Îö³íêà ìîäåë³
predicted_current_dividends <- predict(model, newdata = data)
mse <- mean((predicted_current_dividends - data$Current_dividend_rate)^2)
r_squared <- 1 - mse/var(data$Current_dividend_rate)

cat("Ñåğåäí³é êâàäğàòè÷íèé â³äñîòîê ïîìèëîê:", mse, "\n")
cat("R-êâàäğàò:", r_squared, "\n")


model_2 <- lm(Dividend_at_60_percent_payout ~ Market_cap + Net_cash + cash_flow + Cash_flow_to_Market_cap_ratio, data)
summary(model_2)

# Îö³íêà ìîäåë³
predicted_current_dividends <- predict(model_2, newdata = data)
mse <- mean((predicted_current_dividends - data$Dividend_at_60_percent_payout)^2)
r_squared <- 1 - mse/var(data$Dividend_at_60_percent_payout)

cat("Ñåğåäí³é êâàäğàòè÷íèé â³äñîòîê ïîìèëîê:", mse, "\n")
cat("R-êâàäğàò:", r_squared, "\n")



##############
# Âèêîğèñòàííÿ äåğåâà ğ³øåíü
library(rpart)
model_tree <- rpart(Current_dividend_rate ~ Market_cap + Net_cash + cash_flow + Cash_flow_to_Market_cap_ratio, data = data)
summary(model_tree)

# Âèêîğèñòàííÿ âèïàäêîâîãî ë³ñó
library(randomForest)
model_rf <- randomForest(Current_dividend_rate ~ Market_cap + Net_cash + cash_flow + Cash_flow_to_Market_cap_ratio, data = data)
summary(model_rf)


# Âèêîğèñòàííÿ ìåòîäó îïîğíèõ âåêòîğ³â
library(e1071)
model_svm <- svm(Current_dividend_rate ~ Market_cap + Net_cash + cash_flow + Cash_flow_to_Market_cap_ratio, data = data)
summary(model_svm)









