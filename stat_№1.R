
library(dplyr)
library(caret)
library(glmnet)
library(measures)
library(faraway)

data("cars", package="caret")
# cars
data_set <- select(cars, -c("coupe", "wagon", "Saturn"))
response <- "Price"

fraction <- 0.85
train <- sample(1:nrow(data_set), size=fraction*nrow(data_set))
test <- -train

# fat
data_set <- select(fat, -c("siri", "free", "adipos", "density"))
response <- "brozek"

fraction <- 0.85
train <- sample(1:nrow(data_set), size=fraction*nrow(data_set))
test <- -train

# OLS
ols.fit <- lm(paste(response, "~ ."), data = data_set[train,])
summary(ols.fit)
# cars:
# 
# Multiple R-squared:  0.9183,	Adjusted R-squared:  0.9166
# fat:
# 
# 

ols.pred <- predict(ols.fit, newdata = data_set[test,], type="response")

print(paste("train-MSE:",
            MSE(ols.fit$fitted.values, data_set[train, response])))

print(paste("test-MSE:",
            MSE(ols.pred, data_set[train, response])))













lm0 <- lm(paste(Price~. , data = data_set[train,]))
summary(lm0)


cars_lm0_pred <- predict(lm0, data_set)
MSE(truth = data_set$Price, response = cars_lm0_pred)

lm1 <- lm(Price~. , data = data_set_train)
summary(lm1)

cars_lm1_pred <- predict(lm1, data_set_test)
MSE(truth = data_set_test$Price, response = cars_lm1_pred)


cars_model_ridge_prediction <- predict(cars_model_ridge, s = 4, newx = cars_test_mat)
MSE(data_test$Price, cars_model_ridge_prediction)




cars_model_lasso <- glmnet(x, data_set_train$Price, alpha = 1, lambda = grid)
cars_model_lasso_prediction <- predict(cars_model_lasso, s = grid, newx = cars_test_mat)
plot(cars_model_lasso)
MSE(data_set_test$Price, cars_model_lasso_prediction)







fat_lm0_pred <- predict(lm0, data_df)
MSE(truth = fat_df$brozek, response = fat_lm0_pred)










