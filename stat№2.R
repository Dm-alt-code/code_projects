library(caret)
library(e1071)
library(ggplot2)
library(openxlsx)
library(dplyr)
library(haven)
library(naivebayes)

data(iris)
head(iris)
table(iris$Species)

# petal - листок
# sepal - чашелисток

rows_ind <- createDataPartition(iris$Species, p = 0.8, list = FALSE, times = 1)
iris_train <- iris[rows_ind, ] # на навчання

ggplot(iris, aes(Sepal.Length, Sepal.Width, col = Species)) + 
  geom_point(size = 3)

ggplot(iris, aes(Petal.Length, Petal.Width, col = Species)) + 
  geom_point(size = 3)

model_iris <- naiveBayes(Species ~ ., data = iris_train)

pred_test <- predict(model_iris, iris[-rows_ind, -5])

tab <- table(pred_test, iris[-rows_ind, 5], dnn = c("pred", "true_vals"))
tab 

#confusion matrix
confusionMatrix(data = pred_test, reference = iris[-rows_ind, 5])

fitcontrol <- trainControl(method = "cv", number = 10)
fit <- train(Species ~ ., data = iris_train, method = "nb", trControl = fitcontrol)
confusionMatrix(fit)

# Part 2
car_eval_df<- read.xlsx("D:\\5 курс\\Статистичне навчання\\Лаб№2\\LAB_2 (N. Bayes) everything\\Завдання\\Car_eval_data.xlsx")

car_ev_df <- mutate_if(car_eval_df, is.character, as.factor)

num_obs <- nrow(car_ev_df)

rows_ind <- sample(1:num_obs, 0.7*num_obs)
cars_ev_train <- car_ev_df[rows_ind, ]

model_car <- naiveBayes(Acceptability ~ ., data = cars_ev_train)

pred_test <- predict(model_car, car_ev_df[-rows_ind, -7])

tab <- table(pred_test, car_ev_df[-rows_ind, 7], dnn = c("pred", "true_vals"))
tab 

#confusion matrix
confusionMatrix(data = pred_test, reference = car_ev_df[-rows_ind, 7])


fitcontrol <- trainControl(method = "cv", number = 10)
fit <- train(Acceptability ~ ., data = cars_ev_train, method = "nb", trControl = fitcontrol)
confusionMatrix(fit)

# Об'єднюємо в класи
car_ev_df %>%
  group_by(Acceptability) %>%
  summarise(len = length(Acceptability), prop = len/nrow(car_ev_df)*100)

# v_good
levels(car_ev_df$Acceptability)[c(2,4)] <- "v_good"
car_ev_df %>%
  group_by(Acceptability) %>%
  summarise(len = length(Acceptability), prop = len/nrow(car_ev_df)*100)

num_obs <- nrow(car_ev_df)

rows_ind <- sample(1:num_obs, 0.7*num_obs) # 70% на навчання
cars_ev_train <- car_ev_df[rows_ind, ] # дані на навчання

m <- naiveBayes(Acceptability ~ ., data = cars_ev_train)

pred_test <- predict(m, car_ev_df[-rows_ind, -7])

tab <- table(pred_test, car_ev_df[-rows_ind, 7], dnn = c("pred", "true_vals"))
tab 

#confusion matrix
confusionMatrix(data = pred_test, reference = car_ev_df[-rows_ind, 7])


# acc_v_good
levels(car_ev_df$Acceptability)[c(1,2)] <- "acc_v_good"
car_ev_df %>%
  group_by(Acceptability) %>%
  summarise(len = length(Acceptability), prop = len/nrow(car_ev_df)*100)

num_obs <- nrow(car_ev_df)

rows_ind <- sample(1:num_obs, 0.7*num_obs)
cars_ev_train <- car_ev_df[rows_ind, ]

m <- naiveBayes(Acceptability ~ ., data = cars_ev_train)

pred_test <- predict(m, car_ev_df[-rows_ind, -7])

tab <- table(pred_test, car_ev_df[-rows_ind, 7], dnn = c("pred", "true_vals"))
tab 

#confusion matrix
confusionMatrix(data = pred_test, reference = car_ev_df[-rows_ind, 7])

# Part 3
ed_ch_df <- read_dta("D:\\5 курс\\Статистичне навчання\\Лаб№2\\LAB_2 (N. Bayes) everything\\Завдання\\hsbdemo.dta")
ed_ch_df <- ed_ch_df[-1]

plot(science ~ math, data = ed_ch_df,
     col = c("red", "blue", "green")[as.integer(prog)],
     pch = c(1, 2, 3)[as.integer(prog)])
cor(ed_ch_df$science, ed_ch_df$math)

plot(read ~ math, data = ed_ch_df,
     col = c("red", "blue", "green")[as.integer(prog)],
     pch = c(1, 2, 3)[as.integer(prog)])
cor(ed_ch_df$read, ed_ch_df$math)


####
plot(write ~ read, data = ed_ch_df,
     col = c("red", "blue", "green")[as.integer(prog)],
     pch = c(1, 2, 3)[as.integer(prog)])
cor(ed_ch_df$write, ed_ch_df$read)

plot(science ~ read, data = ed_ch_df,
     col = c("red", "blue", "green")[as.integer(prog)],
     pch = c(1, 2, 3)[as.integer(prog)])
cor(ed_ch_df$science, ed_ch_df$read)


ed_ch_df$prog <- as.factor(ed_ch_df$prog)

train_index <- createDataPartition(ed_ch_df$prog, p=0.7, list = FALSE, times = 1)
train_df <- ed_ch_df[train_index, ]
test_df <- ed_ch_df[-train_index, ]
str(test_df)

# 1 usekernel = FALSE, usepoisson = FALSE
NB <- naive_bayes(prog ~ ses + read + write + math + science + socst,
                  usekernel = FALSE, usepoisson = FALSE, data = train_df)

pred_NB <- predict(NB, test_df[, -4])

confusionMatrix(data = pred_NB, reference = test_df$prog)


# 1.1 usekernel = FALSE, usepoisson = FALSE
NB <- naive_bayes(prog ~ as.integer(female) + as.integer(schtyp) + as.integer(ses) + read + write + math + science + socst + as.integer(honors) + awards + cid,
                  usekernel = FALSE, usepoisson = FALSE, data = train_df)

pred_NB <- predict(NB, test_df[, -4])

confusionMatrix(data = pred_NB, reference = test_df$prog)


# 2 usekernel = TRUE, usepoisson = FALSE
NB_kern <- naive_bayes(prog ~ ses + read + write + math + science + socst,
                  usekernel = TRUE, usepoisson = FALSE, data = train_df)

pred_kern <- predict(NB_kern, test_df[, -4])

confusionMatrix(data = pred_kern, reference = test_df$prog)

# 2.1 usekernel = TRUE, usepoisson = FALSE
NB_kern <- naive_bayes(prog ~ as.integer(female) + as.integer(schtyp) + as.integer(ses) + read + write + math + science + socst + as.integer(honors) + awards + cid,
                       usekernel = TRUE, usepoisson = FALSE, data = train_df)

pred_kern <- predict(NB_kern, test_df[, -4])

confusionMatrix(data = pred_kern, reference = test_df$prog)

# 3 usekernel = FALSE, usepoisson = TRUE
NB_pois <- naive_bayes(prog ~ ses + read + write + math + science + socst,
                       usekernel = FALSE, usepoisson = TRUE, data = train_df)

pred_pois <- predict(NB_pois, test_df[, -4])

confusionMatrix(data = pred_pois, reference = test_df$prog)

# 3.1 usekernel = FALSE, usepoisson = TRUE
NB_pois <- naive_bayes(prog ~ as.integer(female) + as.integer(schtyp) + as.integer(ses) + read + write + math + science + socst + as.integer(honors) + awards + cid,
                       usekernel = FALSE, usepoisson = TRUE, data = train_df)

pred_pois <- predict(NB_pois, test_df[, -4])

confusionMatrix(data = pred_pois, reference = test_df$prog)


# 4 usekernel = TRUE, usepoisson = TRUE
NB_pois_kernel <- naive_bayes(prog ~ ses + read + write + math + science + socst,
                       usekernel = TRUE, usepoisson = TRUE, data = train_df)

pred_pois_kernel <- predict(NB_pois_kernel, test_df[, -4])

confusionMatrix(data = pred_pois_kernel, reference = test_df$prog)

# 4.1 usekernel = TRUE, usepoisson = TRUE
train_df$female=as.integer(train_df$female)
train_df$schtyp=as.integer(train_df$schtyp)
train_df$ses=as.integer(train_df$ses)
train_df$honors=as.integer(train_df$honors)


NB_pois_kernel <- naive_bayes(prog ~ female + schtyp + ses + read + write + math + science + socst + honors + awards + cid,
                              usekernel = TRUE, usepoisson = TRUE, data = train_df)

pred_pois_kernel <- predict(NB_pois_kernel, test_df[, -4])

confusionMatrix(data = pred_pois_kernel, reference = test_df$prog)

# 2 classes
levels(ed_ch_df$prog)[c(1,3)] <- "1"

train_index <- createDataPartition(ed_ch_df$prog, p=0.7, list = FALSE, times = 1)
train_df <- ed_ch_df[train_index, ]
test_df <- ed_ch_df[-train_index, ]

NB_pois_kernel <- naive_bayes(prog ~ ses + read + write + math + science + socst,
                              usekernel = TRUE, usepoisson = TRUE, data = train_df)

pred_pois_kernel <- predict(NB_pois_kernel, test_df[, -4])

confusionMatrix(data = pred_pois_kernel, reference = test_df$prog)







# 2.1 classes
levels(ed_ch_df$prog)[c(1,3)] <- "1"

train_index <- createDataPartition(ed_ch_df$prog, p=0.7, list = FALSE, times = 1)
train_df <- ed_ch_df[train_index, ]
test_df <- ed_ch_df[-train_index, ]

NB_pois_kernel <- naive_bayes(prog ~ female + schtyp + ses + read + write + math + science + socst + honors + awards + cid,
                              usekernel = TRUE, usepoisson = TRUE, data = train_df)

pred_pois_kernel <- predict(NB_pois_kernel, test_df[, -4])

confusionMatrix(data = pred_pois_kernel, reference = test_df$prog)










