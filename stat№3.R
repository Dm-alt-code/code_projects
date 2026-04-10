library(caret)
library(e1071)
library(ggplot2)
library(openxlsx)
library(dplyr)


#1 prostate
data_prostate <- read.csv("D:\\5 курс\\Статистичне навчання\\Лаб№3\\Prostate_Cancer.csv")
data_prostate <- data_prostate[-1]

# Стандартизуємо
data_prostate$radius <- (data_prostate$radius-min(data_prostate$radius))/(max(data_prostate$radius)-min(data_prostate$radius))
data_prostate$texture <- (data_prostate$texture-min(data_prostate$texture))/(max(data_prostate$texture)-min(data_prostate$texture))
data_prostate$perimeter <- (data_prostate$perimeter-min(data_prostate$perimeter))/(max(data_prostate$perimeter)-min(data_prostate$perimeter))
data_prostate$area <- (data_prostate$area-min(data_prostate$area))/(max(data_prostate$area)-min(data_prostate$area))

data_prostate$diagnosis_result <- as.factor(data_prostate$diagnosis_result)


data_prostate

sample <- createDataPartition(data_prostate$diagnosis_result, p=0.80,
                              list=FALSE)
## train data
prostate_train <- data_prostate[sample,]
str(prostate_train)

# Create test data
prostate_test <- data_prostate[-sample,]
str(prostate_test)

k_grid <- expand.grid(k= 1:13)

#### Set caret params
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

#### Тренируем KNN – т.е. определяем оптим. k
fit.knn <- train(diagnosis_result~., data=prostate_train, method='knn',
                 trControl=control, metric=metric, tuneGrid = k_grid)
fit.knn$results


fit.knn$bestTune #### найкращі параметри

prostate_prediction <- predict(fit.knn, prostate_test)
confusionMatrix(prostate_prediction, prostate_test$diagnosis_result)

tab <- table(prostate_prediction, prostate_test$diagnosis_result,
      dnn=c("Predicted","True"))

sum(tab[row(tab) == col(tab)])/sum(tab) #accuracy
tab[1,1]/sum(tab[,1]) # 1 sensitivity
tab[2,2]/sum(tab[,2]) # 2 sensitivity

tab[1,1]/(tab[1,1]+sum(tab[1,-1])) # 1 positive predicted value
tab[2,2]/(tab[2,2]+sum(tab[2,-2])) # 2 positive predicted value


#2 wine
data_wine <- read.csv("D:\\5 курс\\Статистичне навчання\\Лаб№3\\wine_qual_white.csv",
                      header = TRUE, sep = ';')

wine_coln <- colnames(data_wine)
wine_coln_1 <- gsub(".", "_", wine_coln, fixed = TRUE)
### Замінили точки на символи підкреслення
colnames(data_wine) <- wine_coln_1

data_copy <- data_wine

# Стандартизуємо
data_wine$fixed_acidity <- (data_wine$fixed_acidity-min(data_wine$fixed_acidity))/(max(data_wine$fixed_acidity)-min(data_wine$fixed_acidity))
data_wine$volatile_acidity <- (data_wine$volatile_acidity-min(data_wine$volatile_acidity))/(max(data_wine$volatile_acidity)-min(data_wine$volatile_acidity))
data_wine$citric_acid <- (data_wine$citric_acid-min(data_wine$citric_acid))/(max(data_wine$citric_acid)-min(data_wine$citric_acid))
data_wine$residual_sugar <- (data_wine$residual_sugar-min(data_wine$residual_sugar))/(max(data_wine$residual_sugar)-min(data_wine$residual_sugar))
data_wine$chlorides <- (data_wine$chlorides-min(data_wine$chlorides))/(max(data_wine$chlorides)-min(data_wine$chlorides))
data_wine$free_sulfur_dioxide <- (data_wine$free_sulfur_dioxide-min(data_wine$free_sulfur_dioxide))/(max(data_wine$free_sulfur_dioxide)-min(data_wine$free_sulfur_dioxide))
data_wine$total_sulfur_dioxide <- (data_wine$total_sulfur_dioxide-min(data_wine$total_sulfur_dioxide))/(max(data_wine$total_sulfur_dioxide)-min(data_wine$total_sulfur_dioxide))
data_wine$density <- (data_wine$density-min(data_wine$density))/(max(data_wine$density)-min(data_wine$density))
data_wine$pH <- (data_wine$pH-min(data_wine$pH))/(max(data_wine$pH)-min(data_wine$pH))
data_wine$sulphates <- (data_wine$sulphates-min(data_wine$sulphates))/(max(data_wine$sulphates)-min(data_wine$sulphates))
data_wine$alcohol <- (data_wine$alcohol-min(data_wine$alcohol))/(max(data_wine$alcohol)-min(data_wine$alcohol))

data_wine$quality <- as.factor(data_wine$quality)

sample <- createDataPartition(data_wine$quality, p=0.80,
                              list=FALSE)
## train data
wine_train <- data_wine[sample,]
str(wine_train)

# Create test data
wine_test <- data_wine[-sample,]
str(wine_test)

k_grid <- expand.grid(k= 1:13)

#### Set caret params
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

#### Тренируем KNN – т.е. определяем оптим. k
fit.knn <- train(quality~., data=wine_train, method='knn',
                 trControl=control, metric=metric, tuneGrid = k_grid)

fit.knn$results

fit.knn$bestTune #### найкращі параметри


wine_prediction <- predict(fit.knn, wine_test)
confusionMatrix(wine_prediction, wine_test$quality)

table(wine_prediction, wine_test$quality,
      dnn=c("Predicted","True"))

# Об'єднюємо в 3 класи
data_wine <- data_copy


data_wine["quality"][data_wine["quality"] == 3] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 4] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 5] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 6] <- "cl_6"
data_wine["quality"][data_wine["quality"] == 7] <- "cl_7_9"
data_wine["quality"][data_wine["quality"] == 8] <- "cl_7_9"
data_wine["quality"][data_wine["quality"] == 9] <- "cl_7_9"

# Стандартизуємо
data_help <- subset(data_wine, select = -c(quality)) 
datacenter <- scale(data_help, center = TRUE, scale = TRUE) 
datacenter <- as.data.frame(datacenter) 
data_wine <- cbind(data_wine$quality, datacenter) 
data_wine <- as.data.frame(data_wine) 
#View(data_wine) 
names(data_wine)[names(data_wine) == "data_wine$quality"] <- "quality"
#View(data_wine) 
sample <- createDataPartition(data_wine$quality, p=0.80, list = FALSE) 
data_wine <- data_wine %>% mutate(quality = as.factor(quality)) 
data_wine <- mutate_if(data_wine, is.character, as.factor)


## train data
wine_train_3 <- data_wine[sample,]
str(wine_train_3)

# Create test data
wine_test_3 <- data_wine[-sample,]
str(wine_test_3)

#### Set caret params
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

#### Тренируем KNN – т.е. определяем оптим. k
fit.knn <- train(quality~., data=wine_train_3, method='knn',
                 trControl=control, metric=metric)

fit.knn$bestTune #### найкращі параметри


wine_prediction <- predict(fit.knn, wine_test_3)
confusionMatrix(wine_prediction, wine_test_3$quality)

table(wine_prediction, wine_test_3$quality,
      dnn=c("Predicted","True"))



# Об'єднюємо в 2 класи
data_wine <- data_copy

data_wine["quality"][data_wine["quality"] == 3] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 4] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 5] <- "cl_3_5"
data_wine["quality"][data_wine["quality"] == 6] <- "cl_6_9"
data_wine["quality"][data_wine["quality"] == 7] <- "cl_6_9"
data_wine["quality"][data_wine["quality"] == 8] <- "cl_6_9"
data_wine["quality"][data_wine["quality"] == 9] <- "cl_6_9"

# Стандартизуємо
data_help <- subset(data_wine, select = -c(quality)) 
datacenter <- scale(data_help, center = TRUE, scale = TRUE) 
datacenter <- as.data.frame(datacenter) 
data_wine <- cbind(data_wine$quality, datacenter) 
data_wine <- as.data.frame(data_wine) 
#View(data_wine) 
names(data_wine)[names(data_wine) == "data_wine$quality"] <- "quality"
#View(data_wine) 
sample <- createDataPartition(data_wine$quality, p=0.80, list = FALSE) 
data_wine <- data_wine %>% mutate(quality = as.factor(quality)) 
data_wine <- mutate_if(data_wine, is.character, as.factor)


## train data
wine_train_3 <- data_wine[sample,]
str(wine_train_3)

# Create test data
wine_test_3 <- data_wine[-sample,]
str(wine_test_3)

#### Set caret params
control <- trainControl(method='cv', number=10)
metric <- 'Accuracy'

#### Тренируем KNN – т.е. определяем оптим. k
fit.knn <- train(quality~., data=wine_train_3, method='knn',
                 trControl=control, metric=metric)

fit.knn$bestTune #### найкращі параметри


wine_prediction <- predict(fit.knn, wine_test_3)
confusionMatrix(wine_prediction, wine_test_3$quality)

tab <- table(wine_prediction, wine_test_3$quality,
      dnn=c("Predicted","True"))

sum(tab[row(tab) == col(tab)])/sum(tab)
tab[1,1]/sum(tab[,1])
tab[2,2]/sum(tab[,2])

tab[1,1]/(tab[1,1]+sum(tab[1,-1]))
tab[2,2]/(tab[2,2]+sum(tab[2,-2]))



