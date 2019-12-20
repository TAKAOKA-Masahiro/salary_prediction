
# 必要であれば、caretパッケージをインストール
install.packages("caret")
library(caret)
library(ggplot2)
library(dplyr)
library(readxl)

# データの読み込み
library(readr)
test_data_eng <- read_csv("test_data_eng.csv")
train_data_eng <- read_csv("train_data_eng.csv")


train_data <- read_excel("C:/Users/Takaoka/Downloads/train_data.xlsx", 
                         col_types = c("numeric", "numeric", "numeric", 
                                       "text", "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric", 
                                       "numeric", "numeric", "numeric"))
View(train_data)


test_data <- read_excel("C:/Users/Takaoka/Downloads/test_data.xlsx", 
                        col_types = c("numeric", "numeric", "numeric", 
                                      "text", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric"))
View(test_data)




#----------------------------------------------------------------------------------
# caretパッケージによる予測
#----------------------------------------------------------------------------------
head(train_ds)
head(test_ds)
nrow(train_ds)
nrow(test_ds)

# check null
sum(is.na(train_ds))
sum(is.na(test_ds))

# EDA about position
hist(train_ds$position, main="position", xlab="position", col="#993435")
# EDA about age
hist(train_ds$age, main="age", xlab="age", col="#993435")
# EDA about area
train_ds$area %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about sex
train_ds$sex %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about partner
train_ds$partner %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about num_child
train_ds$num_child %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about education
train_ds$education %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about service_length
hist(train_ds$service_length, main="service_length", xlab="service_length", col="#993435")
# EDA about study_time
hist(train_ds$study_time, main="study_time", xlab="study_time", col="#993435")
# EDA about commute
hist(train_ds$commute, main="commute", xlab="commute", col="#993435")
# EDA about overtime
hist(train_ds$overtime, main="overtime", xlab="overtime", col="#993435")
# EDA about salary
hist(train_ds$salary, main="salary", xlab="salary", col="#993435")


# "commute" と "salary" の散布図
plot(train_ds$commute,train_ds$salary)

test_train1 <- train_ds %>% filter((area == '東京都' | area == '大阪府') & partner == 0) 
head(test_train1)
plot(test_train1$commute,test_train1$salary)

test_train2 <- train_ds %>% filter(area != '東京都' & area != '大阪府' & partner == 0) 
head(test_train2)
plot(test_train2$commute,test_train2$salary)




# Model Building on eng data set
## Create test dataset and train dataset
# set.seed

head(test_data_eng)
head(train_data_eng)
nrow(test_data_eng)
nrow(train_data_eng)

# check null
sum(is.na(train_data_eng))
sum(is.na(test_data_eng))

# EDA about position
hist(train_data_eng$position, main="position", xlab="position", col="#993435")
# EDA about age
hist(train_data_eng$age, main="age", xlab="age", col="#993435")
# EDA about area
train_data_eng$area %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about sex
train_data_eng$sex %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about partner
train_data_eng$partner %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about num_child
train_data_eng$num_child %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about education
train_data_eng$education %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")
# EDA about service_length
hist(train_data_eng$service_length, main="service_length", xlab="service_length", col="#993435")
# EDA about study_time
hist(train_data_eng$study_time, main="study_time", xlab="study_time", col="#993435")
# EDA about commute
hist(train_data_eng$commute, main="commute", xlab="commute", col="#993435")
# EDA about overtime
hist(train_data_eng$overtime, main="overtime", xlab="overtime", col="#993435")
# EDA about salary
hist(train_data_eng$salary, main="salary", xlab="salary", col="#993435")


# "commute" と "salary" の散布図
plot(train_data_eng$commute,train_data_eng$salary)

test_train3 <- train_data_eng %>% filter((area == 13 | area == 27) & partner == 0) 
head(test_train3)
plot(test_train3$commute,test_train3$salary)

test_train4 <- train_data_eng %>% filter(area != 13 & area != 27 & partner == 0) 
head(test_train4)
plot(test_train4$commute,test_train4$salary)

# rangerパッケージを使う。
# 時間かかる！engデータ
set.seed(123)
modelRanger <- train(
  salary ~  . -id, 
  data = train_data_eng, 
  method = "ranger", 
  tuneLength = 4,
  preProcess = c('center', 'scale') ,
  trControl = trainControl(method = "cv")
)

predRanger_test <- predict(modelRanger, test_ds)
MAE(predRanger_test, test_ds$salary)
#MAE:21.46716

predRanger <- predict(modelRanger, test_data)
d <- data.frame(0:8999, predRanger)
colnames(d) <- c("id", "y")

write.csv(d, "submission4.csv")

# MAE 23.93438







# ランダムフォレストによる予測。もともとのデータ（日本語都道府県込み）
# randomForestパッケージを使う。
set.seed(123)
modelRF <- train(
  salary ~ . -id, 
  data = train_data, 
  method = "rf", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)


# rangerパッケージを使う。
# 時間かかる！
set.seed(123)
modelRanger <- train(
  salary ~  . -id, 
  data = train_ds, 
  method = "ranger", 
  tuneLength = 4,
  preProcess = c('center', 'scale')
#  ,
#  trControl = trainControl(method = "cv")
)

predRanger_test <- predict(modelRanger, test_ds)
MAE(predRanger_test, test_ds$salary)
#MAE:21.46716

predRanger <- predict(modelRanger, test_data)
d <- data.frame(0:8999, predRanger)
colnames(d) <- c("id", "y")

write.csv(d, "submission4.csv")

# MAE 23.93438



# Rboristパッケージを使う。
set.seed(0)
modelRborist <- train(
  salary ~ . -id, 
  data = train_data, 
  method = "Rborist", 
  tuneLength = 4,
  preProcess = c('center', 'scale'),
  trControl = trainControl(method = "cv")
)