## ----message=FALSE, warning=FALSE----------------------------------------
# Install and load required packages
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
# if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(psych)) install.packages("psych", repos = "http://cran.us.r-project.org")
if(!require(RcppEigen)) install.packages("RcppEigen", repos = "http://cran.us.r-project.org")
if(!require(e1071)) install.packages("e1071", repos = "http://cran.us.r-project.org")
if(!require(ranger)) install.packages("ranger", repos = "http://cran.us.r-project.org")
library(caret)
library(ggplot2)
library(dplyr)
library(readr)
library(psych)
library(RcppEigen)
library(e1071)
library(ranger)


## ------------------------------------------------------------------------
# https://github.com/TAKAOKA-Masahiro/salary_prediction

# Data type is specified
test_data_eng <- read_csv("test_data_eng.csv", col_types=("iiiiiiiiiidd"))
train_data_eng <- read_csv("train_data_eng.csv", col_types=("iiiiiiiiiiddd"))


## ------------------------------------------------------------------------
# Check the first few lines
head(train_data_eng)
# Check the data structure
str(train_data_eng)
# Check basic statistics
summary(train_data_eng)


## ------------------------------------------------------------------------
# check null
sum(is.na(train_data_eng))
sum(is.na(test_data_eng))


## ------------------------------------------------------------------------
# Histogram for position
hist(train_data_eng$position, main="position", xlab="position", col="#993435")


## ------------------------------------------------------------------------
# Histogram for age
hist(train_data_eng$age, main="age", xlab="age", col="#993435")


## ------------------------------------------------------------------------
# Histogram for area
train_data_eng$area %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------------
# Histogram for sex
train_data_eng$sex %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------------
# Histogram for partner
train_data_eng$partner %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------------
# Histogram for num_child
train_data_eng$num_child %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")


## ------------------------------------------------------------------------
# Histogram for education
train_data_eng$education %>%
  table %>%
  as.data.frame() %>%
  ggplot(aes(x =  ., y = Freq, fill = .)) +
  geom_bar(stat = "identity")



## ------------------------------------------------------------------------
# Histogram for service_length
hist(train_data_eng$service_length, main="service_length", xlab="service_length", col="#993435")


## ------------------------------------------------------------------------
# Histogram for study_time
hist(train_data_eng$study_time, main="study_time", xlab="study_time", col="#993435")


## ------------------------------------------------------------------------
# Histogram for commute
hist(train_data_eng$commute, main="commute", xlab="commute", col="#993435")


## ------------------------------------------------------------------------
# Histogram for overtime
hist(train_data_eng$overtime, main="overtime", xlab="overtime", col="#993435")


## ------------------------------------------------------------------------
# Histogram for salary
hist(train_data_eng$salary, main="salary", xlab="salary", col="#993435")


## ------------------------------------------------------------------------
# Execute the following code to calculate the correlation matrix at once. However, due to the heavy processing, please run only high-performance PCs.

# psych::pairs.panels(train_data_eng[-1])



## ------------------------------------------------------------------------

# Scatter plot of "commute" and "salary"
plot(train_data_eng$commute,train_data_eng$salary)

# Scatter plot of "position" and "age"
plot(train_data_eng$position,train_data_eng$age)

# Scatter plot of "study_time" and "age"
plot(train_data_eng$study_time,train_data_eng$age)

# Scatter plot of "study_time" and "overtime"
plot(train_data_eng$study_time,train_data_eng$overtime)

# Scatter plot of "study_time" and "commute"
plot(train_data_eng$study_time,train_data_eng$commute)

# Scatter plot of "commute" and "overtime"
plot(train_data_eng$commute,train_data_eng$overtime)



## ------------------------------------------------------------------------
# Scatter plot of "commute" and "salary" color-coded by partner status
train_data_eng %>% ggplot(aes(y = salary, x = commute)) + geom_point(aes(colour=partner))


## ------------------------------------------------------------------------
# Scatter plot of "commute" and "salary" color-coded by partner status(urban resident only)
train_data_eng %>% filter(area == 13 | area == 27) %>% ggplot(aes(y = salary, x = commute)) + geom_point(aes(colour=partner))


## ------------------------------------------------------------------------
# Scatter plot of "commute" and "salary" color-coded by partner status(non-urban resident only)
train_data_eng %>% filter(area != 13 & area != 27) %>% ggplot(aes(y = salary, x = commute)) + geom_point(aes(colour=partner))


## ------------------------------------------------------------------------
# Create a new dataset with additional columns called segments based on area and partner status
train_data_eng_segment <- train_data_eng %>% mutate(.,segment = 
                                            case_when(
                                              area != 13 & area != 27 & partner == 0 ~ 1,
                                              area != 13 & area != 27 & partner == 1 ~ 2,
                                              (area == 13 | area == 27) & partner == 0 ~ 3,
                                              (area == 13 | area == 27) & partner == 1 ~ 4
                                            ))
# Check data
head(train_data_eng_segment)


## ------------------------------------------------------------------------
# Scatter plot of "commute" and "salary" color-coded by added segment
train_data_eng_segment %>% ggplot(aes(y = salary, x = commute)) + geom_point(aes(colour=segment))


## ------------------------------------------------------------------------
# Create a new dataset with additional columns called segments in the test set
test_data_eng_segment <- test_data_eng %>% mutate(.,segment = 
                                            case_when(
                                              area != 13 & area != 27 & partner == 0 ~ 1,
                                              area != 13 & area != 27 & partner == 1 ~ 2,
                                              (area == 13 | area == 27) & partner == 0 ~ 3,
                                              (area == 13 | area == 27) & partner == 1 ~ 4
                                            ))




## ------------------------------------------------------------------------
# Calculate the average
mu <- mean(train_data_eng$salary)
# Create a sumit file that stores the average value
a <- data.frame(id = 0:8999, y = rep(mu, 9000))
# Export to CSV
write.csv(a, "submission_mean.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- tibble(Model = "Model.1", Method="simple mean",
                                     MAE = 143.379)
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# Calculate the median
med <- median(train_data_eng$salary)
# Create a sumit file that stores the median value
b <- data.frame(id = 0:8999, y = rep(med, 9000))
# Export to CSV
write.csv(b, "submission_median.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.2", Method="simple median",
                                   MAE = 134.864 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# Add position effect
pos_effects <- train_data_eng %>% 
  group_by(position) %>% 
  summarize(pos_effect = mean(salary - med))
# Check the position effect
pos_effects


## ------------------------------------------------------------------------
# Predict salary
pred_salary <- test_data_eng %>% 
  left_join(pos_effects, by='position') %>%
  mutate(pred = med + pos_effect) 

# Check predicted values
head(pred_salary)

# Create a sumit file that stores the predicted values
d <- data.frame(0:8999, pred_salary$pred)
colnames(d) <- c("id", "y")

# Export to CSV
write.csv(d, "submission_pos.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.3", Method="position",
                                   MAE = 94.660 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# Add sex effect
sex_effects <- train_data_eng %>% 
  left_join(pos_effects, by='position') %>%
  group_by(sex) %>% 
  summarize(sex_effect = mean(salary - med - pos_effect))
# Check the sex effect
sex_effects


## ------------------------------------------------------------------------
# Predict salary
pred_salary <- test_data_eng %>% 
  left_join(pos_effects, by='position') %>%
  left_join(sex_effects, by='sex') %>%
  mutate(pred = med + pos_effect + sex_effect) 

# Check predicted values
head(pred_salary)

# Create a sumit file that stores the predicted values
d <- data.frame(0:8999, pred_salary$pred)
colnames(d) <- c("id", "y")

# Export to CSV
write.csv(d, "pos_sex.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.4", Method="pos & sex",
                                   MAE = 84.660 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# Add education effect
education_effects <- train_data_eng %>% 
  left_join(pos_effects, by='position') %>%
  left_join(sex_effects, by='sex') %>%
  group_by(education) %>% 
  summarize(education_effect = mean(salary - med - pos_effect - sex_effect))
# Check the education effect
education_effects


## ------------------------------------------------------------------------
# Predict salary
pred_salary <- test_data_eng %>% 
  left_join(pos_effects, by='position') %>%
  left_join(sex_effects, by='sex') %>%
  left_join(education_effects, by='education') %>%
  mutate(pred = med + pos_effect + sex_effect + education_effect) 

# Check predicted values
head(pred_salary)

# Create a sumit file that stores the predicted values
d <- data.frame(0:8999, pred_salary$pred)
colnames(d) <- c("id", "y")

# Export to CSV
write.csv(d, "pos_sex_edu.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.5", Method="pos & sex & edu",
                                   MAE = 74.660 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# Add segment effect
segment_effects <- train_data_eng_segment %>% 
  left_join(pos_effects, by='position') %>%
  left_join(sex_effects, by='sex') %>%
  left_join(education_effects, by='education') %>%
  group_by(segment) %>% 
  summarize(segment_effect = mean(salary - med - pos_effect - sex_effect - education_effect))
# Check the segment effect
segment_effects


## ------------------------------------------------------------------------
# Predict salary
pred_salary <- test_data_eng_segment %>% 
  left_join(pos_effects, by='position') %>%
  left_join(sex_effects, by='sex') %>%
  left_join(education_effects, by='education') %>%
  left_join(segment_effects, by='segment') %>%
  mutate(pred = med + pos_effect + sex_effect + education_effect + segment_effect) 

# Check predicted values
head(pred_salary)

# Create a sumit file that stores the predicted values
d <- data.frame(0:8999, pred_salary$pred)
colnames(d) <- c("id", "y")

# Export to CSV
write.csv(d, "pos_sex_edu_seg.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.6", Method="pos & sex & edu & seg",
                                   MAE = 64.660 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# ranger model
set.seed(123)
modelRanger <- train(
  salary ~  . -id, # not include ID in model
  data = train_data_eng, # There is no segment column
  method = "ranger", # Specifying the method to use
  tuneLength = 4, # Specify the range of parameter tuning
  preProcess = c('center', 'scale'), # Preprocessing to normalize data
  trControl = trainControl(method = "cv") # Use cross validation
)

# Create a sumit file that stores the predicted values
predRanger <- predict(modelRanger, test_data_eng)
d <- data.frame(0:8999, predRanger)
colnames(d) <- c("id", "y")
# Export to CSV
write.csv(d, "ranger.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.7", Method="ranger",
                                   MAE = 24.660 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# ranger with segment model
set.seed(123)
modelRanger <- train(
  salary ~  . -id, # not include ID in model
  data = train_data_eng_segment, # Use new dataset with added segment columns
  method = "ranger", # Specifying the method to use
  tuneLength = 4, # Specify the range of parameter tuning
  preProcess = c('center', 'scale'), # Preprocessing to normalize data
  trControl = trainControl(method = "cv") # Use cross validation
)
# Create a sumit file that stores the predicted values
predRanger <- predict(modelRanger, test_data_eng_segment)
d <- data.frame(0:8999, predRanger)
colnames(d) <- c("id", "y")
# Export to CSV
write.csv(d, "ranger_seg.csv", row.names=FALSE)


## ------------------------------------------------------------------------
# Store and display results in a list
mae_table <- bind_rows(mae_table,
                        tibble(Model = "Model.8", Method="ranger with segment",
                                   MAE = 23.614 ))
mae_table %>% knitr::kable()


## ------------------------------------------------------------------------
# rf model
# Use an if statement to put a comment out state
if(0){
set.seed(123)
modelRF <- train(
  salary ~ . -id, # not include ID in model
  data = train_data_eng, 
  method = "rf", # Specifying the method to use
  tuneLength = 4, # Specify the range of parameter tuning
  preProcess = c('center', 'scale'), # Preprocessing to normalize data
  trControl = trainControl(method = "cv") # Use cross validation
)
# Create a sumit file that stores the predicted values
predRF <- predict(modelRF, test_data_eng)
d <- data.frame(0:8999, predRF)
colnames(d) <- c("id", "y")
# Export to CSV
write.csv(d, "rf.csv", row.names=FALSE)
}


## ------------------------------------------------------------------------
# Rborist model
# Use an if statement to put a comment out state
if(0){
set.seed(123)
modelRborist <- train(
  salary ~ . -id, # not include ID in model
  data = train_data_eng, 
  method = "Rborist", # Specifying the method to use
  tuneLength = 4, # Specify the range of parameter tuning
  preProcess = c('center', 'scale'), # Preprocessing to normalize data
  trControl = trainControl(method = "cv") # Use cross validation
)
# Create a sumit file that stores the predicted values
predRborist <- predict(modelRborist, test_data_eng)
d <- data.frame(0:8999, predRborist)
colnames(d) <- c("id", "y")
# Export to CSV
write.csv(d, "Rborist.csv", row.names=FALSE)
}

