#!/usr/bin/env Rscript

setwd("~/Dropbox/R/Pizza")
library(jsonlite)
test <- fromJSON("test.json")
raop_data <- fromJSON("train.json")
pizza <- as.factor(raop_data$requester_received_pizza)
raop_data <- raop_data[names(test)]
raop_data$requester_received_pizza <- pizza

library(caret)
set.seed(1)
inTrain <- createDataPartition(raop_data$unix_timestamp_of_request, p=0.75, list=FALSE, times=1)
train <- raop_data[inTrain,]
test <- raop_data[-inTrain,]

nums <- sapply(test, is.numeric)
test_num <- test[, nums]
test_num$requester_received_pizza <- FALSE
train_num <- train[, nums]
train_num$requester_received_pizza <- train$requester_received_pizza

#my_logit <- glm(requester_received_pizza ~ ., data = train_num, family= "binomial")

##Logistic Regression
my_logit <- glm(requester_received_pizza ~
                requester_account_age_in_days_at_request +
                requester_days_since_first_post_on_raop_at_request +
                requester_number_of_comments_in_raop_at_request +
                requester_number_of_posts_on_raop_at_request,
                data = train_num, family= "binomial")

## summary(my_logit)
## confint(my_logit)
## anova(my_logit, test="Chisq")

prediction <- predict(my_logit, newdata = test_num, type = "response")
p <- prediction > 0.5
accuracy <- p == test_num$requester_received_pizza
sum(accuracy)
length(accuracy)

test <- fromJSON("test.json")
prediction <- predict(my_logit, newdata = test[, nums], type = "response")
submit <- data.frame(request_id = test$request_id)
submit$requester_received_pizza <-
  sapply(prediction, function(x) round(x))
write.csv(submit, file = "logit.csv", row.names = FALSE)

##Random Forests
library(randomForest)
my_rf <- randomForest(requester_received_pizza ~ .,
                data = train_num, method= "class")

prediction <- predict(my_rf, newdata = test_num, type = "response")
accuracy <- prediction == test_num$requester_received_pizza
sum(accuracy)
length(accuracy)

prediction <- predict(my_rf, newdata = test[, nums], type = "response")
submit$requester_received_pizza <-
  sapply(prediction, function(x) as.numeric(as.logical(x)))
write.csv(submit, file = "rf.csv", row.names = FALSE)

## par(mfrow = c(2, 2))
## for (i in 1:4)
##   plot(sort(my_rf$importance[,i], dec = TRUE),
##        type = "h", main = paste("Measure", i))
