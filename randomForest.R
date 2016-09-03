# Kaggle competition 'bike sharing demand'
setwd('/Users/snowleopard/Documents/workspace/kaggle/bikeSharingDemand')

library(ggplot2)
library(lubridate)
library(randomForest)

set.seed(1)

# read data
train <- read.csv('data/train.csv')
test <- read.csv('data/test.csv')

head(train)

extractFeatures <- function(data) {
  features <- c('season', 'holiday', 'workingday','weather', 'temp','atemp','humidity','windspeed','hour')
  data$hour <- hour(ymd_hms(data$datetime))
  return(data[,features])
}

submission <- data.frame(datetime = test$datetime, count=NA)

# Using only past data to make predictions
for (i_year in unique(year(ymd_hms(train$datetime)))) {
  for(i_month in unique(month(ymd_hms(train$datetime)))) {
    cat('Year:', i_year,'\tmonth:', i_month,'\n')
    testLocs <- year(ymd_hms(test$datetime))==i_year & month(ymd_hms(test$datetime))==i_month
    testSubset <- test[testLocs,]
    trainLocs  <- ymd_hms(train$datetime) <= min(ymd_hms(testSubset$datetime))
    rf <- randomForest(extractFeatures(train[trainLocs,]), train[trainLocs,"count"], ntree=100)
    submission[testLocs, "count"] <- predict(rf, extractFeatures(testSubset))
  }
}

write.csv(submission, file = "1_random_forest_submission.csv", row.names=FALSE)

# Train a model across all the training data and plot the variable importance
rf <- randomForest(extractFeatures(train), train$count, ntree=100, importance=TRUE)
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), Importance=imp[,1])

p <- ggplot(featureImportance, aes(x=reorder(Feature, Importance), y=Importance)) +
  geom_bar(stat="identity", fill="#53cfff") +
  coord_flip() + 
  theme_light(base_size=20) +
  xlab("Importance") +
  ylab("") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

ggsave("2_feature_importance.png", p)
