#=================================================================
#load libraries 
#=================================================================
library(readr) 
library(mlbench)
library(caret)
library(tidyverse)
library(ggplot2)
library(doSNOW)
library(gbm)
library(C50)
library(fastDummies)
library(corrplot)
library(GGally)
library(psych)
library(reshape)
library(e1071)
library(rminer)
library(gtsummary)
library(lubridate)
library(reshape2)
library(superml)
library(dplyr)
library(ggmap)
library(doParallel)
library(rpart)
library(rpart.plot)
library(data.tree)
library(caTools)
library(plotly)
library(pROC)
library(mlbench)
#=================================================================
#load data 
#=================================================================
a <- file.choose()
df <-read.csv(a)
View(df)
names(df)
newdf <- df
#=================================================================
#Data Pre-processing
#=================================================================

#move the last 9 columns to first 
newdf <- newdf[,c(ncol(newdf), 1:(ncol(newdf)-1))]
View(newdf)
str(newdf) 
col<-c("LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID","TIMESTAMP")
summary(newdf[col])
str(newdf[col])
#The tbl_summary() function calculates descriptive statistics for continuous, categorical, and dichotomous 
#variables in R, and presents the results in a 
#beautiful, customizable summary table ready for publication (for example, Table 1 or demographic tables).
newdf %>% tbl_summary()
#observations from table summary 
# Gave median of Longitude and latitude 
# Floor - 0 is 22%, 1 is 25%, 2 is 22%, 3 is 25%, and 4 is 5.5% 
#BuildingID - 0 is 26%, 1 is 26%, and 2 is 48% 
#Relative position - 1 is 17% and 2 is 83% 
#100 means when a WAP is not detected. We see some of the columns have only 100. 

#converting unix timestamp
t<- as.Date(as.POSIXct(newdf$TIMESTAMP, origin="1970-01-01"))
newdf$TIMESTAMP<- as.Date(as.POSIXct(newdf$TIMESTAMP, origin="1970-01-01"))
## Create "datetime" attribute with lubridate
newdf$year <- year(newdf$TIMESTAMP)
newdf$month <- month(newdf$TIMESTAMP)
newdf$day <- day(newdf$TIMESTAMP)
newdf$hour <- hour(newdf$TIMESTAMP)
newdf$minute <- minute(newdf$TIMESTAMP)
newdf$week <- week(newdf$TIMESTAMP)
newdf$weekDay <- weekdays(newdf$TIMESTAMP)
## Move the attributes - run 7 times
newdf <- newdf[,c(ncol(newdf), 1:(ncol(newdf)-1))]
label <- LabelEncoder$new()
newdf$weekDay <- label$fit_transform(newdf$weekDay)

colnew<-c("month","day","week","weekDay","LONGITUDE","LATITUDE","FLOOR","BUILDINGID","SPACEID","RELATIVEPOSITION","USERID","PHONEID")
summary(newdf[colnew])
sum(is.na(newdf))
psych::describe(newdf)
#=================================================================
#correlation 
#=================================================================
corrData <- cor(newdf[colnew])
corrData
corrplot(corrData)
#observation 
#Week and month are postively correlated. WeekDay and day are negatively correlated and so is day and month. 
#longitude is positive wirh month,day, and week. 
#latitude is negative with month, day, and wee but strong with weeDay. 
#Floor is positive with month. 
#buildingid is positive month, day, and week. and very positive with longitude. 
#UserId is positive with day and longitude and buildingid.
#phoneid is positive with floor. 
#unique id is positive with floor 
#=================================================================
#EDA
#=================================================================
#plot 
ggplot(newdf,aes(newdf$LONGITUDE,newdf$LATITUDE)) +
  geom_point()
#observation of the wifi data in a day 
wifiday <- filter(newdf, day == 4 & between(month,5,6))
View(wifiday)
plot_ly(wifiday, x = ~wifiday$Unique, y = ~wifiday$FLOOR, type = 'scatter', mode = 'lines')

ggplot(wifiday,aes(wifiday$LONGITUDE,wifiday$LATITUDE)) +
  geom_point()

plot(newdf$UniqueID)
plot(newdf$BUILDINGID)
d <- melt(newdf, id.vars="UniqueID")

# Everything on the same plot
ggplot(d, aes(UniqueID,value, col=variable)) + 
  geom_point() + 
  stat_smooth() 

newdf[colnew2] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

Build01<- filter(newdf,BUILDINGID == 0)
plot(Build01$userid,Build01$PHONEID,xlim=c(0,20),ylim=c(0,20))


#=================================================================
#Pre-processing 
#=================================================================

newdf <- newdf[ -which(apply(newdf, 2, var) == 0 )] 

# removing all columns with only 100 
newdf <- newdf[, colSums(newdf != 100) > 0]

# removing all rows with only 100 
newdf <- newdf[rowSums(newdf[,1:465] != 100) > 0,]
View(newdf)


#combine floor/building id/space id /relative position
#newdf <-cbind(newdf,paste(newdf$FLOOR,newdf$BUILDINGID,newdf$SPACEID,newdf$RELATIVEPOSITION), stringsAsFactors=FALSE)
newdf <- unite(newdf, col = "UniqueID", c("BUILDINGID","FLOOR", "SPACEID", "RELATIVEPOSITION"), sep = "", remove = FALSE)
newdf$UniqueID <- factor(newdf$UniqueID)


#drop unwanted columns
# think we can drop time stamp,year,minute, hour and maybe phone id ?
#remove unwanted columns
newdf$TIMESTAMP <- NULL
newdf$year <- NULL
newdf$minute <- NULL
newdf$hour <- NULL
newdf$PHONEID <- NULL
newdf$USERID <- NULL
newdf$LONGITUDE <- NULL
newdf$LATITUDE <- NULL
newdf$week <- NULL
newdf$month <- NULL
newdf$day <- NULL
newdf$weekDay <- NULL
newdf$FLOOR <- NULL
newdf$SPACEID <- NULL
newdf$RELATIVEPOSITION <- NULL

#=================================================================
#split the data by building #
#=================================================================

#my kappa was all 0 so redoing by building subset 
#separate the data by building 
trainingb0 <- subset(newdf, BUILDINGID == 0)
trainingb1 <- subset(newdf, BUILDINGID == 1)
trainingb2 <- subset(newdf, BUILDINGID == 2)

trainingb0$BUILDINGID <- NULL
trainingb1$BUILDINGID <- NULL
trainingb2$BUILDINGID <- NULL

View(trainingb0)

trainingb0$UniqueID <- factor(trainingb0$UniqueID)
trainingb1$UniqueID <- factor(trainingb1$UniqueID)
trainingb2$UniqueID <- factor(trainingb2$UniqueID)

str(trainingb0$UniqueID)

trainingb0 <- trainingb0[ -which(apply(trainingb0, 2, var) == 0 )] 

#=================================================================
#split the data by building #  --- Building 0
#=================================================================

train_b0 <- createDataPartition(trainingb0$UniqueID, p = .70, list = FALSE)
wifib0.training <- trainingb0[train_b0,]
wifib0.testing <- trainingb0[-train_b0,]

set.seed(123)
controlb0 <- trainControl(method = "cv",
                              number = 10)

cl <- makeCluster(3)
# Register Cluster
registerDoParallel(cl)
set.seed(123)
#Decision Tree

tree_b0 <- train(UniqueID~., data = wifib0.training, method = "rpart", 
                trControl = controlb0)

treeb0.unique.predicted <- predict(tree_b0,wifib0.testing)
tree.b0<-confusionMatrix(treeb0.unique.predicted,wifib0.testing$UniqueID)
dcb0<- postResample(treeb0.unique.predicted, wifib0.testing$UniqueID)

#C5.0
C50_b0 <- train(UniqueID~., data = wifib0.training, method = "C5.0", 
                trControl = controlb0)
prediction_C50_b0 <- predict(C50_b0, wifib0.testing)
cm_C50_b0 <- confusionMatrix(prediction_C50_b0, wifib0.testing$UniqueID)

c5b0<-postResample(prediction_C50_b0, wifib0.testing$UniqueID)

#RF
rf_b0 <- train(UniqueID~., data = wifib0.training, method = "rf",
               trControl = controlb0)
preds_b0_rf <- predict(rf_b0, wifib0.testing)

rfb0<-postResample(preds_b0_rf, wifib0.testing$UniqueID)


#KNN
KNN_b0 <- train(UniqueID~., data = wifib0.training, method = "knn",
                trControl = controlb0)
prediction_KNN_b0 <- predict(KNN_b0, wifib0.testing)
knnb0<-postResample(prediction_KNN_b0, wifib0.testing$UniqueID)

stopCluster(cl)

resample_results <- resamples(list( CART=tree_b0, RF = rf_b0, C5.0=C50_b0, KNN = KNN_b0))
summary(resample_results)

X <- cbind(dcb0,c5b0,rfb0,knnb0)


#=================================================================
#split the data by building #  --- Building 1
#=================================================================
train_b1 <- createDataPartition(trainingb1$UniqueID, p = .70, list = FALSE)
wifib1.training <- trainingb1[train_b1,]
wifib1.testing <- trainingb1[-train_b1,]

set.seed(123)
controlb1 <- trainControl(method = "cv",
                          number = 10)

cl <- makeCluster(3)
# Register Cluster
registerDoParallel(cl)
set.seed(123)
#Decision Tree
tree_b1 <- train(UniqueID~., data = wifib1.training, method = "rpart", 
                 trControl = controlb1)
treeb1.unique.predicted <- predict(tree_b1,wifib1.testing,type='class')
tree.b1<-confusionMatrix(treeb1.unique.predicted,wifib1.testing$UniqueID)
dcb1<-postResample(treeb1.unique.predicted, wifib1.testing$UniqueID)

#C5.0
C50_b1 <- train(UniqueID~., data = wifib1.training, method = "C5.0", 
                trControl = controlb1)
prediction_C50_b1 <- predict(C50_b1, wifib1.testing)
cm_C50_b1 <- confusionMatrix(prediction_C50_b1, wifib1.testing$UniqueID)

c5b1<-postResample(prediction_C50_b1, wifib1.testing$UniqueID)

#RF
rf_b1 <- train(UniqueID~., data = wifib1.training, method = "rf",
               trControl = controlb1)
preds_b1_rf <- predict(rf_b1, wifib1.testing)

rfb1<-postResample(preds_b1_rf, wifib1.testing$UniqueID)


#KNN
KNN_b1 <- train(UniqueID~., data = wifib1.training, method = "knn",
                trControl = controlb1)
prediction_KNN_b1 <- predict(KNN_b1, wifib1.testing)
knnb1<-postResample(prediction_KNN_b1, wifib1.testing$UniqueID)

stopCluster(cl)

resample_results1 <- resamples(list(CART=tree_b1, RF = rf_b1, C5.0=C50_b1, KNN = KNN_b1))
summary(resample_results1)

y<- cbind(dcb1,c5b1,rfb1,knnb1)
#=================================================================
#split the data by building #  --- Building 2
#=================================================================

train_b2 <- createDataPartition(trainingb2$UniqueID, p = .70, list = FALSE)
wifib2.training <- trainingb2[train_b2,]
wifib2.testing <- trainingb2[-train_b2,]

set.seed(123)
controlb2 <- trainControl(method = "cv",
                          number = 10)

cl <- makeCluster(3)
# Register Cluster
registerDoParallel(cl)
set.seed(123)
#Decision Tree
tree_b2 <- train(UniqueID~., data = wifib2.training, method = "rpart", 
                 trControl = controlb2)
treeb2.unique.predicted <- predict(tree_b2,wifib2.testing)
tree.b2<-confusionMatrix(treeb2.unique.predicted,wifib2.testing$UniqueID)
dcb2<-postResample(treeb2.unique.predicted, wifib2.testing$UniqueID)

#C5.0
C50_b2 <- train(UniqueID~., data = wifib2.training, method = "C5.0", 
                trControl = controlb2)
prediction_C50_b2 <- predict(C50_b2, wifib2.testing)
cm_C50_b2 <- confusionMatrix(prediction_C50_b2, wifib2.testing$UniqueID)

c5b2<-postResample(prediction_C50_b2, wifib2.testing$UniqueID)

#RF
rf_b2 <- train(UniqueID~., data = wifib2.training, method = "rf",
               trControl = controlb2)
preds_b2_rf <- predict(rf_b2, wifib2.testing)

rfb2<-postResample(preds_b2_rf, wifib2.testing$UniqueID)


#KNN
KNN_b2 <- train(UniqueID~., data = wifib2.training, method = "knn",
                trControl = controlb2)
prediction_KNN_b2 <- predict(KNN_b2, wifib2.testing)
knnb2<-postResample(prediction_KNN_b2, wifib2.testing$UniqueID)

stopCluster(cl)

resample_results2 <- resamples(list(CART=tree_b2, RF = rf_b2, C5.0=C50_b2, KNN = KNN_b2))
summary(resample_results2)

z<- cbind(dcb2,c5b2,rfb2,knnb2)

par(mfrow=c(3,1))

barplot(X,
        main = "Accuracy/Kappa of Building 0",
        xlab = "Accuracy/Kappa",
        col = c("red","yellow"),beside = TRUE
)

barplot(y,
        main = "Accuracy/Kappa of Building 1",
        xlab = "Accuracy/Kappa",
        col = c("blue","yellow"),beside = TRUE
)
barplot(z,
        main = "Accuracy/Kappa of Building 2",
        xlab = "Accuracy/Kappa",
        col = c("green","yellow"),beside = TRUE
)
par(mfrow=c(1,1))