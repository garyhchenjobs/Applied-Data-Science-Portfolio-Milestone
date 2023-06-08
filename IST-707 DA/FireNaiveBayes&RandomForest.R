# Fire Data Cleansing
# IST 707: Group 1

# Read in CSV File (Will be unique to user)
fires <- read.csv("/Users/richie/Downloads/FireData.csv", na.strings = c(""))
library(dplyr)
library (Hmisc)
library (psych)
library (SmartEDA)
library (skimr)
library (caret)
library (gbm)
library (mlbench)
library (readr)
library (gmodels)
library (vcd)
library (C50)
library (party)
library (Amelia)
library (lubridate)
library (imputeTS)
library (missForest)
library (nnet)
library (maps)
library (ggmap)
library (mapproj)
library (tidyverse)
library (kernlab)
library(naniar)
library(visdat)
library(e1071)
# View Structure
str(fires)

# Convert FOD_ID to factor
fires$FOD_ID <- factor(fires$FOD_ID)

# Convert DISCOVERY_DATE and CONT_DATE to date
fires$DISCOVERY_DATE <- as.Date(fires$DISCOVERY_DATE)
fires$CONT_DATE <- as.Date(fires$CONT_DATE)

# Calculate Resolution/Containment Time
fires$RESOLUTION_DAYS <- as.numeric(difftime(fires$CONT_DATE, fires$DISCOVERY_DATE, units = c("days")))
str(fires)

# Bin Fire Size
SIZE_BIN <- cut(fires$FIRE_SIZE, breaks = c(0,25,50,100,200,400,800,1600,3200,6400,Inf), labels = c("1-25","25-50","50-100","100-200","200-400","400-800","800-1600","1600-3200","3200-6400",">6400"))
fires$SIZE_BIN <- SIZE_BIN

# Bin Resolution Days
RESOLUTION_BIN <- cut(fires$RESOLUTION_DAYS, breaks = c(-1,0,7,14,28,56,112,224,448,896,Inf), labels = c("< 1","1-7","7-14","14-28","28-56","56-112","112-224","224-448","448-896",">896"))
fires$RESOLUTION_BIN <- RESOLUTION_BIN

# Bin Season
SEASON <- cut(fires$DISCOVERY_DOY, breaks = c(0,79,162,265,355,365), labels = c("Winter", "Spring", "Summer", "Fall", "Winter"))
fires$SEASON <- SEASON


fires %>% count(SIZE_BIN)
fires %>% count(RESOLUTION_BIN)

# Creating Predictive Models and seeing which one is the best.
# Naive Bayes Model
# First using the attributes I think that would be most useful
myVars = c('SEASON', 'STATE', 'NWCG_CAUSE_CLASSIFICATION', 'FIRE_YEAR','SIZE_BIN', 'RESOLUTION_DAYS', 'DISCOVERY_TIME', 'NWCG_GENERAL_CAUSE','SOURCE_REPORTING_UNIT_NAME')
DirtyNBFire = fires

DirtyNBFire = DirtyNBFire[myVars]
sum(!complete.cases(DirtyNBFire))

NBFire <- NBFire[complete.cases(NBFire), ]

set.seed(123)
#Output is Fire Class size so changing it to a factor
str(NBFire)
NBFire$NWCG_CAUSE_CLASSIFICATION = as.factor(NBFire$NWCG_CAUSE_CLASSIFICATION)
NBFire$STATE = as.factor(NBFire$STATE)
NBFire$SEASON = as.factor(NBFire$SEASON)
NBFire$SIZE_BIN = as.factor(NBFire$SIZE_BIN)
NBFire$FIRE_YEAR = as.factor(NBFire$FIRE_YEAR)
NBFire$FIRE_SIZE = as.factor(NBFire$FIRE_SIZE)
NBFire$DISCOVERY_TIME = as.factor(NBFire$DISCOVERY_TIME)
NBFire$RESOLUTION_DAYS = as.factor(NBFire$RESOLUTION_DAYS)
NBFire$NWCG_GENERAL_CAUSE = as.factor(NBFire$NWCG_GENERAL_CAUSE)
NBFire$SOURCE_REPORTING_UNIT_NAME = as.factor(NBFire$SOURCE_REPORTING_UNIT_NAME)


## Creating the Naive Bayes Model
#subsetting the training and testing data 
indxTrain <- createDataPartition(y = NBFire$SIZE_BIN,p = 0.75,list = FALSE)
NBFiretraining <- NBFire[indxTrain,]
NBFiretesting <- NBFire[-indxTrain,]

prop.table(table(NBFiretraining$SIZE_BIN)) * 100
prop.table(table(NBFiretesting$SIZE_BIN)) * 100

x = NBFiretraining[,-5]
y = NBFiretraining$SIZE_BIN

NB10 = train(x,y,'nb',trControl=trainControl(method='cv',number=10))

NB10

NB5 = train(x,y,'nb',trControl=trainControl(method='cv',number=5))

NB5

NB3 = train(x,y,'nb',trControl=trainControl(method='cv',number=3))

NB3

NBPredict10 <- predict(NB10,newdata = NBFiretesting ) 
table(NBPredict10)

NBPredict5 <- predict(NB5,newdata = NBFiretesting ) 
table(NBPredict5)

NBPredict3 <- predict(NB3,newdata = NBFiretesting ) 
table(NBPredict3)


confusionMatrix(NBPredict10, NBFiretesting$SIZE_BIN)
confusionMatrix(NBPredict5, NBFiretesting$SIZE_BIN)
confusionMatrix(NBPredict3, NBFiretesting$SIZE_BIN)

ModelPerformance <- varImp(NB3)
plot(ModelPerformance)

### Plotting
cm <- confusionMatrix(factor(NBPredict3), factor(NBFiretesting$SIZE_BIN), dnn = c("Prediction", "Reference"))

plt <- as.data.frame(cm$table)
plt$Prediction <- factor(plt$Prediction, levels=rev(levels(plt$Prediction)))

ggplot(plt, aes(Prediction,Reference, fill= Freq)) +
  geom_tile() + geom_text(aes(label=Freq)) +
  scale_fill_gradient(low="green", high="#009194") +
  labs(x = "Reference",y = "Prediction") +
  scale_x_discrete(labels=c(">6400","3200-6400","1600-3200","800-1600","400-800","200-400","100-200","50-100","25-50","1-25"))
scale_y_discrete(labels=c("1-25","25-50","50-100","100-200","200-400","400-800","800-1600","1600-3200","3200-6400",">6400")) 



### Running RF Model to compare to Naive Bayes Results

RFFiretesting <- NBFiretesting
RFFiretraining <- NBFiretraining


RFFiretesting$RESOLUTION_DAYS <- as.numeric(RFFiretesting$RESOLUTION_DAYS)
RFFiretesting$DISCOVERY_TIME <- as.numeric(RFFiretesting$DISCOVERY_TIME)
RFFiretraining$RESOLUTION_DAYS <- as.numeric(RFFiretraining$RESOLUTION_DAYS)
RFFiretraining$DISCOVERY_TIME <- as.numeric(RFFiretraining$DISCOVERY_TIME)

str(RFFiretesting)
str(RFFiretraining)

RFModel3 <- train(SIZE_BIN ~ . , 
               data = RFFiretraining[1:10000, ], # Use the train data frame as the training data
               method = 'rf',# Use the 'random forest' algorithm
               trControl = trainControl(method = 'cv', # Use cross-validation
                                        number = 3)) # Use 5 folds for cross-validation

RFModel5 <- train(SIZE_BIN ~ . , 
                     data = RFFiretraining[1:10000, ], # Use the train data frame as the training data
                     method = 'rf',# Use the 'random forest' algorithm
                     trControl = trainControl(method = 'cv', # Use cross-validation
                                              number = 3)) # Use 5 folds for cross-validation
               
fit_rf<-randomForest(SIZE_BIN~.,
                     data=RFFiretraining[1:10000, ],
                     )

PredRF <- predict(fit_rf, RFFiretesting)
PredRF

table(PredRF)









#SVM Model




