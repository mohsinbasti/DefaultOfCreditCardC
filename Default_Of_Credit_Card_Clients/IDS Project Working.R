# Packages Installation

install.packages("dplyr")
install.packages("ggplot2")
install.packages("plyr")
install.packages("tidyr")
install.packages("DataExplorer")
install.packages("corrplot")
install.packages("randomForest")
install.packages("tidyverse")
install.packages('caTools')
install.packages('e1071')
install.packages("nnet")
install.packages("neuralnet")
install.packages("GGally")
install.packages("caret")
install.packages("Hmisc")
install.packages("ggpubr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("naivebayes")
install.packages("class")
install.packages("clusterGeneration")

# Libraries

library(naivebayes)
library(dplyr)
library(ggplot2)
library(plyr)
library(tidyr)
library(corrplot)
library(randomForest)
library(tidyverse)
library(caTools)
library(e1071)
library(nnet)
library(neuralnet)
library(GGally)
library(caret)
library(Hmisc)
library(ggpubr)
library(rpart)
library(rpart.plot)
library(naniar)
library(class)
library(clusterGeneration)


# STEP 1: Initial Exploratory Analysis

library(readxl)
myData <- read_excel("~/Desktop/default of credit card clients.xls")
myData
View(myData)
head(myData)
tail(myData)
class(myData)
dim(myData)
summary(myData)
names(myData)
str(myData)
glimpse(myData)
unique(myData$PAY_6)
summary(myData$PAY_AMT6)

# Transformation on Dataset:

colnames(myData)[colnames(myData) == "PAY_0"] = "PAY_1"
colnames(myData)[colnames((myData)) == "default.payment.next.month"] = "default_payment_next_month"
myData$SEX = ifelse(myData$SEX == 1, "Male", "Female")
myData$default_payment_next_month = as.factor(myData$default_payment_next_month)
myData$SEX = as.factor(myData$SEX)
table(myData$default_payment_next_month)
myData$MARRIAGE = ifelse(myData$MARRIAGE == 3, 0, myData$MARRIAGE)

# STEP 2: Visual Exploratory Analysis

library(DataExplorer)
DataExplorer::create_report(myData)
plot(myData)

coorelation <- cor(myData)
corrplot(coorelation, method = "circle")

hist(myData$LIMIT_BAL,
     main="Limit of Balance Amount",
     xlab="Balance Limit",
     col="darkmagenta",
     freq=FALSE
)
hist(myData$PAY_0,
     main="Amount of Pay 0",
     xlab="Pay_0",
     col="pink",
     freq=FALSE
)
hist(myData$PAY_2,
     main="LAmount of Pay 2",
     xlab="Pay_2",
     col="red",
     freq=FALSE
)
hist(myData$PAY_3,
     main="Amount of Pay 3",
     xlab="Pay_3",
     col="yellow",
     freq=FALSE
)
hist(myData$PAY_4,
     main="Amount of Pay 4",
     xlab="Pay_4",
     col="violet",
     freq=FALSE
)
hist(myData$PAY_5,
     main="Amount of Pay 5",
     xlab="Pay_5",
     col="green",
     freq=FALSE
)
hist(myData$PAY_6,
     main="Amount of Pay 6",
     xlab="Pay_6",
     col="blue",
     freq=FALSE
)
hist(myData$BILL_AMT1,
     main="Amount of Bill 1",
     xlab="BILL_AMT1",
     col="cyan",
     freq=FALSE
)
hist(myData$BILL_AMT2,
     main="Amount of Bill 2",
     xlab="BILL_AMT2",
     col="darkmagenta",
     freq=FALSE
)
hist(myData$BILL_AMT3,
     main="Amount of Bill 3",
     xlab="BILL_AMT3",
     col="pink",
     freq=FALSE
)
hist(myData$BILL_AMT4,
     main="Amount of Bill 4",
     xlab="BILL_AMT4",
     col="red",
     freq=FALSE
)
hist(myData$BILL_AMT5,
     main="Amount of Bill 5",
     xlab="BILL_AMT5",
     col="yellow",
     freq=FALSE
)
hist(myData$BILL_AMT6,
     main="Amount of Bill 6",
     xlab="BILL_AMT6",
     col="violet",
     freq=FALSE
)
hist(myData$PAY_AMT1,
     main="Amount of Pay 1",
     xlab="PAY_AMT1",
     col="blue",
     freq=FALSE
)
hist(myData$PAY_AMT2,
     main="Amount of Pay 2",
     xlab="PAY_AMT2",
     col="cyan",
     freq=FALSE
)
hist(myData$PAY_AMT3,
     main="Amount of Pay 3",
     xlab="PAY_AMT3",
     col="brown",
     freq=FALSE
)
hist(myData$PAY_AMT4,
     main="Amount of Pay 4",
     xlab="PAY_AMT4",
     col="orange",
     freq=FALSE
)
hist(myData$PAY_AMT5,
     main="Amount of Pay 5",
     xlab="PAY_AMT5",
     col="black",
     freq=FALSE
)
hist(myData$PAY_AMT6,
     main="Amount of Pay 6",
     xlab="PAY_AMT6",
     col="darkmagenta",
     freq=FALSE
)
hist(myData$default_payment_next_month,
     main="Default of Payment Next Month",
     xlab="default_payment_next_month",
     col="green",
     freq=FALSE
)

ggscatter(myData, x = "LIMIT_BAL", y = "BILL_AMT1",
          add = "reg.line", conf.int = TRUE ,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Balance Limit", ylab = "Amount of bill 1")

ggscatter(myData, x = "LIMIT_BAL", y = "BILL_AMT2",
          add = "reg.line", conf.int = TRUE ,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Balance Limit", ylab = "Amount of bill 2")

ggscatter(myData, x = "LIMIT_BAL", y = "PAY_AMT1",
          add = "reg.line", conf.int = TRUE ,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Balance Limit", ylab = "Amount of Pay 1")
ggscatter(myData, x = "LIMIT_BAL", y = "BILL_AMT6",
          add = "reg.line", conf.int = TRUE ,
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Balance Limit", ylab = "Amount of bill 6")

ggplot(myData, aes(x=default_payment_next_month, y=LIMIT_BAL)) + 
  geom_boxplot(color="black", fill="green", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=EDUCATION)) + 
  geom_boxplot(color="black", fill="purple", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=MARRIAGE)) + 
  geom_boxplot(color="black", fill="orange", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=AGE)) + 
  geom_boxplot(color="black", fill="pink", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT1)) + 
  geom_boxplot(color="black", fill="orange", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT2)) + 
  geom_boxplot(color="black", fill="purple", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT3)) + 
  geom_boxplot(color="black", fill="pink", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT4)) + 
  geom_boxplot(color="black", fill="red", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT5)) + 
  geom_boxplot(color="black", fill="yellow", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=BILL_AMT6)) + 
  geom_boxplot(color="black", fill="brown", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT1)) + 
  geom_boxplot(color="black", fill="black", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT2)) + 
  geom_boxplot(color="black", fill="green", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT3)) + 
  geom_boxplot(color="black", fill="purple", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT4)) + 
  geom_boxplot(color="black", fill="red", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT5)) + 
  geom_boxplot(color="black", fill="pink", alpha=0.9)
ggplot(myData, aes(x=default_payment_next_month, y=PAY_AMT6)) + 
  geom_boxplot(color="black", fill="brown", alpha=0.9)

ggplot(data = myData, mapping = aes(x = default_payment_next_month, fill = default_payment_next_month)) +
  geom_bar() +
  xlab("IsDefault") +
  ggtitle(" Defaulters VS non-Defaulters") +
  stat_count(aes(label = ..count..), geom = "label")
ggplot(data = myData, mapping = aes(x = SEX, fill = default_payment_next_month)) +
  geom_bar() +
  xlab("SEX") +
  ggtitle("Gender Wise Classification") +
  stat_count(aes(label = ..count..), geom = "label")
ggplot(data = myData, mapping = aes(x = MARRIAGE, fill = default_payment_next_month)) +
  geom_bar() +
  xlab("Marriage") +
  ggtitle("Merital Status Wise Classification") +
  stat_count(aes(label = ..count..), geom = "label")
ggplot(myData,aes(x=LIMIT_BAL)) + 
  theme_bw()+
  geom_histogram(data=subset(myData,default_payment_next_month == "0"),fill = "red", alpha = 0.6) +
  geom_histogram(data=subset(myData,default_payment_next_month == "1"),fill = "blue", alpha = 0.6)
ggplot(myData, aes(x=LIMIT_BAL, y=AGE, color=LIMIT_BAL)) + 
  geom_point(size=3)
ggplot(myData, aes(x=LIMIT_BAL, y=EDUCATION, color=LIMIT_BAL)) + 
  geom_point(size=3)
ggplot(myData, aes(x=LIMIT_BAL, y=MARRIAGE, color=LIMIT_BAL)) + 
  geom_point(size=3)
ggplot(myData, aes(x=LIMIT_BAL, y=SEX, color=LIMIT_BAL)) + 
  geom_point(size=3)
ggplot(data = myData, aes(x = default_payment_next_month,y=AGE)) +
  geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Age")+xlab("Non-defaulters and Defaulters")
ggplot(data = myData, aes(x = default_payment_next_month,y=EDUCATION)) +
  geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Educatiom")+xlab("Non-defaulters and Defaulters")
ggplot(data = myData, aes(x = default_payment_next_month,y=MARRIAGE)) +
  geom_violin(aes(fill =default_payment_next_month ))+ ggtitle("Default and Non_Default related to Marriage")+xlab("Non-defaulters and Defaulters")

# Extracting desired Attributes

Desired <- myData %>%
  select(LIMIT_BAL,SEX,EDUCATION,MARRIAGE,AGE,PAY_1,PAY_2,PAY_3,PAY_4,PAY_5,PAY_6,BILL_AMT1,
         BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,BILL_AMT6,PAY_AMT1,
         PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)
Desired

# Finding NA in Dataset

FindingNA <- is.na(myData)
FindingNA
yes <- length(FindingNA[FindingNA== TRUE])
which(yes)
checkingDuplicate <- duplicated(myData)
checkingDuplicate
length(checkingDuplicate[checkingDuplicate== TRUE])
vis_miss(myData)

# Creation Classifier:

myDataNew <- transform(myData,
                              ID = as.integer(ID),
                              LIMIT_BAL = as.integer(LIMIT_BAL),
                              SEX = as.integer(SEX),
                              EDUCATION = as.integer(EDUCATION),
                              MARRIAGE = as.integer(MARRIAGE),
                              AGE = as.integer(AGE),
                              PAY_0 = as.integer(PAY_0),
                              PAY_2 = as.integer(PAY_2),
                              PAY_3 = as.integer(PAY_3),
                              PAY_4 = as.integer(PAY_4),
                              PAY_5 = as.integer(PAY_5),
                              PAY_6 = as.integer(PAY_6),
                              BILL_AMT1 = as.integer(BILL_AMT1),
                              BILL_AMT2 = as.integer(BILL_AMT2),
                              BILL_AMT3 = as.integer(BILL_AMT3),
                              BILL_AMT4 = as.integer(BILL_AMT4),
                              BILL_AMT5 = as.integer(BILL_AMT5),
                              BILL_AMT6 = as.integer(BILL_AMT6),
                              PAY_AMT1 = as.integer(PAY_AMT1),
                              PAY_AMT2 = as.integer(PAY_AMT2),
                              PAY_AMT3 = as.integer(PAY_AMT3),
                              PAY_AMT4 = as.integer(PAY_AMT4),
                              PAY_AMT5 = as.integer(PAY_AMT5),
                              PAY_AMT6 = as.integer(PAY_AMT6),
                              default_payment_next_month = as.factor(default_payment_next_month)
                              )
myDataNew

# SVM Classifier

set.seed(123)
split = sample.split(myData$default_payment_next_month, SplitRatio = 0.75)
split
training_set = subset(myData, split == TRUE)
View(training_set)
test_set = subset(myData, split == FALSE)
View(test_set)
training_set[-25] = scale(training_set[-25]) 
View(training_set)
test_set[-25] = scale(test_set[-25]) 
View(test_set)
SVMclassifier = svm(formula = default_payment_next_month ~ ., 
                 data = training_set, 
                 type = 'C-classification', 
                 kernel = 'linear') 
SVMclassifier
prediction = predict(SVMclassifier, newdata = test_set[-25])
prediction
g <- table(test_set$default_payment_next_month, prediction)
cm <- confusionMatrix(g)
cm
xm1 <- cm$overall[1]
xm1
predictionh = predict(SVMclassifier, newdata = newPdata1)
predictionh[[1]]


# Neural Network Classification:

set.seed(123)
splitp = sample.split(myData$default_payment_next_month, SplitRatio = 0.80)
splitp
training_setp = subset(myData, splitp == TRUE)
View(training_setp)
test_setp = subset(myData, splitp == FALSE)
View(test_setp)
training_setp[-25] = scale(training_setp[-25]) 
View(training_setp)
test_setp[-25] = scale(test_setp[-25]) 
View(test_setp)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
maxmindataset <- as.data.frame(lapply(myData[1:24], normalize))
# Training and Test Data for Model_1:

model_1 <- neuralnet(default_payment_next_month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_3
                 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + 
                   BILL_AMT3 + BILL_AMT4 + BILL_AMT5 + BILL_AMT6 + PAY_AMT1 + 
                   PAY_AMT2 + PAY_AMT3 + PAY_AMT4 + PAY_AMT5 + PAY_AMT6, data=training_setp, hidden=1, threshold=0.01)
plot(model_1)

#Testing the Accuracy of Model_1:

model1.results <- compute(model_1, test_setp)
results <- data.frame(actual = test_setp$default_payment_next_month, prediction = model1.results$net.result)
results
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
gx <- table(actual,prediction)
gx
cm4 <- confusionMatrix(gx)
cm4
xm5 <- cm4$overall[1]
xm5


##########################################################################################################

# Decision Tree:

tree <- rpart(default_payment_next_month~.,data = trainset,
              method = "class")
tree
rpart.plot(tree, nn = TRUE)
pred <- predict(object = tree, testset[-25], type = "class")
pred
t <- table(testset$default_payment_next_month,pred)
t
cm1 <- confusionMatrix(t)
cm1
xm2 <- cm1$overall[1]
xm2


# Naive Bayes Model:


set.seed(123)
split1 = sample.split(myData$default_payment_next_month, SplitRatio = 0.75)
split1
training_set1 = subset(myData, split == TRUE)
training_set1 <- transform(training_set1,
                    default_payment_next_month = as.factor(default_payment_next_month)
)
View(training_set1)
class(training_set1$default_payment_next_month)
test_set1 = subset(myData, split == FALSE)
test_set1 <- transform(test_set1,
                           default_payment_next_month = as.factor(default_payment_next_month)
)
View(test_set1)
class(test_set1$default_payment_next_month)
modelNBC <- naive_bayes(default_payment_next_month ~ ., data = training_set1)
modelNBC
plot(modelNBC)
predict(modelNBC, test_set1 ,type = 'prob')
ap <- predict(modelNBC, test_set1)
x <- table(test_set1$default_payment_next_month,ap)
cm2 <- confusionMatrix(x)
cm2
xm3 <- cm2$overall[1]
xm3


# Random Forest:

myData$default_payment_next_month = as.factor(myData$default_payment_next_month)
myData$SEX = as.factor(myData$SEX)
myData$EDUCATION = as.factor(myData$EDUCATION)
myData$MARRIAGE = as.factor(myData$MARRIAGE)
remove_feature = c(1)
our.modified.data = myData[, -remove_feature]
str(our.modified.data)
set.seed(123)
splitnew = sample.split(our.modified.data$default_payment_next_month, SplitRatio = 0.80)
splitnew
training_setnew = subset(our.modified.data, splitnew == TRUE)
View(training_setnew)
test_setnew = subset(our.modified.data, splitnew == FALSE)
View(test_setnew)
classifier.rf = randomForest(formula = default_payment_next_month ~., 
                             data = training_setnew, ntree = 10)
classifier.rf
our.predict.rf = predict(classifier.rf, newdata = test_setnew, type = "class")
our.predict.rf
cm3 <- confusionMatrix(our.predict.rf, test_setnew$default_payment_next_month)
cm3
xm4 <- cm3$overall[1]
xm4

help(geom)

# Comparison of Accuracy of Models:

modelComparison <- data.frame(Models = c('SVM',
                                      'Decision Tree',
                                      'Naive Bayes',
                                      'Random Forest',
                                      'Neural Network'),
                            Accuracy_of_Models = c(xm1,
                                         xm2,
                                         xm3,
                                         xm4,
                                         xm5))

plotAccuracy <- ggplot(aes(x=Models, y=Accuracy_of_Models, fill = Models), data=modelComparison) +
  geom_bar(stat='identity') +
  ggtitle('Comparative Accuracy of Models') +
  xlab('Models') +
  ylab('Accuracy')
plotAccuracy



# Input Data for Predictions:

newPdata1 <- data.frame(
  ID = 2,
  LIMIT_BAL = 300202,
  SEX = 1,
  EDUCATION = 2,
  MARRIAGE = 1,
  AGE = 22,
  PAY_0  = 0,
  PAY_2 = 0,
  PAY_3 = 0,
  PAY_4 = 0,
  PAY_5 = 0,
  PAY_6 = 0,
  BILL_AMT1 = 23223,
  BILL_AMT2 = 2232,
  BILL_AMT3 = 65333,
  BILL_AMT4 = 17263,
  BILL_AMT5 = 38748,
  BILL_AMT6 = 35463,
  PAY_AMT1 = 20000,
  PAY_AMT2 = 356378,
  PAY_AMT3 = 63563,
  PAY_AMT4 = 37277,
  PAY_AMT5 = 30383,
  PAY_AMT6 = 36377
)
View(newPdata1)
