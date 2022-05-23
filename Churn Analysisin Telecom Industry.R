# Reading the customer data
data <- read.csv("C:/Users/Puneet Kishore/OneDrive/Desktop/Imarticus/R/Project 3 - Churn Analysisin Telecom Industry/churn.csv")
data <- data.frame(data)

# Initial Assessmenmt
dim(data)
str(data)
summary(data)

# Checking for the Null values
library(naniar)
vis_miss(data)
colSums(is.na(data))

# Changing "SeniorCitizen" into categorical variable
da <- data
da$SeniorCitizen[1:4]
for (i in seq(length(da$SeniorCitizen))) {
  if(da$SeniorCitizen[i]<=0.6) { da$SeniorCitizen[i]<-"No" }
  else { da$SeniorCitizen[i] <-"Yes" }
}

da$SeniorCitizen <- factor(da$SeniorCitizen)
str(da)
head(da$SeniorCitizen)

data <- data.frame(da)
str(data)

# Splitting into Train and Test data
set.seed(2)
l <- floor(0.75*nrow(data))
train <- sample(1:nrow(data), l)
ID_train <- data[train,1]
ID_test <- data[-train,1]
data_train <- data[train,-1]
data_test <- data[-train,-1]

# Extrapolatory Analysis

# Bar Plot for categorical variables

barplot(table(data_train$gender), col=c('Pink','Blue'), main = "Bar Plot of Gender", xlab="Gender", ylab="Counts")
barplot(table(data_train$SeniorCitizen), col=c('Green','Red'), main = "Bar Plot of Senior Citizen", xlab="Senior Citizen", ylab="Counts")
barplot(table(data_train$Partner), col=c('Pink','Blue'), main = "Bar Plot of Partner", xlab="Partner", ylab="Counts")
barplot(table(data_train$Dependents), col=c('Pink','Blue'), main = "Bar Plot of Dependents", xlab="Dependents", ylab="Counts")
barplot(table(data_train$CallService), col=c('Pink','Blue'), main = "Bar Plot of Call Service", xlab="Call Service", ylab="Counts")

barplot(table(data_train$MultipleConnections), col=c('Red','Blue','Green'), main = "Bar Plot of Multiple Connections", xlab="Multiple Connections", ylab="Counts")
barplot(table(data_train$InternetConnection), col=c('Red','Blue','Green'), main = "Bar Plot of Internet Connection", xlab="Internet Connection", ylab="Counts")
barplot(table(data_train$OnlineSecurity), col=c('Red','Blue','Green'), main = "Bar Plot of Online Security", xlab="Online Security", ylab="Counts")
barplot(table(data_train$OnlineBackup), col=c('Red','Blue','Green'), main = "Bar Plot of Online Back up", xlab="Online Back up", ylab="Counts")
barplot(table(data_train$DeviceProtectionService), col=c('Red','Blue','Green'), main = "Bar Plot of Device Protection Service", xlab="Dewvice Protection Service", ylab="Counts")
barplot(table(data_train$TechnicalHelp), col=c('Red','Blue','Green'), main = "Bar Plot of Technical Help", xlab="Technical Help", ylab="Counts")
barplot(table(data_train$OnlineTV), col=c('Red','Blue','Green'), main = "Bar Plot of Online TV", xlab="Online TV", ylab="Counts")
barplot(table(data_train$OnlineMovies), col=c('Red','Blue','Green'), main = "Bar Plot of Online Movies", xlab="Online Movies", ylab="Counts")
barplot(table(data_train$Agreement), col=c('Red','Blue','Green'), main = "Bar Plot of Agreement", xlab="Agreement", ylab="Counts")

barplot(table(data_train$BillingMethod), col=c('Red','Green'), main = "Bar Plot of Billing Method", xlab="Billing Method", ylab="Counts")

barplot(table(data_train$PaymentMethod), col=c('Red','Yellow','Green','Blue'), main = "Bar Plot of Payment Method", xlab="Payment Method", ylab="Counts")

barplot(table(data_train$Churn), col=c('Red','Blue'), main = "Bar Plot of Churn", xlab="Churn", ylab="Counts")

# Boxplot for continuous variables
boxplot(data_train$tenure, main="Box Plot for Tenure", horizontal = TRUE)
boxplot(data_train$MonthlyServiceCharges, main="Box Plot for Monthly Service Charges", horizontal = TRUE)
boxplot(data_train$TotalAmount, main="Box Plot for Total Amount", horizontal = TRUE)

# Histogram for continuous variables
hist(data_train$tenure, main="Histogram for Tenure", col = "yellow", xlab = "Tenure")
hist(data_train$MonthlyServiceCharges, main="Histogram for Service Charges", col = "orange", xlab = "Service Charges")
hist(data_train$TotalAmount, main="Histogram for Total Amount", col = "red", xlab = "Total Amount")
skew(data_train$TotalAmount)

# Skewness and Kurtosis
install.packages("moments")
library(moments)

skewness(data_train$tenure) # Skewness=0.43, in (-2,2), within the bounds of normal distribution
kurtosis(data_train$tenure) # Kurtosis=1.83, in (-2,2),  within the bounds of normal distribution

skewness(data_train$MonthlyServiceCharges) # Skewness=-0.03, in (-2,2), within the bounds of normal distribution
kurtosis(data_train$MonthlyServiceCharges) # Kurtosis=1.76, in (-2,2), within the bounds of normal distribution

skewness(data_train$TotalAmount) # Skewness=1.15, in (-2,2), within the bounds of normal distribution
kurtosis(data_train$TotalAmount) # Kurtosis=3.32, in (-2,2), within the bounds of normal distribution

# Build a Tree
library(tree)
tree.data_train <- tree(Churn~., data_train)
summary(tree.data_train)

#Plot
plot(tree.data_train)
text(tree.data_train, pretty=0)

# Testing on tree
tree.pred1 <- predict(tree.data_train, data_test, type = 'class')

table(tree.pred1, data_test$Churn)
# Accuracy= (1392+800)/3084 = 71%

# Pruning the Tree
set.seed(3)
cv.data <- cv.tree(tree.data_train, FUN = prune.misclass)

names(cv.data)

cv.data

# Plotting Error
par(mfrow=c(1,2))
plot(cv.data$size, cv.data$dev, type='b', col='red', lwd=2)
plot(cv.data$k, cv.data$dev, type='b', col='red', lwd=2)

# Prune the tree to 4 classes
dev.off()

prune.data <- prune.misclass(tree.data_train, best=4)

plot(prune.data)
text(prune.data, pretty=0)

tree.predp <- predict(prune.data, data_test, type='class')

table(tree.predp, data_test$Churn)
# Accuracy=71%

# Bagging
library(randomForest)
set.seed(1)

bag.data <- randomForest(Churn~., data_train, mtry=10, importance=TRUE)
importance(bag.data)
# As the features that have top 3 MeanDecreaseGini
# MonthlyServiceChargews, tenure and TotalAmount are the most important variables

varImpPlot(bag.data, col='green', pch=10, cex=1.25)

bag.data

bag.predict <- predict(bag.data, data_test, type='class')
table(bag.predict, data_test$Churn)
# Accuracy=(1556+1158)/3084=88%

# Random Forest sqr(10)=mtry=3
set.seed(1)
rf.data <- randomForest(Churn~., data_train, mtry=3, importance=TRUE)
importance(rf.data)

# As the features that have top 3 MeanDecreaseGini
# MonthlyServiceChargews, tenure and TotalAmount are the most important variables

varImpPlot(rf.data, col='purple', pch=10, cex=1.25)

rf.data

rf..predict <- predict(rf.data, data_test, type='class')
table(rf..predict, data_test$Churn)
# Accuracy=(1560+1128)/3084=87.16%

# Accuracy :
# before pruning :71%
# after pruning : 71%
# bagging : 88%
# random forest : 87.16%