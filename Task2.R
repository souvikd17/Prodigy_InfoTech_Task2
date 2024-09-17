#Task-02

## PREPARING THE DATA FOR ANALYSIS
# importing the given data
Titanic <- read.csv("C://Users//HP//Desktop//Prodigy//Task2//Titanic-dataset.csv")
View(Titanic)

#'Name','Ticket','Cabin' and 'PassengerId' is irrelevant for our analysis
# Removing the irrelevant columns from data set
TNew <- Titanic[,-c(1,4,9,11)]

# finding the number of missing observations
sum(is.na(TNew))
# columns with number of missing observations
colSums(is.na(TNew))
# We need to replace the missing values with the mean of the rests
# Finding mean of the other observations of column 'Age'
AgeMean <- round(mean(TNew$Age, na.rm =TRUE),2)
AgeMean
# replacing the missing observations with 'AgeMean'
TNew$Age <- replace(TNew$Age, is.na(TNew$Age) == 1, AgeMean)
View(TNew)
# Rechecking for missing values
which(is.na(TNew))

#Removing the observations for which 'Embarked' column is empty
index <- which(TNew$Embarked == "")
TNew <- TNew[-index,]

#Converting the 'columns taking 0-1 values' to factor
TNew$Sex <- as.factor(TNew$Sex)
TNew$Embarked <- as.factor(TNew$Embarked)
TNew$Pclass <- as.factor(TNew$Pclass)

#The final cleaned data
View(TNew)

##Plotting the final cleaned data
library(ggplot2)
#Plot for Survived column
data <- data.frame("Category" = c('0','1'), 
                   "values" = c(sum(TNew$Survived == '0'), 
                                sum(TNew$Survived == '1')))
ggplot(data, aes(x = Category, y = values, fill = Category)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = values)) +
  scale_fill_manual(values=c('lightblue','lightgreen')) +
  labs(title = "Survived") +
  theme_minimal()

#Plot for Pclass column
data <- data.frame("Category" = c('1','2','3'), 
                   "values" = c(sum(TNew$Pclass == '1'), 
                                sum(TNew$Pclass == '2'),
                                sum(TNew$Pclass == '3')))
ggplot(data, aes(x = Category, y = values, fill = Category)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = values)) +
  scale_fill_manual(values=c('pink','lightblue', 'lightgreen')) +
  labs(title = "Pclass") +
  theme_minimal()

#Plot for Sex
data <- data.frame("Category" = c('male','female'), 
                   "values" = c(sum(TNew$Sex == 'male'), 
                                sum(TNew$Sex == 'female')))
ggplot(data, aes(x = Category, y = values, fill = Category)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = values)) +
  scale_fill_manual(values=c('pink','lightblue')) +
  labs(title = "Sex") +
  theme_minimal()

#Plot for Embarked
data <- data.frame("Category" = c('S','C','Q'), 
                   "values" = c(sum(TNew$Embarked == 'S'), 
                                sum(TNew$Embarked == 'C'),
                                sum(TNew$Embarked == 'Q')))
ggplot(data, aes(x = Category, y = values, fill = Category)) +
  geom_bar(stat="identity") +
  geom_text(aes(label = values)) +
  scale_fill_manual(values=c('pink','lightblue', 'lightgreen')) +
  labs(title = "Embarked") +
  theme_minimal()

#Plot for Age
ggplot(data = TNew, aes(x = Age)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 col = 'black', fill='lightyellow') +
  labs(title = 'Age') +
  geom_density() +
  theme_minimal()

#Plot for Fare
ggplot(data = TNew, aes(x = Fare)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25, 
                 col = 'black', fill='lightyellow') +
  labs(title = 'Fare') +
  geom_density() +
  theme_minimal()

#Plot for Parch
ggplot(data = TNew, aes(x = Parch)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 col = 'black', fill='lightyellow') +
  labs(title = 'Parch') +
  geom_density() +
  theme_minimal()

#Plot for SibSp
ggplot(data = TNew, aes(x = SibSp)) +
  geom_histogram(aes(y = after_stat(density)), bins = 25,
                 col = 'black', fill='lightyellow') +
  labs(title = 'SibSp') +
  geom_density() +
  theme_minimal()

#Predicting model using GLM
attach(TNew)
model <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Embarked, 
             family = 'binomial', data = TNew)
model
summary(model)

#checking the goodness of fit of the fitted model
anova(model, test = 'Chisq')
model2 <- step(model, direction = 'backward')

#plotting residuals
library("hnp")
hnp(model2)
plot(residuals(model2, 'pearson'), main = 'Residuals Plot', xlab = 'Fitted Values', ylab = 'Residuals')


library("caret")
library("ROCR")
prd1 <- predict(model2, type = 'response', TNew)
prd2 <- prediction(prd1, TNew$Survived)
perf <- performance(prd2, 'tpr', 'fpr')

plot(perf, print.cutoffs.at = seq(0, 1, 0.1), colorize = TRUE)
abline(a = 0, b = 1)
auc <- performance(prd2, 'auc')
legend(0.6, 0.4, auc, title = 'AUC', cex = 0.9)

#testing
Data <- read.csv("C://Users//HP//Desktop//Prodigy//Task2//Titanic-dataset.csv")
index <- which(Data$Embarked == "")
Data <- Data[-index,]
NData <- Data[,-c(1,4,9,11)]
AgeMean <- round(mean(NData$Age, na.rm =TRUE),2)
NData$Age <- replace(NData$Age, is.na(NData$Age) == 1, AgeMean)
NData$Sex <- as.factor(NData$Sex)
NData$Embarked <- as.factor(NData$Embarked)
NData$Pclass <- as.factor(NData$Pclass)


prdict <- predict(model2, NData)
val = ifelse((prdict)>0.5, 1, 0)
r = data.frame("PassengerId" = Data[,1], "Survived" = val)
View(r)