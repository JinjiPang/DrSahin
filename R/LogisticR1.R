###April 23rd, 2023
###Jinji Kimki Pang


## Fitting logistic regression to farm data
############################################


#As a first step we load the csv data using the read.csv() function.
##Make sure that the parameter na.strings is equal to c("") so that each missing value is coded as a NA.

training.data.raw <- read.csv('../data/train.csv',header=T,na.strings=c(""))

hdp <- read.csv("https://stats.idre.ucla.edu/stat/data/hdp.csv")


# Output the number of missing values for each column
sapply(training.data.raw,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(training.data.raw, function(x) length(unique(x)))


# A visual way to check for missing data
library(Amelia)
missmap(training.data.raw, main = "Missing values vs observed")

# Subsetting the data
data <- subset(training.data.raw,select=c(2,3,5,6,7,8,10,12))

# Substitute the missing values with the average value
data$Age[is.na(data$Age)] <- mean(data$Age,na.rm=T)

# Check, now Age should not have any missing values
missmap(data, main = "Missing values vs observed")

# R should automatically code Embarked as a factor(). A factor is R's way of dealing with
# categorical variables
str(data)
data$Sex<-as.factor(data$Sex)
is.factor(data$Sex)

data$Embarked<-as.factor(data$Embarked)
is.factor(data$Embarked)

# Check categorical variables encoding for better understanding of the fitted model
contrasts(data$Sex)
contrasts(data$Embarked)

# Remove rows (Embarked) with NAs
data <- data[!is.na(data$Embarked),]
rownames(data) <- NULL

# Train test splitting
train <- data[1:800,]
test <- data[801:889,]


# Model fitting
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)
summary(model)


# Analysis of deviance
anova(model,test="Chisq")














