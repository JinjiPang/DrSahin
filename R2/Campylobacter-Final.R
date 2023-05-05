library(Amelia)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
##Campylobacter

##Step 1:Data cleaning
#################################################################################
##Read in data file
campydata <- read.csv('../data/campylobacter-metadata-os.csv',header=T,na.strings=c(""))

campydata<-campydata[, 1:13]

# Output the number of missing values for each column
sapply(campydata,function(x) sum(is.na(x)))


campydata<-subset(campydata,!is.na(CFU))

## get rid of samples that had dry boot swab, which is represented by BootDry==Y (yes)
campydata<-subset(campydata,BootDry=="N")


# Quick check for how many different values for each feature
sapply(campydata, function(x) length(unique(x)))

# A visual way to check for missing data
missmap(campydata, main = "Missing values vs observed")

# Subsetting the data
data <- subset(campydata,select=c(1,2,3,10,11,12,13))

missmap(data, main = "Missing values vs observed")

##remove samples that doesn't have campy status
data<-data%>%drop_na()
# Check, now data should not have any missing values
missmap(data, main = "Missing values vs observed")

## check levels of each variable
sapply(data, function(x) length(unique(x)))



##change variable attribute
data$FarmID <- as.factor(data$FarmID)
data$HouseID <- as.factor(data$HouseID)
data$Test<-as.numeric(data$Test)
data$Sample_Type <- as.factor(data$Sample_Type)
data$Cycle <- as.factor(data$Cycle)
data$BootDry <- as.factor(data$BootDry)
data$Date<-as.Date(data$Date, "%m/%d/%y")

str(data)

##Mutate new variables
##1.create Campylobacter variable based on CFU
data<-data%>%
  mutate(
    Campylobacter =case_when(
      Test <= 0 ~ "Negative",
      Test > 0 ~ "Positive"
    ))

##use number to show campy status, create target variable, target=1, positive;target=0,negative
data$target <- 0
data$target[data$Campylobacter == "Positive"] <- 1

# Mutate a new variable called Month
data <- data %>%
  mutate(Month = lubridate::month(Date, label = FALSE))

data$Month <- as.factor(data$Month)


##create season variable based on cycle
data<-data%>%
  mutate(
    season =case_when(
      Month == 1 ~ "Winter",
      Month == 2 ~ "Winter",
      Month == 3 ~ "Spring",
      Month == 4 ~ "Spring",
      Month == 5 ~ "Spring",
      Month == 6 ~ "Summer",
      Month == 7 ~ "Summer",
      Month == 8 ~ "Summer",
      Month == 9 ~ "Fall",
      Month == 10 ~ "Fall",
      Month == 11 ~ "Fall",
      Month == 12 ~ "Winter"))

data$season <- as.factor(data$season)


str(data)

## Step 1 univariate analysis, did this but turned out all the predictors had significant effect on the Campylobacter
## farm status, therefore this result was not included in the manuscript

lapply(c("FarmID","HouseID","Sample_Type",
         "Cycle","season","Month"),

       function(var) {

         formula    <- as.formula(paste("target ~", var))
         res.logist <- glm(formula, data = data, family = binomial)

         summary(res.logist)
       })


#################################################################################

## Step 2 logistic regression

# Fit a logistic mixed-effects model, with Month
model_month <- glmer(target ~ Sample_Type + Month + (1 | FarmID) + (1 | FarmID:HouseID),
               data = data, family = binomial)

# View the model summary
summary(model_month)

vcov(model_month)

se <- sqrt(diag(vcov(model_month)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(model_month), LL = fixef(model_month) - 1.96 * se, UL = fixef(model_month) + 1.96 *
                se)

print(exp(tab))

# Fit a logistic mixed-effects model, with season
model_season <- glmer(target ~ Sample_Type + season + (1 | FarmID) + (1 | FarmID:HouseID),
               data = data, family = binomial)

# View the model summary
summary(model_season)

## model_month(AIC=5206) has lower AIC compared with model_season (AIC=5571), therefore, we will proceed with model_month



