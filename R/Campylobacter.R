library(Amelia)
library(dplyr)
library(tidyverse)
library(lme4)
##Campylobacter

##Step 1:Data cleaning
#################################################################################
##Read in data file
campydata <- read.csv('../data/campylobacter-metadata.csv',header=T,na.strings=c(""))

# Output the number of missing values for each column
sapply(campydata,function(x) sum(is.na(x)))

# Quick check for how many different values for each feature
sapply(campydata, function(x) length(unique(x)))

# A visual way to check for missing data
missmap(campydata, main = "Missing values vs observed")

# Subsetting the data
data <- subset(campydata,select=c(1,2,3,10,11,12))

missmap(data, main = "Missing values vs observed")

##remove samples that doesn't have campy status
data<-data%>%drop_na()
# Check, now Age should not have any missing values
missmap(data, main = "Missing values vs observed")

## create Farm_House variable "FH"
data$FH<-paste(data$FarmID,"-",data$HouseID)

sapply(data, function(x) length(unique(x)))


str(data)
##change variable attribute
data$FarmID <- as.factor(data$FarmID)
data$HouseID <- as.factor(data$HouseID)
data$Sample_Type <- as.factor(data$Sample_Type)
data$Cycle <- as.factor(data$Cycle)
data$Month <- as.factor(data$Month)
data$FH <- as.factor(data$FH)
data$Date<-as.Date(data$Date, "%m/%d/%y")
data$Test<-as.numeric(data$Test)


##create season variable based on cycle
data<-data%>%
  mutate(
    Season =case_when(
      Cycle == "1st cycle" ~ "Fall-Winter",
      Cycle == "2nd cycle" ~ "Winter-Spring",
      Cycle == "3rd cycle" ~ "Spring-Summer",
      Cycle == "4th cycle" ~ "Summer-Fall",
      Cycle == "6th cycle" ~ "Fall-Winter",
      Cycle == "7th cycle" ~ "Winter-Spring",
      Cycle == "8th cycle" ~ "Spring-Summer",
      Cycle == "5th cycle" ~ "Summer-Fall"
      ))

##create campylobacter variable based on CFU
data<-data%>%
  mutate(
    Campylobacter =case_when(
      Test <= 0 ~ "Negative",
      Test > 0 ~ "Positive"
    ))

data$target <- 0
data$target[data$Campylobacter == "Positive"] <- 1



# Mutate a new variable called Month
data <- data %>%
  mutate(Month = lubridate::month(Date, label = FALSE))

str(data)


# View the first few rows of the updated dataset
head(data)


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



## Step 1 univariate analysis gave same results as the paper did

lapply(c("FarmID","HouseID","Sample_Type",
         "Cycle","FH", "Season"),

       function(var) {

         formula    <- as.formula(paste("target ~", var))
         res.logist <- glm(formula, data = data, family = binomial)

         summary(res.logist)
       })


#################################################################################

## Step 2 logistic regression

# Fit a logistic regression model
model <- glm(target ~ FarmID + Sample_Type + Month + FarmID:HouseID, data = data, family = "binomial")

# Display the summary of the model
summary(model)



#############################################################################

# Fit a logistic mixed-effects model, with Season
model <- glmer(target ~ Sample_Type + season + (1 | FarmID) + (1 | FarmID:HouseID),
               data = data, family = binomial)

# View the model summary
summary(model)



# Fit a logistic mixed-effects model, with Month
model <- glmer(target ~ Sample_Type + Month + (1 | FarmID) + (1 | FarmID:HouseID),
               data = data, family = binomial)

# View the model summary
summary(model)


