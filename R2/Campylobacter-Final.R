library(Amelia)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(kableExtra)
##Campylobacter

##Step 1:Data cleaning
#################################################################################
campydata <- read.csv('../data/campylobacter-metadata-os.csv',header=T,na.strings=c(""))

campydata<-campydata[, 1:13]

campydata$Flock<-paste(campydata$FarmID,"-",campydata$HouseID,"-",campydata$Cycle)
sapply(campydata, function(x) length(unique(x)))

# FarmID     HouseID Sample_Type    sampleID  Farm_House diluation_1 diluation_3 diluation_4
# 15           4           2          15         954         281         627         341
# CFU        Test        Date       Cycle     BootDry       Flock
# 984          71         109           8           2         410


#remove BB-H1,2,3,4, 2nd cycles


campyda<-campydata[campydata$Flock != "BB - H1 - 2nd cycle" & campydata$Flock != "BB - H2 - 2nd cycle"&
                     campydata$Flock != "BB - H3 - 2nd cycle" & campydata$Flock != "BB - H4 - 2nd cycle", ]

sapply(campyda, function(x) length(unique(x)))

# FarmID     HouseID Sample_Type    sampleID  Farm_House diluation_1 diluation_3 diluation_4
# 15           4           2          15         954         280         619         339
# CFU        Test        Date       Cycle     BootDry       Flock
# 984          71         109           8           2         406

#remove boot sample dry
DF<-subset(campyda,BootDry=="N")

sapply(DF, function(x) length(unique(x)))

# FarmID     HouseID Sample_Type    sampleID  Farm_House diluation_1 diluation_3 diluation_4
# 15           4           2          15         954         258         615         332
# CFU        Test        Date       Cycle     BootDry       Flock
# 974          71         101           8           1         379

DFdry<-subset(campyda,BootDry=="Y")

sapply(DFdry, function(x) length(unique(x)))
# FarmID     HouseID Sample_Type    sampleID  Farm_House diluation_1 diluation_3 diluation_4
# 8           4           2          15         485          56          23          22
# CFU        Test        Date       Cycle     BootDry       Flock
# 89          49           8           1           1          27

#remove no sample samples
df<-subset(DF,!is.na(Test))

#campydf has 6779 samples

sapply(df, function(x) length(unique(x)))

# FarmID     HouseID Sample_Type    sampleID  Farm_House diluation_1 diluation_3 diluation_4
# 15           4           2          15         954         256         614         332
# CFU        Test        Date       Cycle     BootDry       Flock
# 974          70         101           8           1         379

# A visual way to check for missing data
missmap(df, main = "Missing values vs observed")

# Subsetting the data
data <- subset(df,select=c(1,2,3,10,11,12,13,14))

missmap(data, main = "Missing values vs observed")

## check levels of each variable
sapply(data, function(x) length(unique(x)))

# FarmID     HouseID Sample_Type        Test        Date       Cycle     BootDry       Flock
# 15           4           2          70         101           8           1         379

##change variable attribute
data$FarmID <- as.factor(data$FarmID)
data$HouseID <- as.factor(data$HouseID)
data$Sample_Type <- as.factor(data$Sample_Type)
data$Test<-as.numeric(data$Test)
data$Date<-as.Date(data$Date, "%m/%d/%y")
data$Cycle <- as.factor(data$Cycle)
data$BootDry <- as.factor(data$BootDry)


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




data%>%group_by(Sample_Type)%>%
  summarise(
    count = n()
  )

# Sample_Type    count
#   B            1110
#   C            5668


## Step 1 univariate analysis, did this but turned out all the predictors had significant effect on the Campylobacter
## farm status, therefore this result was not included in the manuscript

lapply(c("FarmID","HouseID","Sample_Type",
         "Cycle","season","Month","Flock"),

       function(var) {

         formula    <- as.formula(paste("target ~", var))
         res.logist <- glm(formula, data = data, family = binomial)

         summary(res.logist)
       })


#################################################################################

## Step 2 logistic regression

# Fit a logistic mixed-effects model, with Production cycle
# FarmID:HouseID:Cycle equals to Flock

model_cycle <- glmer(target ~ Sample_Type + Cycle + (1 | FarmID) + (1 | FarmID:HouseID) + (1 | FarmID:HouseID:Cycle),
               data = data, family = binomial)


# View the model summary
summary(model_cycle)


vcov(model_cycle)

se <- sqrt(diag(vcov(model_cycle)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(model_cycle), LL = fixef(model_cycle) - 1.96 * se, UL = fixef(model_cycle) + 1.96 *
                se)

print(exp(tab))


