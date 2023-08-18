library(Amelia)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lme4)
library(kableExtra)
library(writexl)
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
# 984          70         109           8           2         410


# dfnosample<-subset(campydata,is.na(Test))

#
# dfBP<-subset(campydata,FarmID=="BP")
# dfWL<-subset(campydata,FarmID=="WL")
#
# dfnosample%>%group_by(Sample_Type)%>%
#   summarise(
#     count = n()
#   )

#
# write_xlsx(dfnosample, "../data/dfnosample.xlsx")


campydata<-campydata%>%
  mutate(
    Campylobacter =case_when(
      Test <= 0 ~ "Negative",
      Test > 0 ~ "Positive"))


# dfWL<-subset(campydata,FarmID=="WL")


#remove no sample samples
dftotal<-subset(campydata,!is.na(Test))

dftotal%>%group_by(Sample_Type)%>%
  summarise(
    count = n()
  )

# Sample_Type count
# <chr>       <int>
#   B            1191
#   C            6133


sapply(dftotal, function(x) length(unique(x)))


# > sapply(dftotal, function(x) length(unique(x)))
# FarmID       HouseID   Sample_Type      sampleID    Farm_House   diluation_1   diluation_3
# 15             4             2            15           954           279           626
# diluation_4           CFU          Test          Date         Cycle       BootDry         Flock
# 341           984            69           109             8             2           410
# Campylobacter
# 2



#remove BB-H1,2,3,4, 2nd cycles,BP-H1,2,3,4, 8th cycles

campyda<-dftotal[dftotal$Flock != "BB - H1 - 2nd cycle" & dftotal$Flock != "BB - H2 - 2nd cycle"&
                   dftotal$Flock != "BB - H3 - 2nd cycle" & dftotal$Flock != "BB - H4 - 2nd cycle"&
                 dftotal$Flock != "BP - H1 - 8th cycle" & dftotal$Flock != "BP - H2 - 8th cycle"&
                   dftotal$Flock != "BP - H3 - 8th cycle" & dftotal$Flock != "BP - H4 - 8th cycle",]




sapply(campyda, function(x) length(unique(x)))

# > sapply(campyda, function(x) length(unique(x)))
# FarmID       HouseID   Sample_Type      sampleID    Farm_House   diluation_1   diluation_3
# 15             4             2            15           954           278           618
# diluation_4           CFU          Test          Date         Cycle       BootDry         Flock
# 339           984            69           108             8             2           402
# Campylobacter
# 2



#remove boot sample dry
DF<-subset(campyda,BootDry=="N")

sapply(DF, function(x) length(unique(x)))

# > sapply(DF, function(x) length(unique(x)))
# FarmID       HouseID   Sample_Type      sampleID    Farm_House   diluation_1   diluation_3
# 15             4             2            15           954           256           614
# diluation_4           CFU          Test          Date         Cycle       BootDry         Flock
# 332           974            69           100             8             1           375
# Campylobacter
# 2


DF%>%group_by(Sample_Type)%>%
  summarise(
    count = n()
  )

# Sample_Type count
# <chr>       <int>
# 1 B          1110
# 2 C         5608


##Have a look at the samples with dry boot swabs, there are 27 flocks
DFdry<-subset(campyda,BootDry=="Y")

sapply(DFdry, function(x) length(unique(x)))

# > sapply(DFdry, function(x) length(unique(x)))
# FarmID       HouseID   Sample_Type      sampleID    Farm_House   diluation_1   diluation_3
# 8             4             2            15           486            56            23
# diluation_4           CFU          Test          Date         Cycle       BootDry         Flock
# 22            89            49             8             1             1            27
# Campylobacter
# 2


#DF has 6778 samples

sapply(DF, function(x) length(unique(x)))


# A visual way to check for missing data
missmap(DF, main = "Missing values vs observed")

# Subsetting the data
data <- subset(DF,select=c(1,2,3,10,11,12,13,14,15))

missmap(data, main = "Missing values vs observed")

## check levels of each variable
sapply(data, function(x) length(unique(x)))

# > sapply(data, function(x) length(unique(x)))
# FarmID       HouseID   Sample_Type          Test          Date         Cycle       BootDry
# 15             4             2            69           100             8             1
# Flock Campylobacter
# 375             2

##change variable attribute
data$FarmID <- as.factor(data$FarmID)
data$HouseID <- as.factor(data$HouseID)
data$Sample_Type <- as.factor(data$Sample_Type)
data$Test<-as.numeric(data$Test)
data$Date<-as.Date(data$Date, "%m/%d/%y")
data$Cycle <- as.factor(data$Cycle)
data$BootDry <- as.factor(data$BootDry)
data$Flock<-as.factor(data$Flock)
data$Campylobacter<-as.factor(data$Campylobacter)

str(data)

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

unique(data$season)

data$season<-factor(data$season, levels = c("Spring","Summer","Fall","Winter"))

data%>%group_by(Sample_Type)%>%
  summarise(
    count = n()
  )

# Sample_Type    count
#   B            1110
#   C            5608



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

###########

# Fit a logistic mixed-effects model, with month

model_month <- glmer(target ~ Sample_Type + Month + (1 | FarmID) + (1 | FarmID:HouseID) + (1 | FarmID:HouseID:Cycle),
                     data = data, family = binomial)


# View the model summary
summary(model_month)


vcov(model_month)

se <- sqrt(diag(vcov(model_month)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(model_month), LL = fixef(model_month) - 1.96 * se, UL = fixef(model_month) + 1.96 *
               se)

print(exp(tab))

##08-18-2023, minor revision for avian disease paper
# > print(exp(tab))
# Est           LL           UL
# (Intercept)    3.324981e-05 1.033185e-07 1.070041e-02
# Sample_TypeC   2.430417e+01 1.642097e+01 3.597186e+01
# Cycle2nd cycle 2.564350e+03 1.861002e+01 3.533522e+05
# Cycle3rd cycle 2.461029e+00 1.910608e-02 3.170018e+02
# Cycle4th cycle 1.852431e+01 1.306907e-01 2.625665e+03
# Cycle5th cycle 1.142146e-01 1.306393e-03 9.985487e+00
# Cycle6th cycle 2.905455e-02 3.067735e-04 2.751759e+00
# Cycle7th cycle 2.187093e+02 1.591126e+00 3.006283e+04
# Cycle8th cycle 2.607471e+04 9.064713e+01 7.500408e+06



# Fit a logistic mixed-effects model, with month




model_season <- glmer(target ~ Sample_Type + season + (1 | FarmID) + (1 | FarmID:HouseID) + (1 | FarmID:HouseID:Cycle),
                     data = data, family = binomial)


# View the model summary
summary(model_season)


vcov(model_season)

se <- sqrt(diag(vcov(model_season)))
# table of estimates with 95% CI
tab <- cbind(Est = fixef(model_season), LL = fixef(model_season) - 1.96 * se, UL = fixef(model_season) + 1.96 *
               se)

print(exp(tab))


