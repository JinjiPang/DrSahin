## Prevalence and risk factors of Salmonella in commercial poultry farms in Nigeria

saldata <- read.csv('../data/saldata.csv',header=T,na.strings=c(""))

str(saldata)


### univariate analysis

saldata$target <- 0
saldata$target[saldata$Molecular == "Positive"] <- 1

## univariate analysis gave same results as the paper did

lapply(c("Production_System","Outbreak","Episodes","Neighbouring_Outbreaks",
         "Farm_fenced","Waste_management","Livestock","Proximity_Poultry",
         "Foot_dip","Lavatory","Frequency"),

       function(var) {

         formula    <- as.formula(paste("target ~", var))
         res.logist <- glm(formula, data = saldata, family = binomial)

         summary(res.logist)
       })



## logistic regression analysis

# Subsetting the data
testdata <- subset(saldata,select=c(6,9,11,12,13,17))


# Model fitting
model_sal <- glm(target ~.,family=binomial(link='logit'),data=testdata)
summary(model_sal)


# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)
# (Intercept)                         -5.1883     1.3670  -3.795 0.000147 ***
#   Production_SystemDeep Litre System   1.7522     1.0433   1.679 0.093060 .
# Neighbouring_OutbreaksYes            0.7881     1.1217   0.703 0.482333
# Waste_managementOn farm              3.1550     1.1217   2.813 0.004914 **
#   LivestockYes                         2.4426     0.9738   2.508 0.012132 *
#   Proximity_PoultryYes                 1.3777     1.0402   1.324 0.185340
















