setwd('D:/_College/Github')

library(corrplot)
library(ggplot2)
library(DescTools)
library(QuantPsyc)
library(leaps)
library(RColorBrewer)
library(Hmisc)
library(psych)
library(car)
library(MASS)
library(clusterGeneration)
library(corrplot)


CK <- read.csv(file="KingCounty.csv", header=TRUE, sep=",")
CK

hist(CK$sqft_basement, col="red")

boxplot(CK)

corMat = cor(CK)
corMat

corrplot(corMat, method = "ellipse")


fullFit = lm(price ~ ., data=CK)
summary(fullFit)

#as there is singularity with 2 variables i deleted those. Singularity happens when there is a very high correlation
CK <- CK[-c(12, 14)]
CK

fullFit = lm(price ~ ., data=CK)
summary(fullFit)

#From the summary we can say that all the variables have good estimate value. 
#Moreover, there t-tests are higha dn significant 
#Also the p-values of all the variables are less than 0.05 which says that the variables are significant to consider for model
#Also, the p-value of overall model is less than 0.05 which denotes that the model is significant to use for further analysis


VIF(fullFit)

#By looking at the VIF scores of the attributes we can see all have values less than 5 and are hence significant to consider.


# The R function "step" can perform stepwise regression, but to get going, we need to feed 
# it the two "bounding" models so that it knows what to work with
null = lm(price ~ 1, data=CK)
null

full = lm(price ~ ., data=CK)
full


# First we do a forward search - Forward Stepwise
CKforward = step(null, scope = list(lower=null, upper=full), direction="forward")
summary(CKforward)

# Compare the results to the full search above
#Forward step-wise regression selected the below equation.
#grade + yr_renovated + sqft_living + sqft_lot15 + sqft_living15 + view + condition + bathrooms + bedrooms + 
# date + sqft_lot + waterfront + floors + sqft_basement





# Next do a backward search - Backward Stepwise
CKBackward = step(full, direction="backward")
summary(CKBackward)

#Backward Step wise regression selected the below equation:
#date + bedrooms + bathrooms + sqft_living + 
#sqft_lot + floors + waterfront + view + condition + grade + sqft_basement + yr_renovated + sqft_living15 + sqft_lot15

# Finally we do a "step wise" search combining the two - Both Forward and Backward Step wise
CKStep = step(null, scope = list(upper=full), direction="both")

summary(CKStep)
summary(CKforward)
summary(CKBackward)

CKStep

# As all the 3 models are giving very similar result I will go with the Step wise Backward model.


names(CK)


write.csv(CK,'D:/_College/Github/final.csv',  row.names = FALSE)


