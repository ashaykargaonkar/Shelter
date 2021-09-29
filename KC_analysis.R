setwd('D:/_College/Github/King County/New/')

library(corrplot)

#importing the data
data <- read.csv(file="KC_cleaned.csv", header=TRUE, sep=",")
data

#printing the dimension of the data
dim(data)

#printing column names
colnames(data)


#Applying linear model on the data
#Here target variable is 'price' and rest of the variables are independent variable.

fullFit = lm(price ~ ., data=data)
summary(fullFit)

#Deleting the values showing NA results. This means that the they are in singularity which states that the correlation between the 
#variables is high and these variables are safe to delete and not consider for further analysis.

#Variables named 'view_4', 'condition_5', 'age_of_house' are showing NA values and are safe to delete.

data = subset(data, select = -c(view_4,condition_5, age_of_house))
dim(data)

fullFit = lm(price ~ ., data=data)
summary(fullFit)

#Deleting the columns which are not significant, that is to check if the p-value is less than 0.05. If the significant value is less than 0.05 then 
#delete the attribute or column.

#From the below output we can see that the p-value of 'sqft_living attribute' is very less and is safe to delete.

data = subset(data, select = -c(sqft_living))
dim(data)

fullFit = lm(price ~ ., data=data)
summary(fullFit)

#As we can see that the p-value of overall model is very less, we can say that the model is significant.
#Also, you can see that the Multiple-R-Squared value is 65.84% which is decent, not super good but definitely not bad.
#Also, the p-values of the rest of the attribute are significant.
#Therefore, we can say that this model can be used for further analysis.

library(DescTools)

VIF(fullFit)

#By checking the VIF scores we can see that none of the values are greater than 10.
#This states that all attributes are independent of each other, or have no collinearity between them and are good to consider for further analysis.

#The further analysis is done back in the previous jupyter notebook.
#OR you can click this link to continue: https://colab.research.google.com/drive/1DsV-rnybOICK5nbxt4ABEuCKsfCqyrVd?authuser=1#scrollTo=12wyPrW4MEal