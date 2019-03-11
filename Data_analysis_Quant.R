#################
#  VU AMSTERDAM #
#               #
#  Created By:  #
#     Alex      #
#    Berkout    #
# AND           #
#     Coen      #
#   van der     #
#     Geest     #
#################                                                                                                                          

#this code is free the use under the condition to attribute the original authors by refering the following:
#Berkhout, A.T., & van der Geest, C. (2019) R Google Play Store Permissons Extractor, version 1.0, R Script, https://github.com/Snat3r/Quant_P4

library(tidyverse)
library(car)

QP4 = QP4_Data

#first count missing values in Rating / what are we going to do? remove all the rows or mean or median replacement?
# NA == 646
sum(is.na(QP4_Data$rating))

#Replace mising values rating with mean
NA2mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
QP4_Data$rating <- NA2mean(QP4_Data$rating)

#Descriptive statistics for all the variables
#Dangerous permissions count
summary(QP4_Data$dperm)
sd(QP4_Data$dperm)
hist(QP4_Data$dperm) #right skewed, mean is higher then the median

# a logarithmic transformations of the intalls could solve the problem
log(QP4_Data$dperm) %>% 
  hist()

#intalls
summary(QP4_Data$intalls)
sd(QP4_Data$intalls)
hist(QP4_Data$intalls) #right skewed, mean is higher then the median

# a logarithmic transformations of the intalls could solve the problem
log(QP4_Data$intalls) %>% 
  hist()

#rating
summary(QP4_Data$rating)
sd(QP4_Data$rating)
hist(QP4_Data$rating) #left skewed, median is higher then the mean

# a exponential transformation of the rating solves the problem
QP4_Data$rating = exp(QP4_Data$rating)
hist(QP4_Data$rating) #now normaly distributed

#Estimate the regression model without square term as baseline and show the results
reg1 <- lm(intalls ~ dperm, data= QP4_Data)
summary(reg1)

#Estimate the regression model with square term and show the results
#Note that I am using the poly() function here to facilitate predictions later on; others notations exist, such as dperm + I(dperm^2)
reg2 <- lm(intalls ~ poly(dperm, 2), data=QP4_Data)
summary(reg2)

#Generate some meaningful values of dperm to predict lateron
prd <- data.frame(dperm = c(1:29))

#Use the estimations from the baseline and polynomial regression model to predict intalls based on observed values of dperm
prd$intalls_hat <- predict(reg1, newdata=prd)
prd$intalls_hat_sq <- predict(reg2, newdata=prd)

#comparing the plots
plot(prd$dperm, prd$intalls_hat, type='l', xlab="dangerous permissions", ylab="Expected number of installs ")
plot(prd$dperm, prd$intalls_hat_sq, type='l', xlab="dangerous permissions", ylab="Expected number of installs")

#comparing the two models using an F-test (ANOVA), we can conclude that the model including the squared term fits our data significantly better 
anova(reg1, reg2)

######### Building moderation model #########
#Estimate the regression model including the numverapp-cat5 interaction effect, and show the result
mod1 <- lm(intalls ~ poly(dperm,2) + dperm*Leisure + rating, QP4_Data)
summary(mod1)
plot(mod1)

#Generate some meaningful data for prediction
prd <- data.frame(dperm = c(0:29))
prd1 <- data.frame(dperm = c(0:29), Leisure = min(QP4_Data$Leisure), rating = mean(QP4_Data$rating))
prd2 <- data.frame(dperm = c(0:29), Leisure = max(QP4_Data$Leisure), rating = mean(QP4_Data$rating))

#Predict the average number of intalls for games and non-games
prd$intalls_hat_noLeisure <- predict(mod1, prd1)
prd$intalls_hat_Leisure <- predict(mod1, prd2)

#Set up the plot area
plot(prd$dperm, prd$intalls_hat_Leisure, type="n", xlab="Number of Dangerous permissions", ylab="Expected number Intalls")

#Plot the regression lines for games and non-games
lines(prd$dperm, prd$intalls_hat_noLeisure, col="red")
lines(prd$dperm, prd$intalls_hat_Leisure, col="blue")
legend("topleft", legend=c("No Leisure", "Leisure"),
       col=c("red", "blue"), lty=1:1, box.lty=0, cex=0.8, y.intersp=0.3)
