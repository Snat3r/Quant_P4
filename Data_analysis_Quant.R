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

##### Test regression model######
# to see if polynomal regressions works better then linear regression
#Estimate the regression model without square term as baseline and show the results
reg1 <- lm(intalls ~ dperm, data= QP4_Data)
summary(reg1)

#Estimate the regression model with square term and show the results
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

#comparing the two models using an F-test the polynomal regreession works best
anova(reg1, reg2)

######### Building moderation model #########
#Moderation model with Leisure
mod_leis <- lm(intalls ~ poly(dperm,2) + dperm*Leisure + rating, QP4_Data)
summary(mod_leis)

#Moderation model with Living
mod_liv <- lm(intalls ~ poly(dperm,2) + poly(dperm*Living,2) + rating, QP4_Data)
summary(mod_liv)

#Moderation model with Productivity
mod_prod <- lm(intalls ~ dperm + poly(dperm*Productivity, 2) + rating, QP4_Data)
summary(mod_prod)


#Generate some meaningful data for prediction
prd <- data.frame(dperm = c(0:29))
prd1 <- data.frame(dperm = c(0:29), 
                   Leisure = min(QP4_Data$Leisure), 
                   Living = min(QP4_Data$Living), 
                   Productivity = min(QP4_Data$Productivity), 
                   rating = mean(QP4_Data$rating))
prd2 <- data.frame(dperm = c(0:29), 
                   Leisure = max(QP4_Data$Leisure), 
                   Living = max(QP4_Data$Living), 
                   Productivity = max(QP4_Data$Productivity), 
                   rating = mean(QP4_Data$rating))

#Predict the average number of intalls for Leisure and non-Leisure
prd$intalls_noLeisure <- predict(mod_leis, prd1)
prd$intalls_Leisure <- predict(mod_leis, prd2)

#Predict the average number of intalls for Living and non-Living
prd$intalls_noLiving <- predict(mod_liv, prd1)
prd$intalls_Living <- predict(mod_liv, prd2)

#Predict the average number of intalls for productivity and non-productivity
prd$intalls_noProductivity <- predict(mod_prod, prd1)
prd$intalls_Productivity <- predict(mod_prod, prd2)

#Set up the plot area
plot(prd$dperm, prd$intalls_Leisure, type="n", xlab="Number of Dangerous permissions", ylab="Expected number Intalls")

#Plot the regression lines for Leisure and non-Leisure
lines(prd$dperm, prd$intalls_noLeisure, col="red")
lines(prd$dperm, prd$intalls_Leisure, col="blue")
legend("topleft", legend=c("No Leisure", "Leisure"),
       col=c("red", "blue"), lty=1:1, box.lty=0, cex=0.8, y.intersp=0.3)

#Plot the regression lines for Living and non-Living
lines(prd$dperm, prd$intalls_noLiving, col="red")
lines(prd$dperm, prd$intalls_Living, col="blue")
legend("topleft", legend=c("No Living", "Living"),
       col=c("red", "blue"), lty=1:1, box.lty=0, cex=0.8, y.intersp=0.3)

#Plot the regression lines for Productivity and non-Productivity
lines(prd$dperm, prd$intalls_noProductivity, col="red")
lines(prd$dperm, prd$intalls_Productivity, col="blue")
legend("topleft", legend=c("No Productivity", "Productivity"),
       col=c("red", "blue"), lty=1:1, box.lty=0, cex=0.8, y.intersp=0.3)
