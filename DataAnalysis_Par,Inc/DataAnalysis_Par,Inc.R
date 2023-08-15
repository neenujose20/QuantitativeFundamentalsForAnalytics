#Question 1

library(rcompanion)
library(readr)
library(datasets)  # Load base packages manually
mydata= import("C:\\Users\\valli\\Desktop\\BAN_602_Case_3.csv")
summary(mydata$Current)
summary(mydata$New)
var(mydata$Current)
var(mydata$New)
sqrt(var(mydata$Current))
sqrt(var(mydata$New))

#boxplot
boxplot(mydata)
boxplot(mydata, main="Boxplot of current and new golf balls")
-----------------------------------------------------------------
#Question 2
  
diff = mydata$Current - mydata$New
diff_mean = mean(diff)

t.test(mydata$Current, mydata$New, 
       paired = F, 
       conf.level = 0.95, 
       alternative = "t")
------------------------------------------------------------------------
#Question 3

#to compute a 95% confidence interval for the true population mean weight of the data.

#Calculation for Current data
mean(mydata$Current)     #mean calculation for Current data
sd(mydata$Current)       #standard deviation for Current data
length(mydata$Current)   #Sample size for Current data

# calculate the margin of error for Current data
margin <- qt(0.975,df=length(mydata$Current)-1)*sd(mydata$Current)/sqrt(length(mydata$Current))
cat("margin of error is:" , margin)

#lower interval for Current data
lowerinterval <- mean(mydata$Current)  - margin
cat("lowerinterval for Current data is:",  lowerinterval)

#upper interval for Current data
upperinterval <- mean(mydata$Current) + margin
cat("Upperinterval for Current data is:",  upperinterval)
 
#Calculation for New data
mean(mydata$New)         #mean calculatiobn for New data
sd(mydata$New)           #standard deviation for New data
length(mydata$New)       #Sample size for New data

# calculate the margin of error for New data
margin <- qt(0.975,df=length(mydata$New)-1)*sd(mydata$New)/sqrt(length(mydata$New))
cat("margin of error is:" , margin)

#lower interval for New data
lowerinterval <- mean(mydata$New)  - margin
cat("lowerinterval for New data is:",  lowerinterval)

#upper interval for New data
upperinterval <- mean(mydata$New) + margin
cat("Upperinterval for New data is:",  upperinterval)

#Calculation for difference between the means of the two population data
mean(diff)         #mean calculatiobn for mean data
sd(diff)           #standard deviation for mean data
length(diff)       #Sample size for mean data

# calculate the margin of error for mean data
margin <- qt(0.975,df=length(diff)-1)*sd(diff)/sqrt(length(diff))
cat("margin of error is:" , margin)

#lower interval for New data
lowerinterval <- mean(diff)  - margin
cat("lowerinterval for New data is:",  lowerinterval)

#upper interval for New data
upperinterval <- mean(diff) + margin
cat("Upperinterval for New data is:",  upperinterval)

t.test(diff, 
       paired = F, 
       conf.level = 0.95, 
       alternative = "t")
