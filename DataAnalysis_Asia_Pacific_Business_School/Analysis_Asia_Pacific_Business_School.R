# INSTALL AND LOAD PACKAGES ################################

library(datasets)  # Load base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, knitr) 
p_load(psych)

data= import("~/Desktop/case1.csv")
data
colnames(data) # to get column names of all columns in dataframe#
str(data) # to get datatypes of all columns in dataframe#


# Converting character to numeric datatype
data$`Local Tuition ($)` <- gsub(",", "", data$`Local Tuition ($)`)
data$`Local Tuition ($)` <- as.integer(data$`Local Tuition ($)`)
data$`Foreign Tuitiion ($)` <-gsub(",", "", data$`Foreign Tuitiion ($)`)
data$`Foreign Tuitiion ($)` <- as.integer(data$`Foreign Tuitiion ($)`)
data$`Starting Salary ($)` <- gsub(",", "", data$`Starting Salary ($)`)
data$`Starting Salary ($)` <- as.integer(data$`Starting Salary ($)`)
str(data)

# Question 1: Summary of all the quantitative variables and use a table to show the summary

options("scipen"=100, "digits"=3)
install.packages("pastecs")
# Use the function stat.desc() to compute descriptive statistics
# Compute descriptive statistics
summary_data <- stat.desc(data)
round_summary_data <- round(summary_data, 2)
# Take transpose of the data to get in correct format
tran_summary_data <- t(round_summary_data)
# Loading the data to csv file
write.csv(tran_summary_data,"~/Desktop/summary.csv")


#Question 2 

# a. Summarize difference between local and foreign tuition class
mean(data$`Foreign Tuitiion ($)` - data$`Local Tuition ($)`)
median(data$`Foreign Tuitiion ($)`- data$`Local Tuition ($)`)


# b. Summarize difference between mean starting salaries for schools requiring and not requiring work experience 
mean(data[data$`Work Experience` == "Yes", c(11)])
mean(data[data$`Work Experience` == "No", c(11)])
median(data[data$`Work Experience` == "Yes", c(11)])
median(data[data$`Work Experience` == "No", c(11)])
diff_work_exp <- mean(data[data$`Work Experience` == "Yes", c(11)]) - mean(data[data$`Work Experience` == "No", c(11)])
print(diff_work_exp)


# c. Summarize difference between mean starting salaries for schools requiring and not requiring english test
mean(data[data$`English Test` == "Yes", c(11)])
mean(data[data$`English Test` == "No", c(11)])
median(data[data$`English Test` == "Yes", c(11)])
median(data[data$`English Test` == "No", c(11)])
diff <- mean(data[data$`English Test` == "Yes", c(11)]) - mean(data[data$`English Test` == "No", c(11)])
print(diff)


#Question 3: 

# a. Do starting salaries appear to be related to local tuition? Do starting salaries appear to be related to foreign tuition? Justify using visual and numerical measures. 

#calculate covariance
cov(data$`Local Tuition ($)`,data$`Starting Salary ($)`)
#calculate correlation coefficient 
cor(data$`Local Tuition ($)`,data$`Starting Salary ($)`)
#plot scatter graph
plot(data$`Local Tuition ($)`,data$`Starting Salary ($)`, main="Scatter Plot of Starting Salary vs Local Tuition", xlab = "Local Tuition", ylab = "Starting Salary")  
#add line to plot 
abline(lm(data$`Starting Salary ($)` ~ data$`Local Tuition ($)`))

#b. Do starting salaries appear to be related to foreign tuition?

#Calculate covariance
cov(data$`Foreign Tuitiion ($)`, data$`Starting Salary ($)`)
#calculate correlation coefficient
cor(data$`Foreign Tuitiion ($)`, data$`Starting Salary ($)`)
#plot scatter graph
plot(data$`Foreign Tuitiion ($)`, data$`Starting Salary ($)`, main="Scatter Plot of Starting Salary vs Foreign Tuition", xlab = "Foreign Tuition", ylab = "Starting Salary")  
#add line to plot 
abline(lm(data$`Starting Salary ($)` ~ data$`Foreign Tuitiion ($)`))



#Question 4. Draw a box plot to graphically summarize the starting salaries . Identify the first, second and third quartiles from the boxplot. Are there any outliers? 

# Box plot with details as min, max,1st quartile,2nd quartile,3rd quartile and median

boxplot(data$`Starting Salary ($)`,horizontal=TRUE,col="grey",
        main="Boxplot of Starting Salary",
        xlab="Starting Salary",
        staplewex=1, axes=FALSE)
text(x = boxplot.stats(data$`Starting Salary ($)`)$stats, 
     labels = boxplot.stats(data$`Starting Salary ($)`)$stats,
     y = 1.25,
     box(lty = 'solid', col = 'black'))

# Box plot statistics
cal_data <- boxplot.stats(data$`Starting Salary ($)`)
cal_data$stats
# Upper limit of whisker: Q3+(1.5*IQR)
cal_data$stats[4]+(1.5*(IQR(data$`Starting Salary ($)`)))
# Lower limit of whisker: Q1-(1.5*IQR)
cal_data$stats[2]-(1.5*(IQR(data$`Starting Salary ($)`)))
