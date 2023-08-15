library(datasets)  # Load base packages manually

# Installs pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load add-on packages as desired
pacman::p_load(pacman, rio, knitr) 
p_load(psych)


data4= import("D:\\BAN 602\\Case Studies\\Case study 4\\BAN_602_Case_4.csv")
colnames(data4)
str(data4)
nT=count(data4)



library(dplyr)  

# White background
wA = filter(data4, Background == "white background", Font == "Ariel")
wC = filter(data4, Background == "white background", Font == "Calibri")
wT = filter(data4, Background == "white background", Font == "Tahoma")
W = data.frame("Background" = wA$Background, "Ariel" = wA$`Time (seconds)`, "Calibri" = wC$`Time (seconds)`, 
               "Tahoma" = wT$`Time (seconds)`)
W1 = rbind(wA, wC, wT)
sum(wA$`Time (seconds)`)

# Green Background
gA = filter(data4, Background == "green background", Font == "Ariel")
gC = filter(data4, Background == "green background", Font == "Calibri")
gT = filter(data4, Background == "green background", Font == "Tahoma")
G = data.frame("Background" = gA$Background, "Ariel" = gA$`Time (seconds)`, "Calibri" = gC$`Time (seconds)`, 
               "Tahoma" = gT$`Time (seconds)`)
G1=  rbind(gA, gC, gT)

# Pink Background
pA = filter(data4, Background == "pink background", Font == "Ariel")
pC = filter(data4, Background == "pink background", Font == "Calibri")
pT = filter(data4, Background == "pink background", Font == "Tahoma")
P = data.frame("Background" = pA$Background, "Ariel" = pA$`Time (seconds)`, "Calibri" = pC$`Time (seconds)`, 
               "Tahoma" = pT$`Time (seconds)`)
P1 = rbind(pA, pC, pT)

maindata = rbind(W, G, P)
write.csv(maindata,"D:\\BAN 602\\Case Studies\\Case study 4\\Casestudy 4.csv")

#Finding the sample means
x_doublebar = mean(data4$`Time (seconds)`) # mean of all observations in experiment
x1._bar = mean(W1$`Time (seconds)`) #sample mean for the observations in treatment 1(White color)->Factor A(color) 
x2._bar = mean(G1$`Time (seconds)`) #sample mean for the observations in treatment 2(Green color)->Factor A(color)
x3._bar = mean(P1$`Time (seconds)`) #sample mean for the observations in treatment 3(Pink color)->Factor A(color)
x.1_bar = mean(maindata$Ariel) #sample mean for the observations in treatment 1(Ariel)->Factor B(Font)
x.2_bar = mean(maindata$Calibri) #sample mean for the observations in treatment 2(Calibri)->Factor B(Font)
x.3_bar = mean(maindata$Tahoma) #sample mean for the observations in treatment 3(Tahoma)->Factor B(Font)


x11_x21_x31_bar = colMeans(matrix(maindata$Ariel, nrow=10))
print(x11_x21_x31_bar) 
x12_x22_x32_bar = colMeans(matrix(maindata$Calibri, nrow=10))
print(x12_x22_x32_bar)
x13_x23_x33_bar = colMeans(matrix(maindata$Tahoma, nrow=10))
print(x13_x23_x33_bar)
mean_comb <- cbind(x11_x21_x31_bar, x12_x22_x32_bar, x13_x23_x33_bar, data.frame(c(x1._bar,x2._bar, x3._bar))) #sample mean for the observations corresponding to the combination 
                                                          #of treatment i (factor A) and treatment j (factor B)#
print(mean_comb)

SST = sum((data4$`Time (seconds)`- x_doublebar)^2) #total sum of squares
print(SST)

a = 3 # number of levels of factor A
b = 3 # number of levels of factor B
r = 10 # number of replications
nT = as.integer (count(data4)) #total no. of observations taken in the experiment
print(nT)

SSA = b*r*(sum((c(x1._bar, x2._bar, x3._bar)-x_doublebar)^2))
print(SSA)

SSB = a*r*(sum((c(x.1_bar, x.2_bar, x.3_bar)-x_doublebar)^2))
print(SSB)

SSAB = 10*((sum((mean_comb$x11_x21_x31_bar - mean_comb$c.x1._bar..x2._bar..x3._bar. - x.1_bar + x_doublebar)^2) + 
        sum((mean_comb$x12_x22_x32_bar - mean_comb$c.x1._bar..x2._bar..x3._bar. - x.2_bar + x_doublebar)^2) +
        sum((mean_comb$x13_x23_x33_bar - mean_comb$c.x1._bar..x2._bar..x3._bar. - x.3_bar + x_doublebar)^2)))
print(SSAB)

SSE = SST - SSA - SSB - SSAB
print(SSE)

anova <- data.frame("Source" = c('FactorA', 'FactorB', 'Interaction', 'Error', 'Total'), "Sum of squares" = c(SSA, SSB, SSAB, SSE, SST),
                "df" = c(a-1,b-1,(a-1)*(b-1), a*b*(r-1), a-1+b-1+(a-1)*(b-1)+a*b*(r-1)))
anova["Mean Square"] <- anova$Sum.of.squares / anova$df


F_A <- (anova$`Mean Square`[1]/anova$`Mean Square`[4]) #F statistic for factor A
F_B <- (anova$`Mean Square`[2]/anova$`Mean Square`[4]) #F statistic for factor B
F_I <- (anova$`Mean Square`[3]/anova$`Mean Square`[4]) #F statistic for Interaction of Factor A and factor B
F_A_critical <- qf(0.05, anova$df[1], anova$df[4], lower.tail = FALSE)
F_B_critical <- qf(0.05, anova$df[2], anova$df[4], lower.tail = FALSE) 
F_I_critical <- qf(0.05, anova$df[3], anova$df[4], lower.tail = FALSE)
p_A <- pf(F_A, anova$df[1], anova$df[4], lower.tail = FALSE)
p_B <- pf(F_B, anova$df[2], anova$df[4], lower.tail = FALSE)
p_I <- pf(F_I, anova$df[3], anova$df[4], lower.tail = FALSE)

anova["F statistic"] <- c(F_A, F_B, F_I, "", "")
anova["p value"] <- c(p_A, p_B, p_I, "", "")
anova["F critical"] <- c(F_A_critical, F_B_critical, F_I_critical, "", "")
write.csv(anova, "D:\\BAN 602\\Case Studies\\Case study 4\\Anova.csv")



