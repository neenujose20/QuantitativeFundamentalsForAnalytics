#Question 1


library(ggplot2)
mu <- 20000
x10 <- 10000
x30 <- 30000
Z30 <- qnorm(0.975) # z-value for 30,000 units and probability of 0.975
Z30
sd <- (x30 - mu)/Z30
sd
x <- seq(mu-4*sd, mu+4*sd, by = 0.1)
y <- dnorm(x, mean = mu, sd = sd)
plot(x,y, type = "l", main = "Normal Probability Distribution", col = "blue", 
     lwd = 2, xlab = "Demand in units (Mean = 20000)", ylab= "")
polygon(x, y,col = 'green')


x=seq(10000,30000,by = 0.1)
y=dnorm(x, mean = 20000, sd = sd)
polygon(c(10000,x,30000),c(0,y,0),col="Grey")
########################################################################################

#Question 2
order_quantities <- c(15000, 18000, 24000, 28000) 
z <-  (order_quantities - mu)/sd
p <-  pnorm(z)
p_stockout <-  1-p
result <-  data.frame(order_quantities, z, p, p_stockout)
print(result)
write.csv(result, "D:\\BAN 602\\Case Studies\\Case study 2.csv")
########################################################################################


#Question 3

sp <-  24 # Selling price of the toys in seasonal demand = $24/unit
cp <-  16 # Cost price of the toy = $16/unit
sp_surplus <- 5 # Selling price of the left -out toys in December = $5/unit
sales = c(10000, 20000, 30000)



#Scenario-1: sales= 10000
result1 <- data.frame("order_quantities" = result$order_quantities, "sales" = sales[1], 
                      "costprice" = result$order_quantities * cp, 
                      "surplus" = result$order_quantities - sales[1])

result1$sellingprice = result1$sales*sp +(result1$surplus)*sp_surplus
result1$profit_loss = result1$sellingprice - result1$costprice
print(result1)
write.csv(result1, "D:\\BAN 602\\Case Studies\\CaseStudy2.csv")

#Scenario-2 : sales = 20000
result2 <- data.frame("order_quantities" = result$order_quantities, "sales" = sales[2], 
                      "costprice" = result$order_quantities * cp, 
                      "surplus" = result$order_quantities - sales[2])
result2$surplus[1:2] = 0 
result2$sellingprice[1:2] = result2$order_quantities[1:2]*sp
result2$sellingprice[3:4] <- result2$sales[3:4]*sp+(result2$surplus[3:4])*sp_surplus
result2$profit_loss <- result2$sellingprice - result2$costprice
print(result2)
write.csv(result2, "D:\\BAN 602\\Case Studies\\Casestudy 2.csv")

#Scenario-3: Sales=30000
result3 <- data.frame("order_quantities" = result$order_quantities, "sales" = sales[3], 
                      "costprice" = result$order_quantities * cp, 
                      "surplus" = result$order_quantities - sales[3]
)
result3$surplus= 0 # Stock-out scenario
result3$sellingprice <- result3$order_quantities*sp
result3$profit_loss <- result3$sellingprice - result3$costprice
print(result3)
write.csv(result3, "D:\\BAN 602\\Case Studies\\Casestudy2.csv")
########################################################################################


#Question 4
Z = qnorm(0.7)
qty = as.integer(sd*Z + mu)
print(Z)
print(qty)
result4 <- data.frame(qty, sales, "costprice" = qty * cp,
                      "surplus" = qty - sales)
result4$surplus[3] = 0 #stock-out scenario  
result4$sellingprice[3] <- result4$qty[3] * sp
result4$sellingprice[1:2] <- result4$sales[1:2] * sp + result4$surplus[1:2] * sp_surplus
result4$profit_loss = result4$sellingprice - result4$costprice
print(result4)
write.csv(result4, "D:\\BAN 602\\Case Studies\\casestudytoys.csv")  
########################################################################################


#Question5
P <- 8/(8+5)
P
Z_p <- qnorm(P) # z-value for 30,000 units and probability of 0.975
Z_p
X= as.integer(Z_p * sd + mu)
X
qty1 <- X
result5 <- data.frame(qty1, sales, "costprice" = qty1 * cp,
                      "surplus" = qty1 - sales)
result5$surplus[3] = 0 #stock-out scenario
result5$sellingprice[3] <- result5$qty1[3] * sp
result5$sellingprice[1:2] <- result5$sales[1:2] * sp + result5$surplus[1:2] * sp_surplus
result5$profit_loss = result5$sellingprice - result5$costprice
print(result5)
########################################################################################

