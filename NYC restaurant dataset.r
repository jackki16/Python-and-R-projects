restaurants = read.table("NYC_restaurants.txt")
print(restaurants)
str(restaurants)


plot(restaurants$Food, restaurants$Price, col=c("red", "blue")[restaurants$Location])
plot(restaurants$Decor, restaurants$Price, col=c("red", "blue")[restaurants$Location])
plot(restaurants$Service, restaurants$Price, col=c("red", "blue")[restaurants$Location])
#There is no pattern associated with location. Location might not be the best variable to use for multilinear regression. 

model.full = lm(Price ~ .- Restaurant, data = restaurants)
summary(model.full)
#a. Price(hut symbol)= -21.9558 + 1.538*Food + 1.910*Decor -0.002727*Service -2.068*Location 
#b. When Food, Decor, Service, Location scores are all 0, the predicted price is -29.96. However, this does not make sense in real life as price cannot be negative and other variables cannot be 0.
#When food score increases by 1, price increases by $1.538 when all other variables are kept constant. 
#When decor score increases by 1, price increases by $1.910 when all other variables are kept constant. 
#When service score increases by 1, price decreases by $0.002727 when all other variables are kept constant. 
#When location score increases by 1(change from east to west), price decreases by $2.068 when all other variables are kept constant. 
#c. Yes for food, decor and location, the coefficients have a small p value, which are 1.19e-05,4.96e-05,1.87e-15,0.0304. However, service variable is not significant since 0.99>0.05. 
#d. yes the overall p value is  very small 2.2e-16, so it shows that the overall regression is significant. 
#e. 0.6187. 61.87% variability in price is explained by the multilinear regression model.


model = lm(Price ~ .- Restaurant, data = restaurants)
model

qqnorm(model$residuals)
qqline(model$residuals)
#checking for normality of error

plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for restaurant Dataset")
abline(h = 0, lty = 2)
#checking for independent error and constant variances

#linearity: There is a linear relationship as evident from the r2 value of 0.6279.
#constant variances: The variance of the residual is the same across all values of the x axis. 
#normality of error: On the normal probability plot for the error term, the points are near the line. 
#independent error: There is no correlation between residual values and fitted y values. 


library(car)
vif(model)
#All variables have a small VIF (<5), but service VIF is the highest at 3.559. When service is regressed against other variables, it shows that it is moderately correlated.

#Remove service as variable. 
model.full2 = lm(Price ~ .- Restaurant -Service, data = restaurants)
summary(model.full2)
#The overall model is significant as evident from the small p value is 2.2e-16.

model2 = lm(Price ~ .- Restaurant -Service, data = restaurants)
model2

qqnorm(model2$residuals)
qqline(model2$residuals)
#checking for normality of error

plot(model2$fitted, model2$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for restaurant Dataset")
abline(h = 0, lty = 2)
#checking for independent error and constant variances

#linearity: There is a linear relationship as evident from the r2 value of 0.6279.
#constant variances: The variance of the residual is the same across all values of the x axis. 
#normality of error: On the normal probability plot for the error term, the points are near the line. 
#independent error: There is no correlation between residual values and fitted y values.

vif(model2)
#VIF is significantly reduced for food and decor, location VIF has a small decrease too. 


