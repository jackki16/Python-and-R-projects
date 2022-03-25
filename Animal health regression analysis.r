library(MASS)
data(cats, package="MASS")
head(cats)

plot(x=cats$Bwt, y=cats$Hwt, main="Scatter diagram", xlab="bodyweight", ylab="heart weight")
#Yes it would be a good fit there is a linear relationship between body weight and heart weight


model = lm(Hwt ~ Bwt, data = cats)      
model 
summary(model)

b1 = sum((cats$Bwt - mean(cats$Bwt)) * (cats$Hwt - mean(cats$Hwt))) /
  sum((cats$Bwt - mean(cats$Bwt))^2)
b1

b0 = mean(cats$Hwt) - b1*mean(cats$Bwt)
b0

sse <- sum((fitted(model) - cats$Hwt)^2)
sse

# the regression equation is y= -0.357 +4.03x
#b. As body weight increases by 1, the heart weight increases by 4.03 times. 
#c. Yes the RSE is 1.45 which shows a relatively small deviation from the regression line and the p value is < 2.2e-16. 
#d. Yes the overall regression is significant because approximately 65% of the variability in the heart weight variable can be explained by the weight variable.
#d. If the coefficient is significant, it would mean that the overall regression would be significant also. 
#e. RSE=1.452, MSE= 1.452/143=0.0102 , SSE= 299.53
#f. 0.6466^2= 0.418



plot(x=cats$Bwt, y=cats$Hwt, main="Scatter diagram", xlab="bodyweight", ylab="heart weight")
abline(b0, b1, lty = 2)
abline(a=b0, b=b1, lty = 2, col="red")


confint(model)
#95% confident that the confidence interval contains the population mean for the specified values of the Bwt in the model.

#linearity: There is a linear relationship as evident from the r2 value of 0.6466
#constant variances: The variance of the residual is the same across all values of the x axis. 
#normality of error: On the normal probability plot for the error term, the points are near the line. 
#independent error: There is no correlation between residual values and fitted y values. 




qqnorm(model$residuals)
qqline(model$residuals)
#checking for normality of error

plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for cats Dataset")
abline(h = 0, lty = 2)
#checking for independent error and constant variances

newdata = data.frame(Bwt = c(2.8, 5, 10)) 
predict(model, newdata, interval = "confidence")
#Just because the two variables are highly correlated, it does not mean that Bwt is the cause of Hwt.

log_bwt <- log10(cats$Bwt)
log_hwt <- log10(cats$Hwt)


df1 = data.frame(log_bwt,cats$Hwt) 
head(df1)

df2 = data.frame(cats$Bwt,log_hwt) 
head(df2)

df3= data.frame(log_bwt,log_hwt) 
head(df3)

# model = lm(Hwt ~ Bwt, data = cats)      
# model 
# summary(model)

model = lm(log_bwt ~ cats.Hwt, data = df1)      
model 
summary(model)

qqnorm(model$residuals)
qqline(model$residuals)

plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for cats Dataset")
abline(h = 0, lty = 2)

model = lm(cats.Bwt ~ log_hwt, data = df2)      
model 
summary(model)

qqnorm(model$residuals)
qqline(model$residuals)

plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for cats Dataset")
abline(h = 0, lty = 2)

model = lm(log_bwt ~ log_hwt, data = df3)      
model 
summary(model)

qqnorm(model$residuals)
qqline(model$residuals)

plot(model$fitted, model$residuals,
     xlab = "Fitted Values", ylab = "Residual Values",
     main = "Residual Plot for cats Dataset")
abline(h = 0, lty = 2)

#cats.Bwt ~ log_hwt produces the highest R squared value, which would mean that the model is best able to predict the variable. 
