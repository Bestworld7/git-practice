#데이터 마이닝 
library(MASS)

setwd("/Users/bestw/Desktop/ex/KNOU/Datemaining")

prod <- read.csv("productivityORG.csv")
prod$date <-as.Date(prod$date,format='%m/%d/%Y')
prod$quarter <- factor(prod$quarter)
prod$department <- factor(prod$department)
prod$day <- factor(prod$day)
prod$team <- factor(prod$team)
summary(prod)
attach(prod)
str(prod)

summary(lm(productivity ~. , data = prod))

par(mfrow=c(3,4))
boxplot(target, col="cyan3", xlab="Target Productivity")
boxplot(smv, col="cyan3", xlab="Standard Minute Value")
boxplot(wip, col="cyan3", xlab="Work in Progress")
boxplot(over_time, col="cyan3", xlab="Overtime")
boxplot(incentive, col="cyan3", xlab="Incentive")
boxplot(idle_time, col="cyan3", xlab="Idle TIme")
boxplot(idle_men, col="cyan3", xlab="Idle Men")
boxplot(numchange, col="cyan3", xlab="Number of Changes in Style")
boxplot(numworkers, col="cyan3", xlab="Number of Workers")
boxplot(productivity, col="cyan3", xlab="Productivity")


dout <- rep(0,15)
dout2 <- rep(0,15)
for (i in 6:15){
  t3 <- quantile(prod[,i], 0.75)
  t1 <- quantile(prod[,i], 0.25)
  tq <- IQR(prod[,i], 0.75)
  dout[i] <- t3 + 1.5*tq
  dout2[i] <- t1 - 1.5*tq
}
outindex <- matrix(0,1197, 15)
for (i in 1:1197)
  for (j in 6:15){
    if (prod[i,j] > dout[j] || prod[i,j] < dout2[j]) outindex[i,j] <- 1
  }
prod2 <- prod[apply(outindex,1,sum)==0,]
prodnew <- prod2[,-c(1,11:13)]
head(prodnew)
write.csv(prodnew,"/Users/bestw/Desktop/ex/KNOU/Datemaining/productivityREG.csv",quote=F,row.names=F)

detach(prod)


##########wine quality pre-rocessing###################

wine <- read.csv("winequalityORG.csv")

attach(wine)
par(mfrow=c(3,4))
boxplot(fixed, col="cyan3", xlab="Fixed Acidity")
boxplot(volatile, col="cyan3", xlab="Volatile Acidity")
boxplot(citric, col="cyan3", xlab="Citric Acid")
boxplot(residsugar, col="cyan3", xlab="Residual Sugar")
boxplot(chlorides, col="cyan3", xlab="Chlorides")
boxplot(freeSD, col="cyan3", xlab="Free Sulfur Dioxide")
boxplot(totalSD, col="cyan3", xlab="Total Sulfur Dioxide")
boxplot(density, col="cyan3", xlab="Density")
boxplot(pH, col="cyan3", xlab="pH")
boxplot(sulphates, col="cyan3", xlab="Sulphates")
boxplot(alcohol, col="cyan3", xlab="Alcohol")

dout <- rep(0,12)
dout2 <- rep(0,12)
for (i in 1:11){
  t3 <- quantile(wine[,i], 0.75)
  t1 <- quantile(wine[,i], 0.25)
  tq <- IQR(wine[,i], 0.75)
  dout[i] <- t3 + 1.5*tq
  dout2[i] <- t1 - 1.5*tq
}
outindex <- matrix(0,1599, 12)
for (i in 1:1599)
  for (j in 1:11){
    if (wine[i,j] > dout[j] || wine[i,j] < dout2[j]) outindex[i,j] <- 1
  }
wine2 <- wine[apply(outindex,1,sum)==0,]
table(wine2$quality)
write.csv(wine2,"winequalityCLASS.csv",quote=F,row.names=F)
detach(wine)

############## dummy #######################
#install.packages("dummy")

library(dummy)
prod =  read.csv("productivityREG.csv")
summary(prod)
prod$quarter = factor(prod$quarter)
prod$department = factor(prod$department)
prod$day = factor(prod$day)
prod$team = factor(prod$team)
summary(prod)

# Generate dummy variables
dvar = c(1:4)
prod2 = dummy(x=prod[,dvar])
prod2 = prod2[,-c(5, 7, 13, 25)]
prod = cbind(prod[,-dvar], prod2)
for(i in 1: ncol(prod)) if(!is.numeric(prod[,i])) prod[,i] = as.numeric(prod[,i])

par(mfrow=c(1,1))

prod

str(prod)
head(prod)

pred <- read.csv("/Users/bestw/Desktop/ex/KNOU/Datemaining/productivityREG.csv", header = TRUE)

pred
head(pred)
str(pred)

colnames(pred)

colnames(pred)[1:4]

coloumns <- colnames(pred)[0:4]

pred$quarter <- factor(pred$quarter)
pred$department <- factor(pred$department)
pred$day = factor(pred$day)
pred$team <- factor(pred$team)


for (col_name in columns) {
  pred[[col_name]] <- factor(pred[[col_name]])
}
  
str(pred)  


fit.all <- lm(productivity ~ . , data = pred)
summary(fit.all)
fit.step <- step(fit.all, direction = 'both')
fit.step$anova
summary(fit.step)

head(fit.all)

pred.reg <- predict(fit.step, newdata = pred, type = 'response')
pred.reg

print(pred.reg)

summary(pred.reg)
mean((pred$productivity ~ pred.reg)^2)

str(pred.reg)

head(pred.reg)



wine <- read.csv("/Users/bestw/Desktop/ex/KNOU/Datemaining/winequalityCLASS.csv", 
                 header = TRUE)
fit.all <- glm(quality~. , family = binomial, data = wine)
summary(fit.all)
fit.step <- step(fit.all, direction = "both")
fit.step$anova
summary(fit.step)

p <- predict(fit.step, newdata = wine, type = "response")
cotoff <- 0.5
yhat = ifelse(p > cotoff, 1,0)

tab = table(wine$quality, yhat, dnn=c("Observed", "Predicted"))
tab
sum(diag(tab))/sum(tab)

par(mfrow=c(1,1))
