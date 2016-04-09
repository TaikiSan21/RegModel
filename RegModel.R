
##You work for Motor Trend, a magazine about the automobile industry. Looking at a data 
## set of a collection of cars, they are interested in exploring the relationship between
## a set of variables and miles per gallon (MPG) (outcome). They are particularly 
## interested in the following two questions:
      
## “Is an automatic or manual transmission better for MPG”
## "Quantify the MPG difference between automatic and manual transmissions"
library(datasets)
data(mtcars)
cor(mtcars)
pairs(mtcars)


fit1 <- lm(mpg~hp+am, mtcars)
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)

anova(fit1,lm(mpg~hp*am,mtcars))[6]
anova(fit1,lm(mpg~hp+am+cyl,mtcars))[6]
anova(fit1,lm(mpg~hp+am+carb,mtcars))[6]
anova(fit1,lm(mpg~hp+am+vs,mtcars))[6]
anova(fit1,lm(mpg~hp+am+disp,mtcars))[6]
anova(fit1,lm(mpg~hp+am+wt,mtcars))[6]
anova(fit1,lm(mpg~hp+am+gear,mtcars))[6]
anova(fit1,lm(mpg~hp+am+drat,mtcars))[6]

fit2<-lm(mpg~hp+am+wt,mtcars)
par(mfrow=c(2,2))
plot(fit2)
summary(fit1)
summary(fit2)
## fit2 increased SE of am so that it is no longer sign. 

g<- ggplot(data=mtcars, aes(y=mpg,x=hp)) +
      geom_abline(intercept=26.58, slope=-.0589) +
      geom_abline(intercept=31.86, slope=-.0589,colour='blue') +
      geom_point(aes(colour=am),size=4)
g
