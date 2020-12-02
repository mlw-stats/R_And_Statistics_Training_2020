## ----setup, include=FALSE, echo=F----------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=16, fig.height=9, dpi=150, highlight=T, collapse=T)

require(tidyverse)
require(knitr)
require(gridExtra)


## ------------------------------------------------------------------------------------------
Tbreg<-read.csv("btTBreg.csv")
Tbreg<-Tbreg %>% mutate(cd4change=cd42-cd41)
# TB dataset used previously

bw<-read.csv("https://www.sheffield.ac.uk/polopoly_fs/1.886038!/file/Birthweight_reduced_R.csv")
# Data contributed by Ellen Marshall, University of Sheffield

titanic<-read.csv("titanic.csv")
titanic<-titanic[,-1]
# data on survivors and casualties of the Titanic disaster


## ------------------------------------------------------------------------------------------
df<-tibble(
  x=runif(25,min=-5,max=5),
  y=1.5*x+rnorm(25,sd=2)+3.5
)

ggplot(data=df,aes(x=x,y=y)) + 
  geom_point(size=3) +
  theme(text = element_text(size=20)) 


## ------------------------------------------------------------------------------------------
ggplot(data=df,aes(x=x,y=y)) + 
  geom_abline(intercept=3,slope=1,colour="steelblue",lwd=1.5) +
  geom_point(size=3) +
  theme(text = element_text(size=20)) 


## ------------------------------------------------------------------------------------------
ggplot(data=df,aes(x=x,y=y)) + 
  geom_abline(intercept=4,slope=2,colour="salmon",lwd=2) +
  geom_point(size=3) +
  theme(text = element_text(size=20)) 


## ------------------------------------------------------------------------------------------
ggplot(data=df,aes(x=x,y=y)) + 
  geom_abline(intercept=3,slope=0.5,colour="darkgrey",lwd=2) +
  geom_abline(intercept=3.1,slope=1,colour="mediumorchid",lwd=2) +
  geom_abline(intercept=3.8,slope=1.25,colour="orange",lwd=2) +
  geom_abline(intercept=2.8,slope=1.75,colour="steelblue",lwd=2) +
  geom_abline(intercept=3.5,slope=2,colour="salmon",lwd=2) +
  geom_abline(intercept=4,slope=2.5,colour="greenyellow",lwd=2) +
  geom_point(size=3) +
  theme(text = element_text(size=20)) 


## ------------------------------------------------------------------------------------------
ggplot(data=df,aes(x=x,y=y)) + 
  geom_abline(intercept=3,slope=1,colour="steelblue",lwd=2) +
  geom_segment(aes(x=x,xend=x,y=y,yend=x+3),colour="red",lwd=1.5) +
  geom_point(size=4) +
  theme(text = element_text(size=20)) 


## ------------------------------------------------------------------------------------------
sumSquares<-function(beta,dat=df){
  return(
    sum( (dat$y - (beta[1]+beta[2]*dat$x))^2 )
  )
}


## ------------------------------------------------------------------------------------------
sumSquares(c(0,0))
sumSquares(c(0,1))
sumSquares(c(2,1))
sumSquares(c(3,1.5))


## ------------------------------------------------------------------------------------------
betaHat<-optim(fn=sumSquares,par=c(0,0))
print(betaHat$par)
print(betaHat$value)


## ------------------------------------------------------------------------------------------
mod<-lm(y~x,data=df)

print(mod)


## ---- collapse=T---------------------------------------------------------------------------
modAlt<-glm(y~x,data=df,family=gaussian("identity"))

summary(modAlt)


## ------------------------------------------------------------------------------------------
modPois<-glm(dist~speed,data=cars,family=poisson)
newX<-data.frame(speed=seq(1,30,length=500))
pred<-predict(modPois,type="link",newdata=newX, se.fit=T)
predFit<-exp(pred$fit)
predLow<-exp(pred$fit-qnorm(0.975)*pred$se.fit)
predHigh<-exp(pred$fit+qnorm(0.975)*pred$se.fit)

plot(dist~speed,data=cars,cex=2,xlim=c(1,30),ylim=c(0,170))
lines(newX$speed,predFit,lwd=2,col="steelblue")
polygon(x=c(newX$speed,newX$speed[nrow(newX):1]),y=c(predLow,predHigh[nrow(newX):1]),col=rgb(70,130,180,alpha=100,maxColorValue=255),border=NA)


## ------------------------------------------------------------------------------------------
set.seed(20190718)
x<-rnorm(50)
y<-1.5*x+rnorm(50)
df<-data.frame(x=x,y=y)
mod<-lm(y~x,data=df)
xx<-seq(-3,3,length=500)
predMean<-as.data.frame(predict(mod,newdata=data.frame(x=xx),interval="confidence"))
predNew<-as.data.frame(predict(mod,newdata=data.frame(x=xx),interval="prediction"))


## ------------------------------------------------------------------------------------------
plot(y~x,data=df,cex=2)
lines(c(-3,3),coef(mod)[1]+coef(mod)[2]*c(-3,3),lwd=2,col="steelblue")
polygon(c(xx,xx[length(xx):1]),c(predNew$lwr,predNew$upr[length(xx):1]),border=NA,col=rgb(200,0,0,alpha=80,maxColorValue=255))
polygon(c(xx,xx[length(xx):1]),c(predMean$lwr,predMean$upr[length(xx):1]),border=NA,col=rgb(120,200,0,alpha=130,maxColorValue=255))
lines(c(-3,3),coef(mod)[1]+coef(mod)[2]*c(-3,3),lwd=2,col="steelblue")


## ------------------------------------------------------------------------------------------
# ungrouped
modLowBirthWeight<-glm(as.factor(lowbwt)~Gestation,data=bw,family=binomial)
 # family=binomial("logit") also works
#summary(mod4)

round(digits=2,cbind(
  exp(coef(modLowBirthWeight)),
  exp(confint(modLowBirthWeight))
))


## ---- collapse=T---------------------------------------------------------------------------
# grouped
modTitanic<-glm(cbind(survivors, dead) ~ class + age + sex + class*sex,
                data = titanic,
                family = binomial("logit"))

round(digits=2,cbind(
  exp(coef(modTitanic)),
  exp(confint(modTitanic))
))


## ------------------------------------------------------------------------------------------
# empirical survival probabilities
titanic$empSurvP<-round(digits=4,titanic$survivors/(titanic$survivors+titanic$dead))

# model survival probabilities
linearPredictor<-predict(modTitanic,type="link")
titanic$modSurvP<-round(digits=2,exp(linearPredictor)/(1+exp(linearPredictor)))


## ------------------------------------------------------------------------------------------
print(titanic)


## ------------------------------------------------------------------------------------------
library(pscl) # install.packages("pscl")
data(prussian) # data on deaths from horse kicks in the Prussian army (famous example of a Poisson process)

modPrus<-glm(y~year+as.factor(corp),data=prussian,family=poisson)
  # family=poisson(link="log") also works


## ------------------------------------------------------------------------------------------
modBW_Mppwt<-glm(Birthweight~mppwt,data=bw)
modBW_GestMppwt<-glm(Birthweight~Gestation+mppwt,data=bw)


## ------------------------------------------------------------------------------------------
  ggplot(data=bw,mapping=aes(x=mppwt,y=Birthweight)) +
    geom_point() +
    geom_abline(intercept=coef(modBW_Mppwt)[1],
                slope=coef(modBW_Mppwt)[2],
                col="steelblue",
                lwd=2) +
    xlab("mother's pre-pregnancy weight (lbs)") +
    ylab("neonate birthweight (lbs)")


## ------------------------------------------------------------------------------------------
ggplot(data=bw,mapping=aes(x=mppwt,y=Birthweight)) +
  geom_point()+
  geom_smooth(method="lm")


## ------------------------------------------------------------------------------------------
rmodBW<-residuals(modBW_Mppwt)
theoQ<-qnorm(order(order(rmodBW))/length(rmodBW)) # calculates theorectical normal quantiles
plot(theoQ,rmodBW,
     xlab="theoretical normal quantiles",
     ylab="sample quantiles",
     main="QQ plot")
qqline(rmodBW) # just adds the line


## ------------------------------------------------------------------------------------------
qqnorm(rmodBW,
     xlab="theoretical normal quantiles",
     ylab="sample quantiles",
     main="QQ plot")
qqline(rmodBW)


## ------------------------------------------------------------------------------------------
plot(predict(modBW_Mppwt,data=bw),residuals(modBW_Mppwt),
     xlab="fitted values",
     ylab="residuals")


## ------------------------------------------------------------------------------------------
plot(modBW_Mppwt)


## ------------------------------------------------------------------------------------------
plot(hatvalues(modBW_Mppwt),cex=2)


## ------------------------------------------------------------------------------------------
plot(cooks.distance(modBW_Mppwt),cex=2)


## ---- message=F, warnings=F----------------------------------------------------------------
library(car)
influencePlot(modBW_Mppwt)

