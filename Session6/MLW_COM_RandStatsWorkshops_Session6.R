## ----setup, include=FALSE, echo=F--------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.width=16, fig.height=9, dpi=150, highlight=T, collapse=T)

require(tidyverse)
require(knitr)
require(gridExtra)
require(rms)


## ---- eval=F-----------------------------------------------------------
## install.packages(c("nlme","lme4","gee","rms","car","Hmisc")


## ---- warning=F, message=F---------------------------------------------
library(nlme)
library(lme4)
library(gee)
library(rms)
library(Hmisc)


## ----------------------------------------------------------------------
ado<-read.csv("adolescent_small.csv")

modWeightInt<-glm(a104wt~a12age*hiv,data=ado)
summary(modWeightInt)$coefficients


## ----------------------------------------------------------------------
ado$tmpVar<-ado$a103ht+rnorm(nrow(ado),sd=6)

modWeightColl<-glm(a104wt~as.factor(a13sex)+a12age+a103ht+tmpVar,data=ado)
car::vif(modWeightColl)


## ----------------------------------------------------------------------
x<-rnorm(100,mean=5,sd=2)
y<-2+x-2*x^2+0.2*x^3+rnorm(100,sd=5)
df<-data.frame(x,y)
rm(x,y)

modLsp<-glm(y~lsp(x,c(3,6)),data=df)
 # lsp() requires rms package

newdf<-data.frame(x=df$x,predict.lm(modLsp,interval="confidence"))


## ----------------------------------------------------------------------
df %>%
  ggplot(mapping=aes(x=x,y=y)) +
  geom_point(size=3) +
  geom_line(data=newdf,mapping=aes(x=x,y=fit),col="orange",lwd=2) +
  geom_ribbon(data=newdf,mapping=aes(y=fit,ymin=lwr,ymax=upr),alpha=0.2) +
  geom_vline(xintercept=3,lty=2,col="darkgrey") +
  geom_vline(xintercept=6,lty=2,col="darkgrey") 


## ----------------------------------------------------------------------
modRcs<-glm(y~rcs(x,nk=3),data=df)
  # rcs requires rms package

newdf<-data.frame(x=df$x,predict.lm(modRcs,interval="confidence"))


## ----------------------------------------------------------------------
df %>%
  ggplot(mapping=aes(x=x,y=y)) +
  geom_point() +
  geom_line(col="orange",lwd=2,mapping=aes(x=x,y=predict(modRcs))) +
  geom_line(data=newdf,mapping=aes(x=x,y=fit),col="orange",lwd=2) +
  geom_ribbon(data=newdf,mapping=aes(y=fit,ymin=lwr,ymax=upr),alpha=0.2)


## ----------------------------------------------------------------------
chol<-read.csv("choldiet.csv")

t.test(chol$CORNFLK_mmolPerL,chol$OATBRAN_mmolPerL,paired=F)


## ----------------------------------------------------------------------
t.test(chol$CORNFLK_mmolPerL,chol$OATBRAN_mmolPerL,paired=T)


## ----------------------------------------------------------------------
t.test(chol$CORNFLK_mmolPerL-chol$OATBRAN_mmolPerL,mu=0)


## ----------------------------------------------------------------------
wilcox.test(chol$CORNFLK_mmolPerL,chol$OATBRAN_mmolPerL,paired=T)


## ----------------------------------------------------------------------
wilcox.test(chol$CORNFLK_mmolPerL-chol$OATBRAN_mmolPerL,mu=0)


## ----------------------------------------------------------------------
apprPres<-matrix(c(794, 86, 150, 570),
       nrow = 2,
       dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                       "2nd Survey" = c("Approve", "Disapprove")))
apprPres



## ----------------------------------------------------------------------
mcnemar.test(apprPres)


## ---- eval=F-----------------------------------------------------------
## orthModLme<-lme(distance~age+Sex,
##                 random=~1|Subject,
##                 data=Orthodont)
## summary(orthModLme)


## ---- eval=F-----------------------------------------------------------
## orthModLmer<-lmer(distance~age+Sex+(1|Subject),
##                   data=Orthodont)
## summary(orthModLmer)


## ---- echo=F, collapse=T-----------------------------------------------
orthModLme<-lme(distance~age+Sex,
                random=~1|Subject,
                data=Orthodont)
summary(orthModLme)


## ---- echo=F, collapse=T-----------------------------------------------
orthModLmer<-lmer(distance~age+Sex+(1|Subject),
                  data=Orthodont)
summary(orthModLmer)


## ---- eval=F-----------------------------------------------------------
## sleepModLme<-lme(Reaction~Days,
##                  random=~Days|Subject,
##                  data=sleepstudy)
## summary(sleepModLme)


## ---- eval=F-----------------------------------------------------------
## sleepModLmer<-lmer(Reaction~Days+(Days|Subject),
##                    data=sleepstudy)
## summary(sleepModLmer)


## ---- echo=F, collapse=T-----------------------------------------------
sleepModLme<-lme(Reaction~Days,
                 random=~Days|Subject,
                 data=sleepstudy)
summary(sleepModLme)


## ---- echo=F, collapse=T-----------------------------------------------
sleepModLmer<-lmer(Reaction~Days+(Days|Subject),
                   data=sleepstudy)
summary(sleepModLmer)


## ----------------------------------------------------------------------
orthModGee<-gee(distance~age+Sex,
                 id=Subject,
                 data=Orthodont)
summary(orthModGee)

