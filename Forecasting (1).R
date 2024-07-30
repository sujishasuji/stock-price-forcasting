library(readxl)
library(ggplot2)
library(tidyverse)
library(rstatix)
library(forecast)
library(nnfor)
library(neuralnet)
library(thief)
library(doParallel)
library(forecastHybrid)
library(readxl)
library(tseries)
library(rugarch)
library(fGarch)
library(aTSA)
library(car)
library(FinTS)


A <- read_excel("new 15.xlsx")

##### Log Return #####
LR=function(x){
  lr={}
  n=length(x)
  for(i in 2:(n-1))
    lr[i-1]=log(x[i])-log(x[i-1])
  return(lr)
}

#### Prediction ####
Pred=function(v,f){
  p1=rep(0,52)
  ex=exp(f)
  p1[1]=v*ex[1]
  for(i in 2:52){
    p1[i]=p1[i-1]*ex[i]
  }
  return(p1)
}


#### Central ####
P={}
for(i in 2:16){
  x1=A[,i]
  x1=unlist(as.vector(x1))
  v=x1[435]
  x1=LR(x1)
  g= ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,1)))
  m =ugarchfit(spec=g, data=x1,out.sample=52)
  m1=ugarchforecast(m, n.ahead = 52, n.roll = 0)
  f=as.numeric(m1@forecast$seriesFor)
  p=Pred(v,f)
  P=cbind(P,p)
}

colnames(P)=colnames(A)[2:16]
P


