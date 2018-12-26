#TRABALHO DE ECONOMETRIA
require(tseries)
require(forecast)
require(fGarch)
require(e1071)
require(xlsx)
#require(rugarch)

setwd("C:\\Users\\Luiza\\Desktop\\UFMG\\VaR")

dados<-read.csv("carteira.csv",header=TRUE,sep=";")
head(dados)

euro<-dados$euro
dolar<-dados$dolar
ibov<-dados$ibov

euro<-as.numeric(gsub(",", ".", gsub("\\.", "", euro)))
dolar<-as.numeric(gsub(",", ".", gsub("\\.", "", dolar)))
ibov<-as.numeric(gsub(",", ".", gsub("\\.", "", ibov)))

n<-length(euro)

euro<-euro[2:n]
dolar<-dolar[2:n]
ibov<-ibov[2:n]

#ANALISE DESCRITIVA DAS SERIES

#Dolar
ts.plot(dolar)

acf(dolar)
pacf(dolar) #Parece nao haver correlação na média.

acf(dolar^2)
pacf(dolar^2)

kurtosis(dolar) #Não deve ser normal. 
summary(dolar)

adf.test(dolar)

#Euro
ts.plot(euro)

acf(euro)
pacf(euro) #Parece nao haver correlação na média.

acf(euro^2)
pacf(euro^2)

kurtosis(euro) #Não deve ser normal. 
summary(euro)

adf.test(euro)

#Ibov
ts.plot(ibov)

acf(ibov)
pacf(ibov) #Parece nao haver correlação na média.

acf(ibov^2)
pacf(ibov^2)

kurtosis(ibov) #Pode ser normal. 
summary(ibov)

adf.test(ibov)
pp.test(ibov)

############################

#MODELAGEM
#com distribuição Normal

#Dolar
m1<-auto.arima(dolar)
m1
#Não precisa de modelar a media.

m2<-garchFit(dolar~garch(1,1))
summary(m2)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.125236 1.136559 1.125228 1.129396 

#Euro
m3<-auto.arima(euro)
m3

m4<-garchFit(euro~garch(1,1))
summary(m4)
plot(m4)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.125236 1.136559 1.125228 1.129396 

m4@fit$coef
m2@fit$coef

m2
m4

#Ibov
m5<-auto.arima(ibov)
m5

m6<-garchFit(ibov~garch(1,1))
summary(m6)

m<-garch(ibov,order=c(1,1))
summary(m)


plot(m6)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.125236 1.136559 1.125228 1.129396 

##################
#Ajustes com distribuição T

#Dolar
m7<-garchFit(dolar~garch(1,1),include.mean = FALSE,cond.dist = "std")
summary(m7)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.006546 1.017868 1.006537 1.010706 

plot(m7)

#Euro
m8<-garchFit(euro~garch(1,1),include.mean = FALSE,cond.dist = "std")
summary(m8)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.006546 1.017868 1.006537 1.010706 

plot(m8)

#Ibovespa
m8<-garchFit(euro~garch(1,1),include.mean = FALSE,cond.dist = "std")
summary(m8)
#Information Criterion Statistics:
#  AIC      BIC      SIC     HQIC 
#1.006546 1.017868 1.006537 1.010706 

plot(m8)
predict(m8,5)


#Teste de Kolmogorov Smirnov para aderencia da T, em que shape é aproximação
#dos gl.

# arrumar erro do pacote.
