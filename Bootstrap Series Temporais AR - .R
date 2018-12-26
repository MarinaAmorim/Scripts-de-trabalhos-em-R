
#######################################
#Bootstrap para Séries Temporais AR
#MArina Amorim
# marinaaamorim@hotmail.com
#######################################

#Pacotes:
#install.packages("boot")
require(boot)
#install.packages("bootstrap")
require(bootstrap)

#Definindo a semente:
set.seed(7)

##Simulando um AR(1) com phi = 0.6 :
amostra<-arima.sim(n=50,list(ar=c(0.6)))

##Ajustar um ARIMA(1,0,0) sem intercepto para saber o phi amostral:
mod<- arima(amostra,order=c(1,0,0),include.mean = FALSE)
mod
phi_amostra <-mod$coef[1]
Tobs<- phi_amostra/sqrt(mod$var.coef[1,1])

##Bootstrap para estimar phi:

B<-1000                      #Repetições bootstrap.
phis_estimados<-rep(NA,B)    #Vetor para guardar as estimativas de phi.
phis_sd<-rep(NA,B)           #Vetor para guardar as estimativas de EP de phi.

res<-mod$residuals           #residuos da série original
n<-length(amostra)

x_star<-0

for (k in 1:B){
  res_star<- sample(res,n,replace=TRUE)
  x_star[1]<- res_star[1]
  
  for (i in 2:n){
    x_star[i]<- x_star[i-1]*phi_amostra+ res_star[i]
    x_star<-x_star[1:n]
  }
  
  mod<-arima(x_star,order=c(1,0,0))
  phis_estimados[k]<- mod$coef[1]
  phis_sd[k]<-sqrt(mod$sigma2)
  
}

mean(phis_estimados)
phi_amostra

#A estimativa Boot ficou melhor.

##IC percentilico
icboot<-quantile(phis_estimados,probs=c(0.25,0.975))
icboot


##Teste T:
count<-0
for (k in 1:B){
  t<-phis_estimados/phis_sd
  if ((abs(t[k]) >= abs(Tobs))) 
    count <- count + 1
}

pvalor <- count/B
pvalor

#Rejeita H0.

######################################################################
