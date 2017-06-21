# trabalho 2  analise de sobrevivencia
# modelo de cox

setdw("C:/Users/marin/Downloads/2017-1/Analise de Sobrevivencia")

#install.packages("survival")]
require(survival)
dados.0 <- read.table("dados.original.txt", header = T)
dados<- na.omit(dados.0[,-10]) #Retirar Mitose e NA
head(dados)
attach(dados)
#detach(dados)
# Transformando a variavel censura em 0 ou 1 
METASTASE<-ifelse(METASTASE==2,0,1)
table(METASTASE) #202 censuras, 36 eventos.

################################################
names(dados)
# modelo nulo :
fit<-coxph(Surv(TEMPOACOMP,METASTASE)~1, data=dados,
            method="breslow")
summary(fit)


#genero
fit1<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(GENERO), data=dados,
             method="breslow")
summary(fit1)
fit1$loglik
trv <- 2*(fit1$loglik[2]-fit$loglik[1])
1-pchisq(trv, length(fit$coefficients)-length(fit1$coefficients))


# grupo idade
fit2<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(GRUPOIDADE), data=dados,
            method="breslow")
summary(fit2)

# localmmpr
fit3<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(LOCALMMPR), data=dados,
            method="breslow")
summary(fit3)

# tipo histologico
fit4<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(TIPOHE), data=dados,
            method="breslow")
summary(fit4)

# breslow
fit5<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(CATBRESLOW), data=dados,
            method="breslow")
summary(fit5)


# ulceração
fit6<-coxph(Surv(TEMPOACOMP,METASTASE)~factor(ULCERHE), data=dados,
            method="breslow")
summary(fit6)

### Variaveis significativas
# rejeita : genero

# nao rejeita : 

