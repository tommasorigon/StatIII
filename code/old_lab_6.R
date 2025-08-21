

###############
#STATISTICA III
###############

#____________________________________
#     LEZIONE 4: binomial regression
#____________________________________


#---------------------------------------
#Terzo esempio: confronto tra i link
#---------------------------------------

#data set bliss: numero di insetti morti vs diverse concentrazioni di veleno
library("faraway")
data(bliss)
bliss

mod.logit<-glm(cbind(dead,alive)~conc,family=binomial,data=bliss)
mod.probit<-glm(cbind(dead,alive)~conc,family=binomial(link=probit),data=bliss)
mod.cloglog<-glm(cbind(dead,alive)~conc,family=binomial(link=cloglog),data=bliss)

mod.logit
mod.probit
mod.cloglog

#in base ad AIC probit e' il migliore, ma logit e' molto simile

#calcoliamo anche X^2 di Pearson 
(X2.logit<-sum(residuals(mod.logit,type="pearson")^2))
(X2.probit<-sum(residuals(mod.probit,type="pearson")^2))
(X2.cloglog<-sum(residuals(mod.cloglog,type="pearson")^2))

#HOMEWORK: test per la bonta' di ciascun modello. La conclusione e' affidabile?


#R^2 di Naglekerke 
#------------------
#ci servono dev nulla, dev modello e n (numero di osservazioni binarie:n=somma di ni)
names(mod.logit)
mod.logit$dev
mod.logit$null

(n<-sum(bliss$dead+bliss$alive))

R2.NAG<-function(modello,n){
  num<-(1-exp((modello$dev-modello$null)/n))
  den<-(1-exp((-modello$null)/n))
  round(num/den,3)
}

R2.NAG(mod.logit,n)
R2.NAG(mod.probit,n)
R2.NAG(mod.cloglog,n)

#serve per il confronto tra modelli annidati (non con link differente)

#Confrontiamo i valori previsti dai 3 modelli
#---------------------------------------------

#confrontiamo le probabilita` stimate
round(cbind(mod.logit$fitted,mod.probit$fitted,mod.cloglog$fitted),3)
#mod$fitted e' equivalente a predict(mod,type="response")

#confrontiamo le frequenze stimate (numero di successi attesi)
(ni<-bliss$dead+bliss$alive)
freq.attese<-cbind(mod.logit$fitted,mod.probit$fitted,mod.cloglog$fitted)*ni
colnames(freq.attese)<-c("logit","probit","cloglog")
cbind(freq.attese,bliss$dead)


#sembrano piuttosto simili. Graficamente
x<-seq(0,4,0.1)
#x<-seq(-2,8,0.1)   #valori della esplicativa piu' estremi di quelli osservati/pianificati 
eta.logit<-coef(mod.logit)[1]+coef(mod.logit)[2]*x
eta.probit<-coef(mod.probit)[1]+coef(mod.probit)[2]*x
eta.cloglog<-coef(mod.cloglog)[1]+coef(mod.cloglog)[2]*x
mu.logit<-ilogit(eta.logit)
mu.probit<-pnorm(eta.probit)
mu.cloglog<-1-exp(-exp(eta.cloglog))
plot(x,mu.logit,type="l",xlab="conc",ylab="prob")
lines(x,mu.probit,col=2)
lines(x,mu.cloglog,col=3)
points(bliss$conc,bliss$dead/ni)
#abline(v=0,lty=3)
#abline(v=4,lty=3)

#sul data set osservato e' difficile distinguere logit vs probit, ma le 
#previsioni possono differire anche molto, soprattutto sui valori piu' estremi.



#---------------------------------
#Quarto esempio: sovradispersione
#---------------------------------

library(faraway)
data(troutegg)
help(troutegg)
str(troutegg)
View(troutegg)

#tabelle a doppia entrata
xtabs(cbind(survive,total) ~location+period,troutegg)
#anziche' 2 tabelle a doppia entrata una sola tabella
ftable(xtabs(cbind(survive,total) ~location+period,troutegg))  

mod1<-glm(cbind(survive,total-survive)~location+period,family=binomial,troutegg)
summary(mod1)

model.matrix(mod1)   #come sono state codificate le dummies

#interpretazione: esempio beta_location2. beta.hat= -0.4168=log odds ratio
exp(-0.4168)  #circa 0.66 stima dell'odds ratio
#passando da location 1 a location 2, e fermo restando tutto il resto,
#l'odds relativo alla prob. di sopravvivenza
#diminuisce al 66% (cioe' del 34%) dell'odds relativo a location 1 
#(ASSOCIAZIONE NEGATIVA TRA SOPRAVVIVENZA E LOCATION 2)

#torniamo al mod1: dev= 64.5 vs gdl=12: la devianza e' "troppo elevata".
#Prima di concludere che c'e' sovradispersione
#cerchiamo altre potenziali spiegazioni

#1. non siamo nel caso di dati sparsi:
troutegg$total                                      #ni>5
troutegg$total*predict(mod1,type="response")        #ni*mui.hat>1


#2. correttezza modello
par(mfrow=c(2,2))
plot(mod1)
par(mfrow=c(1,1))

#per valutare la presenza di OUTLIER (approssimativamente)

par(mfrow=c(1,2))
plot(rstudent(mod1),pch=19)
abline(h=c(-2,2))
par(mfrow=c(1,1))

pos<-which(abs(rstudent(mod1))>2);pos
#6 su 20!!!

library("car")
outlierTest(mod1)    
#Le unita' 1, 14, 15 e 20 sono outlier al 5%

influenceIndexPlot(mod1)

#Proviamo a trascurare l'osservazione 20
data.new<-troutegg[-20,]   #stiamo 138 trascurando uova!!
mod2<-glm(cbind(survive,total-survive)~location+period,family=binomial,data.new)
summary(mod2)


#proviamo a trattare la sovradispersione
#---------------------------------------

#(uova disomogenee in partenza? condizioni sperimentali diverse? 
#variabili esplicative non rilevate (condizioni atmosferiche, ...)?)

#stima del parametro di dispersione: X^2/12 (nel modello binomiale dovrebbe essere 1)
(phi.hat<-sum(residuals(mod1,type="pearson")^2)/df.residual(mod1))


summary(mod1)
summary(mod1,dispersion=phi.hat)

0.2813*sqrt(phi.hat)  #l'opzione dispersion moltiplica gli SE per sqrt(phi.hat)

#non si possono fare gli usuali test sulla bonta' del modello basati sulla devianza. 


#posso massimizzare direttamente una quasi-verosimiglianza (non fisso ldd della risposta,
#scelgo solo il link e la funzione di varianza)
mod3<-glm(cbind(survive,total-survive)~location+period,family=quasibinomial,troutegg)
summary(mod3)      #AIC NA
summary(mod1,dispersion=phi.hat)
plot(mod3)


pnorm(-0.734)*2
pt(-0.734,12)*2

