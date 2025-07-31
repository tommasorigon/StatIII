

###############
#STATISTICA III
###############

#____________________________________
#     LEZIONE 4: diagnostica
#____________________________________


#-------
#RESIDUI
#-------

#R fornisce molti tipi di residui

#residuals(mod) o residuals(mod,type="deviance")   fornisce i residui di devianza.
#E' la scelta di default per la diagnostica

#residuals(mod,type="pearson")   fornisce i residui di pearson

#residuals(mod,type="response")   fornisce i residui in termini di risposta,
#cioe' freq rel oss-prob stimate, cioe' yi-mui.hat. Sono i residui grezzi e
#numeratore dei residui di pearson

#residuals(mod,type="working")   fornisce un tipo di residui sotto prodotto della procedura IRLS,
#non servono per la diagnostica attenzione!!: mod$residuals fornisce i working


#LEVERAGE
#---------

#a differenza degli lm, la hat matrix H e' funzione non solo delle esplicative ma anche
#della risposta. Il leverage si usa in modo analogo agli lm: un leverage h.ii elevato e'
#indice di un punto di leva (potenziale punto influente), inoltre il leverage serve per
#ottenere residui standardizzati e jack-knifed estraendo gli h.ii dalla diagonale
#principale della matrice H

#influence(mod)$hat
#hatvalues(mod)

#RESIDUI STANDARDIZED E JACK-KNIFED (INT. O EST. STUDENTIZZATI)
#---------------------------------------------------------------

#le funzioni sono le stesse degli lm:  rstandard(mod) e rstudent(mod)
#rstandard possono essere di devianza o di pearson (occorre specificare il type="")

#rstudent no (sono solo un'approssimazione (di Williams) dei veri residui 
#jack-knifed)


#-----------------------------------------------------------------------------
#Binomial regression: Dataset proveniente da "The American Voter" 
#------------------------------------------------------------------------------

(closeness<-factor(rep(c("not close","close"),c(3,3)),levels=c("not close","close")))
(intensity<-factor(rep(c("weak","medium","strong"),2),levels=c("weak","medium","strong")))
voted<-c(91,121,64,214,284,201)
did.not.vote<-c(39,49,24,87,76,25)
data.vote<-data.frame(closeness,intensity,voted,did.not.vote)

(n<-sum(data.vote$voted+data.vote$did.not.vote))
(m<-length(closeness))
(n.i<-data.vote$voted+data.vote$did.not.vote)
(y.i<-data.vote$voted/n.i)


#stima dei due modelli binomiali (con e senza interazioni)
#-----------------------------------------------------------

binomial.mod<-glm(cbind(voted, did.not.vote)~closeness*intensity,family=binomial,data=data.vote)
binomial.mod2<-update(binomial.mod,.~.-closeness:intensity)


#-------------------------------------------------------------
#(1) Diagnostica per singole osservazioni o piccoli gruppi di oss.
#-------------------------------------------------------------

#(1.a) LEVERAGE
#------------
influence(binomial.mod)$hat   #nel modello saturo sono tutti 1 (fit perfetto)
influence(binomial.mod2)$hat
#oppure
hatvalues(binomial.mod2)

sum(hatvalues(binomial.mod2))         #k+1=4
(h.iimedio<-sum(hatvalues(binomial.mod2))/m)      #leverage medio



#(1.b) MISURE DI INFLUENZA
#-----------------------

#Sono analoghe a quelle degli lm. Tra le altre

#distanza di Cook:  cooks.distance(mod)  (valutare i dati con dist piu' elevata)
#-------------------------------------------------------------------------------
cooks.distance(binomial.mod2)


#stime dei coeff di regresssione: quanto cambiano se si omette l'i-esimo caso:
#-----------------------------------------------------------------------------
#influence(mod)$coef
influence(binomial.mod2)$coef


#(1.c) OUTLIER
#-----------

#tipicamente gli outlier vengono identificati con dati per i quali rstudent e' "elevato"
#(la soglia approssimata e' pari a 2, ma non tiene conto della correzione di Bonferroni)

(res.stud<-rstudent(binomial.mod2))
#2 residui sono superiori a 2 in modulo

library("car")
outlierTest(binomial.mod2)     #package car
#nessuno di questi sembra essere un outlier


#INFLUENCE INDEX PLOTS
#Grafici relativi alle varie misure (Cook's distances, leverages, Studentized residuals,
#and outlier significance levels) plottate per ogni u.s.
influenceIndexPlot(binomial.mod2)

#INFLUENCE PLOTS
#a "bubble" plot of Studentized residuals by hat values, with the areas of the
#circles representing the observations proportional to Cook's distances. 
#Vertical reference lines are drawn at twice and three times the average hat value,
#horizontal reference lines at -2, 0, and 2 on the Studentized-residual scale
influencePlot(binomial.mod2)



#--------------------------------------
#(2) Diagnostica per assunzioni modello
#--------------------------------------

#(2.a) forma strutturale: eventuali relazioni non lineari tra eta.hat (predittore
#lineare stimato) e e^DS (residui di devianza standardizzati)

(eta.hat<-predict(binomial.mod2,type="link"))        #di default, cioe' equivalente a predict(binomial.mod2)
(eDS<-rstandard(binomial.mod2,type="deviance"))      #di default, cioe' equivalente a rstandard(binomial.mod2)
plot(eta.hat,eDS,main="plot per linearita'")

x.new<-eta.hat^2
new.mod<-update(binomial.mod2,.~.+x.new)
summary(new.mod)
#c'e' evidenza di non linearita', tuttavia le esplicative sono fattori!!


#(2.a) forma strutturale: link. 
#Plot di eta.hat vs zeta.hat (stima pseudo-risposte) 

(eta.hat<-predict(binomial.mod2))
mu.hat<-predict(binomial.mod2,type="response")
(derivata<-1/(mu.hat*(1-mu.hat)))
zeta.hat<-eta.hat+(y.i-mu.hat)*derivata
plot(zeta.hat~eta.hat,xlab=expression(eta),ylab="z",main="diagnostica link")
#aspettativa: andamento rettilineo

#(2.b) funzione di varianza.
#mu.hat (risposta media stimata) vs residui standardizzati (di devianza o di pearson)
(mu.hat<-predict(binomial.mod2,type="response"))
(eDS<-rstandard(binomial.mod2,type="deviance")) 
#aspettativa: dispersione costante

#si usa anche: (mu.hat vs ePS)
plot(eDS~mu.hat,,xlab=expression(mu),ylab=expression(e^{DS}),main="diagnostica funz varianza")

#Oppure (eta.hat vs e grezzi), ma questi ultimi forniscono
#un grafico da interpretare diversamente!!


#--------------------------------
#Plot diagnostici di default di R
#--------------------------------

#Attenzione: a. i fitted values sono in termini di predittore lineare (etai=g(mui) e non di 
#                valori previsti per la risposta (mui) 
#            b. un leggero allontanamento dalla bisettrice nel QQ-plot e' frequente
#GRAFICO 1: RESIDUALS VS FITTED. Riporta eta.hat vs residui di pearson (eP)
#GRAFICO 2: NORMAL Q-Q
#GRAFICO 3: SCALE-LOCATION. Riporta eta.hat vs sqrt(ePS)
#GRAFICO 4: RESIDUALS VS LEVERAGE. Riporta h.ii vs ePS

par(mfrow=c(2,2))
plot(binomial.mod2)
par(mfrow=c(1,1))

summary(binomial.mod2)

#------------------------------------
#ESEMPIO ATTACCO CARDIACO CREATININA
#------------------------------------

#ck = livello di creatinina (solo valori centrali di classe)
#h.ok = numero di pazienti senza attacco cardiaco
#h.a = numero di pazienti con attacco cardiaco


ck<-c(20,60,100,140,180,220,260,300,340,380,420,460)
seq(20,460,by=40)
h.a<-c(2,13,30,30,21,19,18,13,19,15,7,8)    #yitilde
h.ok<-c(88,26,8,5,0,1,1,1,1,0,0,0)          #ni-yitilde
heart<-data.frame(ck,h.ok,h.a) 
(m<-length(ck))        #numero di pattern della esplicativa (con k=1 coincide col numero di valori)
(n.i<-h.ok+h.a)        #numero di repliche per ogni valore della esplicativa
(y.i<-h.a/n.i)         #frequenza relativa dei successi

#stima del modello di regressione logistica 
mod.1<-glm(y.i~ck,family=binomial,weights=n.i)
summary(mod.1)

par(mfrow=c(2,2))
plot(mod.1)
par(mfrow=c(1,1))


#commenti
#---------
#sembra ci sia un trend nel primo plot.
#il plot dei residui vs i fitted suggerisce un predittore lineare cubico al posto della retta

#il primo punto e' influente

mod.3<-glm(y.i~ck+I(ck^2)+I(ck^3),family=binomial,weights=n.i)
mod.3

summary(mod.3)

#la devianza=4.252 h circa la meta` dei gdl=8. L'AIC e' passato da 62.33 a 33.66
#(miglioramento sostanziale)

#Dati osservati e valori fittati
par(mfrow=c(1,2))
plot(y.i~ck,xlab="livello creatinina", ylab="proporzione ha",main="predittore lineare")
lines(ck,fitted(mod.1))

plot(y.i~ck,xlab="livello creatinina", ylab="proporzione ha",main="predittore cubico")
lines(ck,fitted(mod.3))
par(mfrow=c(1,1))
#e' molto meglio!

#Ovviamente il confronto va fatto formalmente tramite test (funzione "anova" ma e' TRV
#cioe' basato sulla devianza)

anova(mod.1,mod.3,test="Chisq")
#forte evidenza contro l'ipotesi nulla: chiaro sostegno al modello cubico
1-pchisq(32.676,2)

#attenzione
anova(mod.1,mod.3,test="F")   #non va bene!!!

#test F: k1=1, k2=3, m=12
(dev.1<-mod.1$deviance)
(dev.2<-mod.3$deviance)
(f.obs<-((dev.1-dev.2)/2)/(dev.2/8))
1-pf(f.obs,2,8)


#diagnostica
par(mfrow=c(2,2))
plot(mod.3)
par(mfrow=c(1,1))

#i residui mostrano pattern meno chiari (forse la funzione di varianza non e' adatta?)


