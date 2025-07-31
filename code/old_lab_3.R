

###############
#STATISTICA III
###############


#________________________________
#     LEZIONE 2: GLM IN GENERALE
#________________________________



#In R i glm si stimano con la funzione: glm(formula, family = , data=)

#L'oggetto family specifica sia la famiglia parametrica di tipo DE1, sia il link.

#Un elenco delle DE1 implementate con i rispettivi link canonici (dall'help):
#binomial(link = "logit")
#gaussian(link = "identity")
#Gamma(link = "inverse")
#inverse.gaussian(link = "1/mu^2")
#poisson(link = "log")
#quasi(link = "identity", variance = "constant")
#quasibinomial(link = "logit")
#quasipoisson(link = "log")

#Se non specificato il link di default e' quello canonico.
#I link implementati da R sono: "logit", "probit", "cauchit", "cloglog", "identity",
#"log", "sqrt", "1/mu^2"e "inverse"


#ogni link (oggetto di classe "link-glm") e' una lista che fornisce

#$linkfun:  Link function:  function(mu)
  
#$linkinv:   Inverse link function:  function(eta)
  
#$mu.eta:  Derivative function(eta):  dmu/deta

#$valideta:  function(eta) { TRUE if eta is in the domain of linkinv }.

#$name: a name to be used for the link

#Ad esempio:

make.link("inverse")

#-----------------------------------
#Caratteristiche della funzione glm
#-----------------------------------

help(glm)     #oppure evidenzio glm +F1

#Gli argomenti data, subset, na.action, offset hanno lo stesso significato
#e lo stesso utilizzo che avevano negli lm

#Argomento start: serve per specificare eventuali valori iniziali dei beta presenti nel
#predittore lineare (tipicamente non ce n'e' bisogno perche' l'algoritmo implementato
#in R fornisce in modo efficiente i valori iniziali)

#Argomento control: per scegliere alcune opzioni tecniche che controllano l'algortimo
#IRLS (possono essere utili quando nella stima si verificano problemi di convergenza).
#Ad esempio: 

#epsilon= (di default e' |dev - dev_{old}|/(|dev| + 0.1) <  1e-8.) indica il valore 
#nel cambiamento di due valori successivi nella devianza per poter dichiarare l'avvenuta
#convergenza

#maxit=  (di default e'25) numero massimo di iterazioni



#___________________
#Binomial regression
#___________________

#La famiglia e' binomial, i link implementati sono: logit, probit, cloglog


#-----------------------------
#confronto grafico tra i link
#-----------------------------

link.logit<-function(mu) log(mu/(1-mu))
link.probit<-function(mu) qnorm(mu)
link.cloglog<-function(mu) log(-log(1-mu))
link.loglog<-function(mu) -log(-log(mu))

par(mfrow=c(1,1))
curve(link.logit,0,1,xlab=expression(mu),ylab="link")
curve(link.probit,add=T,lty=2)
curve(link.cloglog,add=T,lty=3)
curve(link.loglog,add=T,lty=4)
legend(0.005,5,legend=c("logit","probit","cloglog","loglog"),lty=c(1,2,3,4),bty = "n")


#Ci sono due modi per specificare la variabile risposta:

#1. si fornisce la proporzione di successi (yi) come risposta e, tra gli argomenti
#della funzione glm, si specificano gli ni (numero di repliche) come weights

#2. si fornisce la risposta come array costituito da due colonne: la prima
#contiene il numero di successi (yitilde) e la seconda il numero degli insuccessi
#(ni-yitilde)


#----------------------------------------------------------------------------------
#Primo esempio con dati dicotomici con replicazioni e 1 esplicativa. Link canonico.
#----------------------------------------------------------------------------------

#DATA SET HEART: 360 pazienti reclutati perche' con sospetti problemi cardiaci (Hand et al., 1994,
#"A handbook of small data sets", Chapman & Hall). Investigazione medica successiva:
#si/no attacco cardiaco (ha)

#ck = livello di creatinina (solo valori centrali di classe)
#h.ok = numero di pazienti senza attacco cardiaco
#h.a = numero di pazienti con attacco cardiaco

#obiettivo: capire se ck puo' essere una buona esplicativa per la probabilita` di ha (=successo)


ck<-c(20,60,100,140,180,220,260,300,340,380,420,460)
seq(20,460,by=40)
h.a<-c(2,13,30,30,21,19,18,13,19,15,7,8)    #yitilde
h.ok<-c(88,26,8,5,0,1,1,1,1,0,0,0)          #ni-yitilde
(heart<-data.frame(ck,h.ok,h.a) )


(m<-length(ck))        #numero di pattern della esplicativa (con k=1 coincide col numero di valori)
(n.i<-h.ok+h.a)        #numero di repliche per ogni valore della esplicativa
(y.i<-h.a/n.i)         #frequenza relativa dei successi


#plot delle frequenze relative vs ai livelli di ck
plot(y.i~ck,xlab="livello creatinina", ylab="proporzione ha")


#stima del modello di regressione logistica nelle due varianti di specificazione della risposta
mod.1<-glm(y.i~ck,family=binomial,weights=n.i)
mod.1

mod.2<-glm(cbind(h.a,h.ok)~ck,family=binomial)
mod.2

summary(mod.2)



#Null Deviance e' la devianza del modello con la sola intercetta
#Residual Deviance e' la devianza del modello stimato

#ATTENZIONE: df=m-k-1 e non n-k-1

#La devianza=36.93 e' "troppo alta" rispetto ai gdl=10 (media del chi-quadro) anche tenendo
#conto della deviazione standard: sqrt(2*gdl) oppure 2*sqrt(2*gdl)
sqrt(2*10)
2*sqrt(2*10)

(dev<-mod.2$deviance)

#Test (approssimato) per la bonta' del modello BASATO SULLA DEVIANZA
#H0: il modello e' adeguato (cioe' sufficientemente vicino al modello saturo)
(p.value<-1-pchisq(dev,10))
#rifiuto H0: rifiuto il modello


#Test (approssimato) per la bonta' del modello BASATO SU X2
(X2<-sum(residuals(mod.2,type="pearson")^2))
(p.value<-1-pchisq(X2,10))
#rifiuto H0: rifiuto il modello


#interpretazione: exp(beta.hat)= odds ratio
exp(0.031244)  #circa 1.03 stima dell'odds ratio
#passando da un valore di ck al valore aumentato di 1 (e fermo restando tutto il resto)
#l'odds relativo all'attacco cardiaco aumenta del 3% circa
#(associazione positiva tra aumento di creatinina e attacco cardiaco)


#Dati osservati e valori fittati: il plot del modello stimato sovraimposto alle proporzioni
#osservate:
par(mfrow=c(1,1))
plot(y.i~ck,xlab="livello creatinina", ylab="proporzione ha")
lines(ck,fitted(mod.1))

#MANCA LA DIAGNOSTICA

