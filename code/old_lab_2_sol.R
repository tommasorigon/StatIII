

###############
#STATISTICA III
###############


#_______________________________
#REGRESSIONE LOGISTICA MULTIPLA
#_______________________________

#--------------------------------
#--------------------------------
#DATA SPECTOR (ESAME DEL 24.6.15)
#--------------------------------
#--------------------------------

#Il data set  spector contenuto nella library faraway contiene i dati relativi a 
#32 studenti.

#Le variabili presenti sono

#grade: variabile dicotomica che esprime se sia avvenuto (grade = 1) o non sia avvenuto
#(grade = 0) un miglioramento  nei voti

#psi: variabile dicotomica che esprime se lo studente sia stato esposto (psi = 1) o meno
#(psi = 0) ad un nuovo metodo di insegnamento

#tuce: misura della capacita' dello studente all'inizio del corso

#gpa: voto medio riportato dallo studente


library(faraway)
data(spector)
help(spector)
str(spector)
summary(spector)  
attach(spector)

#la risposta "grade" e' Bernoulli

#DOMANDA 1:
#-----------
#Si fornisca una tabella a doppia entrata riferita alle due variabili dicotomiche
#che fornisca le frequenze relative campionarie di ogni coppia di modalita' e si 
#decida se e' piu' frequente un miglioramento nei voti (grade =1) tra gli studenti
#esposti oppure non esposti al nuovo metodo


(n<-nrow(spector))
#tabella a doppia entrata delle due dicotomiche. Frequenze relative
(tab<-round(xtabs(~grade+psi)/n,2))
#frequenze relative condizionate
round(prop.table(tab,2),2) 

#COMMENTO: tra coloro che non sono stati esposti al nuovo metodo di studio (psi=0)
#solo il 16% ha avuto un miglioramento nei voti, invece tra coloro che sono stati 
#esposti (psi=1) il 57% ha ottenuto il miglioramento


#DOMANDA 2:
#-----------
#Scelta la variabile risposta grade, si stimi un GLM opportuno (senza interazioni)

#modello di regressione logistica (senza interazioni)
#----------------------------------------------------



mod<-glm(grade~psi+tuce+gpa,data=spector,family=binomial)

#stesso risultato indicando la risposta come array costituito da due colonne:
#la prima contiene il numero di successi e la seconda il numero degli insuccessi
mod<-glm(cbind(grade,1-grade)~psi+tuce+gpa,data=spector,family=binomial)

(mod.sum<-summary(mod))


#	L'equazione che esprime la risposta media stimata in funzione delle esplicative
##(equazione del modello stimato):

#logit(theta^)=-13.02135 +2.37869 psi+0.09516  tuce+2.82611 gpa 


#DOMANDA 3:
#-----------
#Si interpreti la stima del coefficiente di regressione per "psi":


#beta stimato per psi e' 2.37869 = stima del log OR (differenza nei logit)
# exp(beta.hat)
exp(2.37869) 
#=10.79 stima dell'OR
#passando da psi=0 (lo studente non e' esposto al nuovo metodo) al valore psi=1
#(studente e' esposto)
#l'odds relativo alla prob. di migliorare i voti
#aumenta di quasi 11 volte!
#(=ASSOCIAZIONE positiva TRA il nuovo metodo e il miglioramento nei voti)


#DOMANDA 4:
#-----------
#Si fornisca una misura del rischio relativo del successo (Y=1) per l'esplicativa "psi"
#per uno studente con tuce e gpa pari alla media



new<-data.frame(psi=c(0,1),tuce=c(mean(tuce),mean(tuce)),gpa=c(mean(gpa),mean(gpa)))
(p<-predict(mod,newdata=new,type="response"))
(RR<-p[2]/p[1])

#gli studenti (medi) sottoposti al nuovo metodo hanno una prob di migliorare i voti
#pari a 5.28 volte quella degli studenti (medi) non sottoposti al nuovo metodo

#per le esplicative che devono rimanere costanti va bene un valore qualunque purche'
#lo stesso ripetuto due volte ai fini dell'OR: il valore dell'OR non cambia
(OR<-(p[2]/(1-p[2]))/(p[1]/(1-p[1])))

#cio' non e' vero per il RR. Ad esempio:
new<-data.frame(psi=c(0,1),tuce=c(min(tuce),min(tuce)),gpa=c(min(gpa),min(gpa)))
(p<-predict(mod,newdata=new,type="response"))
(RR<-p[2]/p[1])
(OR<-(p[2]/(1-p[2]))/(p[1]/(1-p[1])))


#se valori max anziche' min il RR e' 1.31


#DOMANDA 5:
#-----------

#Si disegnino un grafico riportante la curva stimata (asse ordinate) al variare
#dei valori dell'esplicativa gpa (asse ascisse) (caso A) per uno studente che non
#e' stato esposto al nuovo metodo di insegnamento e ha valore della variabile tuce
#pari  alla sua media; (caso B)  per uno studente che e' stato esposto al nuovo
#metodo di insegnamento e ha valore della variabile tuce pari  alla sua media 
#Si rappresentino anche i punti osservati. 
#Il range per l'esplicativa deve essere (2,4).


par(mfrow=c(1,1))
plot(grade~gpa, xlab="gpa",ylab="Improvement prob",xlim=c(2,4),ylim=c(0,1),pch=20)
grid<-seq(2,4,by=.1)
eta.psi0<-coef(mod)[1]+coef(mod)[3]*mean(tuce)+coef(mod)[4]*grid
eta.psi1<-coef(mod)[1]+coef(mod)[2]+coef(mod)[3]*mean(tuce)+coef(mod)[4]*grid
mu.psi0<-ilogit(eta.psi0)
mu.psi1<-ilogit(eta.psi1)
lines(grid,mu.psi0,type="l",lty=5,col=2)
lines(grid,mu.psi1,type="l",lty=5,col=4)


#Commento:per ogni valore di gpa, la probabilita' stimata di successo
#(miglioramento dei voti) e' piu' elevata  per gli esposti rispetto ai non esposti. 


#DOMANDA 6:
#-----------
#Si stimi un GLM per la medesima risposta con le medesime esplicative, ma che
#preveda anche l'interazione tra la variabile psi e la variabile gpa
#a. si commentino i principali cambiamenti avvenuti nel modello
#b. si interpreti il valore assunto dal coefficiente dell'interazione
#c. si scelga tra i due modelli in base ad un opportuno criterio e in base ad
#un opportuno test



mod<-glm(grade~psi+tuce+gpa,data=spector,family=binomial)

mod2<-glm(grade~psi+tuce+gpa+psi:gpa,data=spector,family=binomial)

mod2<-glm(grade~tuce+psi*gpa,family=binomial, data=spector) 


(mod2.sum<-summary(mod2))
mod.sum

#a. Principali cambiamenti avvenuti nel modello
#---------------------------------------------
#Nel modello senza interazione sono significativi (al 5% ma non all' 1%) i 
#coefficienti delle variabili psi e gpa. Nel modello con interazione
#lo e' solo (al 10% ma non al 5%) gpa. (Ulteriori commenti sulla devianza quando
#avremo visto i glm)


#b. interpretazione coefficiente dell'interazione tra psi e gpa
#-------------------#------------------------------------------
#osserviamo i coefficienti di regressione stimati
coef(mod2)
#la stima dell'odds ratio per gpa e' exp(5.44)
exp(coef(mod2)[4])
#231
#se  psi=0 (lo studente non e' stato esposto al nuovo metodo).

#Essendo la stima del coefficiente dell'interazione -3.69379,
#per studenti con psi=1 (esposti al nuovo metodo) la stima
#del coefficiente di gpa diventa
coef(mod2)[4]+coef(mod2)[5]
#=1.749 quindi la stima dell'odds ratio in tal caso diventa
exp(coef(mod2)[4]+coef(mod2)[5])
#5.749. Cioe' l'associazione tra gpa e miglioramento dei voti resta
#positiva ma molto meno intensa.


#c. scelta tra i due modelli
#---------------------------

#criterion-based
#----------------
AIC(mod)
AIC(mod2)

#il modello senza interazione ha AIC piu' basso, quindi e' da preferirsi

#test-based
#-----------

anova(mod,mod2,test = "Chisq")

#il p-value porta all'accettazione dell'ipotesi che prevede che 
#il coeff dell'interazione sia nullo: si sceglie il modello senza interazione

#l'interazione e' il primo regressore da togliere


#MANCA LA DIAGNOSTICA!!!


