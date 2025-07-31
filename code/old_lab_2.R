

###############
#STATISTICA III
###############


#______________________________________________
#     LEZIONE 2: REGRESSIONE LOGISTICA BINARIA
#______________________________________________


#------------------------------------------
#------------------------------------------
#DATA CARDIAC CON ESPLICATIVA DICOTOMIZZATA
#------------------------------------------
#------------------------------------------

cardiac <- read.csv("G:/Il mio Drive/sonia/Docs/didattic/Statistica III/laboratorio_R_24-25/cardiac.csv", sep=";")
attach(cardiac)

#dicotomizziamo l'eta' in base alla soglia 55
(x<-ifelse(Age<55,0,1))
str(x)
x<-as.factor(x)
levels(x)<-c("giovani","anziani")
#giovani e' la modalita' baseline, anziani e' quella codificata con la dummy
summary(x)


#Per capire se la malattia sia piu' diffusa tra i giovani o tra gli anziani:
#tabella a doppia entrata delle due dicotomiche
(tab<-xtabs(~x+Chd))


#se servisse convertire la tabella in matrice non basta as.matrix, serve
#as.matrix.xtabs del package DescTools


#Frequenze relative condizionate: proporzioni di malati distinte per eta':
#devo dividere per i totali di riga
(tab<-round(prop.table(tab,1),2))


#Cross product ratio
tab[1,1]*tab[2,2]/(tab[1,2]*tab[2,1])


#stima del modello
mod.dic<-glm(Chd~x,family=binomial,data=cardiac)
summary(mod.dic)
library("car")
Anova(mod.dic)

#ATTENZIONE: i p-value sono diversi. L'equivalenza e' solo asintotica.

#Nel caso di x dicotomizzata il numero di modalita' diverse di x e'
#pari a 2 con 73 ripetizioni per x=0 (giovani) e 27 per x=1 (anziani)
#SAREBBE PIU' OPPORTUNA UNA BINOMIAL LOGISTIC REGRESSION (risposta binomiale)
#ANZICHE' LA BINARY LOGISTIC REGRESSION (risposta Bernoulli)


#Interpretazione
#----------------

round(exp(cbind(Stima.OR=coef(mod.dic),confint(mod.dic))),3)

#L'odds di malattia per gli anziani e' oltre 8 volte quello dei giovani
#L'IC e' molto ampio

#Il rischio relativo e':
new<-data.frame(x=c("giovani","anziani"))
(p<-predict(mod.dic,newdata=new,type="response"))
(OR<-(p[2]/(1-p[2]))/(p[1]/(1-p[1])))

(RR<-p[2]/p[1])
#gli anziani hanno 2.58 volte la prob di malattia dei giovani



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

#..... 

#COMMENTO: tra coloro che non sono stati esposti al nuovo metodo di studio (psi=0)
#solo il 16% ha avuto un miglioramento nei voti, invece tra coloro che sono stati 
#esposti (psi=1) il 57% ha ottenuto il miglioramento


#DOMANDA 2:
#-----------
#Scelta la variabile risposta grade, si stimi un GLM opportuno (senza interazioni)

#modello di regressione logistica (senza interazioni)
#----------------------------------------------------


#.....


#	L'equazione che esprime la risposta media stimata in funzione delle esplicative
##(equazione del modello stimato):

#logit(theta^)=-13.02135 +2.37869 psi+0.09516  tuce+2.82611 gpa 


#DOMANDA 3:
#-----------
#Si interpreti la stima del coefficiente di regressione per "psi":


#.....


#passando da psi=0 (lo studente non e' esposto al nuovo metodo) al valore psi=1
#(studente e' esposto)
#l'odds relativo alla prob. di migliorare i voti
#aumenta di quasi 11 volte!
#(=ASSOCIAZIONE positiva TRA il nuovo metodo e il miglioramento nei voti)


#DOMANDA 4:
#-----------
#Si fornisca una misura del rischio relativo del successo (Y=1) per l'esplicativa "psi"
#per uno studente con tuce e gpa pari alla media

#.....

#gli studenti sottoposti al nuovo metodo hanno una prob di migliorare i voti
#pari a 5.28 volte quella degli studenti non sottoposti al nuovo metodo

#per le esplicative che devono rimanere costanti va bene un valore qualunque purche'
#lo stesso ripetuto due volte ai fini dell'OR: il valore dell'OR non cambia


#cio' non e' vero per il RR. Ad esempio:

#.....


#DOMANDA 5:
#-----------

#Si disegnino un grafico riportante la curva stimata (asse ordinate) al variare
#dei valori dell'esplicativa gpa (asse ascisse) (caso A) per uno studente che non
#e' stato esposto al nuovo metodo di insegnamento e ha valore della variabile tuce
#pari  alla sua media; (caso B)  per uno studente che e' stato esposto al nuovo
#metodo di insegnamento e ha valore della variabile tuce pari  alla sua media 
#Si rappresentino anche i punti osservati. 
#Il range per l'esplicativa deve essere (2,4).


#.....


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


#.....


#a. Principali cambiamenti avvenuti nel modello
#---------------------------------------------
#Nel modello senza interazione sono significativi (al 5% ma non all' 1%) i 
#coefficienti delle variabili psi e gpa. Nel modello con interazione
#lo e' solo (al 10% ma non al 5%) gpa. (Ulteriori commenti sulla devianza quando
#avremo visto i glm)


#b. interpretazione coefficiente dell'interazione tra psi e gpa
#-------------------#------------------------------------------


#.....


#c. scelta tra i due modelli
#---------------------------

#criterion-based
#----------------


#.....

#il modello senza interazione ha AIC piu' basso, quindi e' da preferirsi

#test-based
#-----------



#il p-value porta all'accettazione dell'ipotesi che prevede che 
#il coeff dell'interazione sia nullo: si sceglie il modello senza interazione

#l'interazione e' il primo regressore da togliere


#MANCA LA DIAGNOSTICA!!!


