

###############
#STATISTICA III
###############

#____________________________________
#     LEZIONE 3: binomial regression
#____________________________________


#-----------------------------------------------------------------------------
#Binomial regression: secondo esempio. Dati in forma di tabella di contingenza
#------------------------------------------------------------------------------

#Dataset proveniente da "The American Voter" (Campbell et al.)
#Risposta dicotomica: ha votato/non ha votato
#Esplicative: 1. closeness (vicinanza percepita delle elezioni): non vicino/vicino
#             2. intensity (intensita' della preferenza elettorale): debole, media, forte

#                            voted            did not vote
# closeness   intensity
# not close    weak           91                  39
#              medium         121                 49
#              strong         64                  24

# close        weak           214                 87
#              medium         284                 76
#              strong         201                 25



#data entry
#-----------
(closeness<-factor(rep(c("not close","close"),c(3,3)),levels=c("not close","close")))
(intensity<-factor(rep(c("weak","medium","strong"),2),levels=c("weak","medium","strong")))
voted<-c(91,121,64,214,284,201)
did.not.vote<-c(39,49,24,87,76,25)
(data.vote<-data.frame(closeness,intensity,voted,did.not.vote))


(n<-sum(data.vote$voted+data.vote$did.not.vote))
(m<-length(closeness))
(n.i<-data.vote$voted+data.vote$did.not.vote)
(y.i<-data.vote$voted/n.i)


#interaction.plot
#-----------------
#E' un grafico molto informativo quando le esplicative sono 2 fattori: il
#primo e' messo sull'asse orizzontale, il secondo definisce una variabile 
#che distingue i gruppi. Per ogni combinazione dei due fattori viene disegnato
#il valore medio della terza variabile


with(data.vote,interaction.plot(intensity,closeness,voted/(voted+did.not.vote),
                                ylab="proportion voted",type="b",pch=c(1,16)))
with(data.vote,interaction.plot(closeness,intensity,voted/(voted+did.not.vote),
                                ylab="proportion voted",type="b",pch=c(1,16)))

#stima del modello
#-----------------
binomial.mod<-glm(cbind(voted, did.not.vote)~closeness*intensity,family=binomial,data=data.vote)

summary(binomial.mod)


#si tratta del modello saturo: m=6 e i parametri beta da stimare sono 6 (intercetta,
#3 effetti principali e due interazioni).

#i residui sono nulli e i valori osservati coincidono con quelli stimati

(prob.obs<-(voted/n.i))                 #frequenze relative osservate
(predict(binomial.mod,type="response")) #probabilita' stimate

# (logit.obs<-log(voted/did.not.vote))
# (predict(binomial.mod,type="link"))


#modello senza interazioni

binomial.mod2<-update(binomial.mod,.~.-closeness:intensity)
summary(binomial.mod2)

#diagnostica

par(mfrow=c(2,2))
plot(binomial.mod2)
par(mfrow=c(1,1))
#con esplicative fattori non si possono inserire trasformate

anova(binomial.mod2,binomial.mod,test="Chisq")

#E' il test basato sulla differenza delle devianze (qui D e D* coincidono)
(D1<-deviance(binomial.mod2))
(D2<-deviance(binomial.mod))
(D1-D2)
1-pchisq(D1-D2,2)

#Qui coincide con:
#Test (approssimato) per la bonta' del modello BASATO SULLA DEVIANZA
#H0: il modello e' sufficientemente vicino al modello saturo
#perche' uno dei due modelli e' quello saturo
(p.value<-1-pchisq(D1,2))

#--------------------------------------------
#E se fosse trattato come binary regression?
#--------------------------------------------

#I dati presenti nella tabella di contingenza devono essere trasformati
#in  n=1275 righe
#-----------------------------------------------------------------------

#(n<-sum(data.vote$voted+data.vote$did.not.vote))
data.vote

#proviamo a generare le prime 39 righe con closeness=not close, intensity=weak, 
#modalita' "did.not.vote" per la risposta

prova<-with(data.vote,data.frame(clo=closeness[1],inten=intensity[1],
                                 vot=rep(0,did.not.vote[1])))
str(prova)


#Costruiamo un ciclo che ripete la generazione per ognuna delle 12 celle:
#-----------------------------------------------------------------------

#Inizializzazione di un data frame vuoto. 
#Nomi delle variabili abbreviati per non fare confusione
data.vote.long<-data.frame(clo=NULL,inten=NULL,vot=NULL)

#ciclo for con iteratore che va da 1 a 6=m=numero di pattern delle esplicative
for(i in 1:6){
  rows.non.voters<-with(data.vote,data.frame(clo=closeness[i],inten=intensity[i],
                                             vot=rep(0,did.not.vote[i])))
  rows.voters<-with(data.vote,data.frame(clo=closeness[i],inten=intensity[i],
                                         vot=rep(1,voted[i])))
  data.vote.long<-rbind(data.vote.long,rows.non.voters,rows.voters)
}


str(data.vote.long)


#verifica
#---------
#tabella di contingenza a 3 entrate
xtabs(~clo+inten+vot,data=data.vote.long)

#provate a riportare il dataset nella forma originale (data.vote)


#stima del binary regression model
#-----------------------------------

binary.mod<-glm(vot~clo*inten,family=binomial,data=data.vote.long)
#OSSERVAZIONE: la sintassi e' coerente con almeno uno dei due modi in cui si stima
#con family binomial?

summary(binary.mod)
summary(binomial.mod)

#La table of coefficients e' identica (stima, SE, z e p-value)
#Le devianze, i residui di devianza e l'AIC sono diversi!!!
#IL RISULTATO AFFIDABILE E' QUELLO DATO DAL BINOMIAL MODEL

#I test basati sulle differenze tra devianze danno gli stessi risultati (distribuzione
#chi-quadro con k2-k1 gdl)

#modello senza interazioni
binary.mod2<-update(binary.mod,.~.-clo:inten)
summary(binary.mod2)
anova(binary.mod2,binary.mod,test="Chisq")


anova(binomial.mod2,binomial.mod,test="Chisq")

#Il binary.mod non e' quello saturo (6 parametri ma 1275 singole osservazioni 
#e non piu' 6 celle)

