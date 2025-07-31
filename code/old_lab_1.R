###############
# STATISTICA III
###############


# ______________________________________________
#     LEZIONE 1: REGRESSIONE LOGISTICA BINARIA
# ______________________________________________



#---------------
# glm in generale
#---------------


# In R i glm si stimano con la funzione: glm(formula, family = , data=)

# L'oggetto family specifica sia la famiglia parametrica di tipo DE1, sia il link.

# Per la regressione logistica (BINARIA O BINOMIALE) la family e': binomial(link = "logit")

# Andamento dell'Odds
par(mfrow = c(1, 2))
f1 <- function(theta) (theta / (1 - theta))
curve(f1, 0, 1, xlab = expression(theta), lty = 2, ylab = "Odds")
abline(v = 0.5, col = 2, lty = 3)
abline(h = 1, col = 2, lty = 3)

# Andamento del link logit
link.logit <- function(theta) log(theta / (1 - theta))
curve(link.logit, 0, 1, xlab = expression(theta), lty = 2, ylab = "link logit")
abline(v = 0.5, col = 2, lty = 3)
par(mfrow = c(1, 1))

#-------------
#-------------
# DATA CARDIAC
#-------------
#-------------

# lettura di un dataframe (e' un file .csv)

cardiac <- read.csv("../data/cardiac.csv", sep = ";")
str(cardiac)
View(cardiac)

# lettura di un tibble
library(readr)
cardiac <- read_delim("G:/Il mio Drive/sonia/Docs/didattic/Statistica III/laboratorio_R_23-24/cardiac.csv", ";", escape_double = FALSE, trim_ws = TRUE)
str(cardiac)
View(cardiac)


attach(cardiac)

plot(Age, Chd, col = "red", pch = 19)

# boxplot per la variabile Age (x) distinta per malati (Y=1) e sani (Y=0)
plot(factor(Chd), Age, xlab = "Y", ylab = "age")


#-----------------
# stima del modello
#-----------------
mod <- glm(Chd ~ Age, family = binomial(link = "logit"), data = cardiac)
# il link logit e' assunto per default
mod <- glm(Chd ~ Age, family = binomial, data = cardiac)

(summ <- summary(mod))

# In base al test di Wald, i due coefficienti beta0 e beta1 sono significativi
# Numerose quantita' che vengono tabulate sono per ora sconosciute


#-----------------
# Alcune verifiche:
#-----------------
summ$coef

# dato lo z-value calcolo il p-value
(z.value <- summ$coef[1, 3])
2 * (1 - pnorm(abs(z.value)))

# dato il p-value calcolo lo z-value
(p.value <- summ$coef[1, 4])
qnorm(1 - p.value / 2) * (-1) # segno della stima!!


#---------------------
# grafico curva stimata
#---------------------
plot(Age, Chd, col = "red", pch = 19, main = "curva stimata")
grid <- seq(18, 70, by = .01)
eta <- coef(mod)[1] + coef(mod)[2] * grid
mu <- exp(eta) / (1 + exp(eta))

# nella library "faraway" e' implementata la funzione "ilogit" che calcola
# l'inversa del logit, cioe' mu=exp(eta)/(1+exp(eta))
# library(faraway)
# mu1<-ilogit(eta)

lines(grid, mu, type = "l", col = 3, lw = 3)



#------------------------------
# test per la bonta' del modello
#------------------------------

# confronto col modello nullo (M1= modello nullo, M2=modello corrente)
# H0:beta1=0

# stimo il modello nullo
mod.0 <- glm(Chd ~ 1, family = binomial, data = cardiac)
(summ.0 <- summary(mod.0))


(theta.hat <- exp(coef(mod.0)) / (1 + exp(coef(mod.0))))
sum(Chd) / 100

# Nel modello nullo si ha che tutte le u.s. hanno associata la stessa prob.
# di successo. Dovendo essere  sum(y_i)=sum(theta_i) (I equazione del sistema di
# equazioni di verosimiglianza) con theta_i costante, tale prob.
# e' stimata con  la frequenza relativa dei successi nel campione
# sum(y_i)/n


# LRT (likelihood ratio test=test del rapporto di verosimiglianza)
logLik(mod)
logLik(mod.0)
str(logLik(mod.0))
(lM0 <- as.numeric(logLik(mod.0)))
(lM <- as.numeric(logLik(mod)))

# valore osservato della statistica test
(lambda.obs <- -2 * (lM0 - lM))

# uso la legge asintotica (chi-quadro)
(p.value.bonta <- 1 - pchisq(lambda.obs, 1))

# rifiuto Ho:beta1=0, quindi accetto il modello con l'esplicativa

# ATTENZIONE: IL TEST DI WALD E LRT PER CONFRONTARE DUE MODELLI CHE
# DIFFERISCONO PER UNA ESPLICATIVA SONO ASINTOTICAMENTE EQUIVALENTI,
# MA NON SONO LO STESSO TEST


anova(mod)
anova(mod, test = "Chisq")
library("car")
Anova(mod) # library car


#----------------------
# aspetti interpretativi
#----------------------

(beta1 <- summ$coef[2, 1])
# esprime la variazione nel logit (quindi il log OR= log Odds ratio)
# 0.111>0: associazione positiva tra avere la malattia (Y=1) e avere un'eta'
# aumentata di 1 (da x a x+1)

(OR <- exp(beta1)) # detto anche fattore di rischio
# 1.117>1 l'odds di coloro che hanno eta' x+1 e' pari a 1.117 volte quello
# di coloro che hanno eta' x

# Quindi: se passo da x a x+1 l'Odds aumenta del (1.117-1)*100%=11.7%

# IC per beta
confint(mod) # osservo se sia incluso lo zero
# IC per OR
exp(confint(mod)) # osservo se sia incluso l'uno

# sintesi esaustiva
round(exp(cbind(Stima.OR = coef(mod), confint(mod))), 3)


#-------------------
# previsione puntuale
#-------------------

# stime del predittore lineare in corrispondenza dei valori di x osservati
x <- Age
(eta <- coef(mod)[1] + coef(mod)[2] * x)

# stime della risposta media (prob successo): usiamo il logit inverso
(theta <- exp(eta) / (1 + exp(eta)))



# se scelgo un valore per l'esplicativa procedo analogamente
# Ad esempio, per un ottantenne
eta.80 <- as.numeric(coef(mod)[1] + coef(mod)[2] * 80)
(theta.80 <- exp(eta.80) / (1 + exp(eta.80)))


# Funzione predict(mod,newdata=  ,type = )
#-----------------------------------------

# senza newdata usa i valori delle esplicative del data set (come in lm)

# type puo' essere:  "link" (stima il predittore lineare eta) (valore di default)
#                   "response" (stima le probabilita' theta ovvero mu)


predict(mod, type = "response")
predict(mod, type = "link")

# in modo equivalente (ma senza la possibilita` di fare previsioni per nuovi
# valori delle esplicative)

mod$fitted
mod$lin

# previsione per l'ottantenne
new <- data.frame(Age = 80)
predict(mod, newdata = new, type = "response")


#-----------------------
# previsione intervallare
#-----------------------

# matrice di varianze e covarianze asintotica stimata
names(summ)
(sigma.hat <- summ$cov.unscaled)
summ$cov.scaled # per altri glm questa coincidenza verra' meno


# per un ottantenne: x=80
x <- 80
new <- data.frame(Age = 80)
(var.logit.hat <- sigma.hat[1, 1] + x^2 * sigma.hat[2, 2] + 2 * x * sigma.hat[1, 2])
(se.logit <- sqrt(var.logit.hat))
(logit.80 <- predict(mod, newdata = new, type = "link"))
(IC.logit.80 <- logit.80 + c(-1, 1) * qnorm(0.975) * se.logit)
(IC.prob.80 <- exp(IC.logit.80) / (1 + exp(IC.logit.80)))
