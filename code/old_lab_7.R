###############
# STATISTICA III
###############

# _____________________________________________________________
#     GLM PER RISPOSTE COUNT (POISSON REGRESSION)
# _____________________________________________________________


# La famiglia e il link implementati sono:
# poisson(link = "log")
# quasipoisson(link = "log")


###############
# Primo esempio
###############

library(faraway)
data(gala)
help(gala)
str(gala)
data.set <- gala[, -2]


#----------------
# modello lineare
#----------------
mod.lin <- lm(Species ~ ., data = data.set)
(sum.1 <- summary(mod.lin))

# la devianza residua del modello lineare coincide con la devianza del glm
names(sum.1)
(DR <- (sum.1$sigma)^2 * mod.lin$df)
(DR <- sum((mod.lin$residuals)^2))

mod.lin2 <- glm(Species ~ ., family = gaussian, data = data.set)
summary(mod.lin2)

# i residui di pearson e di devianza coincidono
residuals(mod.lin2, type = "pearson") == residuals(mod.lin2, type = "deviance")


par(mfrow = c(2, 2))
plot(mod.lin)
plot(mod.lin2)
par(mfrow = c(1, 1))
# C'e' sospetto di eteroschedasticita' (ovvero problemi sulla funzione di varianza).
# Si potrebbe provare a trasformare la risposta (Box-Cox suggerisce sqrt(y))
# con le conseguenti difficolta' interpretative


#-------------------------
# regressione di  poisson
#-------------------------

mod.pois <- glm(Species ~ ., family = poisson, data.set)
summary(mod.pois)

# tutte le esplicative significative!!! Standard Error non affidabili?
# Residual Deviance = 716.85 e' "troppo alta" rispetto ai gdl=24
# Con un test

p.value <- 1 - pchisq(mod.pois$deviance, 24)
p.value
# il modello viene rifiutato: e' troppo "lontano" da quello saturo in termini
# di sup della log-verosimiglianza

par(mfrow = c(2, 2))
plot(mod.pois)
par(mfrow = c(1, 1))


# commenti
# Ci sono parecchi punti influenti
# scale-location evidenzia problemi nella struttura di varianza
# indaghiamo

par(mfrow = c(1, 2))
mu.hat <- fitted(mod.pois)
# eta<-predict(mod.pois,type="link")
# oppure eta<-mod.pois$lin

# per valutare la funzione di varianza
plot(rstandard(mod.pois) ~ mu.hat) # mi aspetto dispersione costante, ma qui non e' cosi'


# proviamo a trattare la sovradispersione

# stima del parametro di dispersione: X^2/24 (nel modello poisson dovrebbe essere 1)
phi.hat <- sum(residuals(mod.pois, type = "pearson")^2) / df.residual(mod.pois)
phi.hat

summary(mod.pois, dispersion = phi.hat)
# l'opzione dispersion moltiplica gli SE per sqrt(phi.hat)

# non si possono fare gli usuali test sulla bonta' del modello basati sulla devianza.


# commenti all'output: 3 esplicative significative

# interpretazione: beta2.hat=0.0035406
# exp(0.0035406)=1.003547   incidence rate ratio
# se elevation aumenta di 1 metro il numero di specie mediamente
# aumenta dello 0,35%


# posso massimizzare direttamente una quasi-verosimiglianza
# mod.quasi.pois<-glm(Species~.,family=quasipoisson,data.set)
# in questo caso non ho AIC (non ho verosimiglianza specificata)


# proviamo a togliere Isabela (16a osservazione)
mod.miss <- glm(Species ~ ., family = poisson, data.set[-16, ])
summary(mod.miss)
summary(mod.pois)
par(mfrow = c(2, 2))
plot(mod.miss)

# guardiamo i dati
data.set

# Isabela ha una area molto diversa dalle altre isole
# l'area ha una distribuzione molto asimmetrica e dispersa
par(mfrow = c(2, 2))
hist(data.set$Area, freq = F, breaks = 20)
plot(density(data.set$Area))
log.area <- log(data.set$Area)
hist(log.area, freq = F, breaks = 10)
plot(density(log.area))

# altrettanto vale per Adjacent
# proviamo a trasformarle col log

mod.pois2 <- glm(Species ~ log(Area) + Elevation + Nearest +
  Scruz + log(Adjacent), family = poisson, data.set)

summary(mod.pois2)
summary(mod.pois)
# la devianza resta alta. Proviamo a sistemare la sovradispersione
phi.hat2 <- sum(residuals(mod.pois2, type = "pearson")^2) / df.residual(mod.pois2)
phi.hat2
summary(mod.pois2, dispersion = phi.hat2)


# se ancora non siamo soddisfatti dal modello: regressione non parametrica



##############
# Secondo esempio
##############
library(car)
data(Ornstein)
str(Ornstein)
help(Ornstein)

Boxplot(interlocks ~ nation, data = Ornstein)
# The firms are in decreasing order by assets

# HOMEWORK: scelta la risposta interlocks, provare a fare regressione lineare

# La risposta e' un conteggio: preferibile una regressione di poisson

Ornstein$interlocks
(tab <- xtabs(~interlocks, data = Ornstein)) # distribuzione di frequenza


# Graficamente:
par(mfrow = c(1, 2))
names(tab) # sono etichette e devono essere convertite in numeri
ascisse <- as.numeric(names(tab))

# distribuzione empirica
plot(ascisse, tab, type = "h", xlab = "Numero di interlocks effettivi", ylab = "frequenza")
points(ascisse, tab, pch = 16)
# si notano asimmetria positiva e frequenza elevata per il valore 0

# distribuzione teorica (poisson di media pari alla media campionaria)
(media <- mean(Ornstein$interlocks))
camp <- rpois(250, media) # DISTRIBUZIONE SIMULATA
(tab.camp <- xtabs(~camp))
ascisse.camp <- as.numeric(names(tab.camp))
plot(ascisse.camp, tab.camp, type = "h", xlab = "Numero di interlocks teorici", ylab = "frequenza")
points(ascisse.camp, tab.camp, pch = 16)
par(mfrow = c(1, 1))


qqPlot(tab, distribution = "pois", lambda = media) # library car

# test di adattamento
ks.test(tab, "ppois", media) # servirebbe jitter(tab) per evitare i ties
# si rifiuta l'ipotesi nulla che la distribuzione sia poisson

# tuttavia la nostra aspettativa e' che la risposta sia una poisson
# con media diversa a seconda del pattern delle covariate


# Modello
mod.Orn <- glm(interlocks ~ log(assets) + nation + sector, family = poisson, data = Ornstein)
summary(mod.Orn)

# D=1547.1 su 234 df!!!
# HOMEWORK: test per il modello
# HOMEWORK: COME MAI SI E' RITENUTO UTILE INSERIRE LOG ASSETS?


Anova(mod.Orn)
# tutte le esplicative sono significative


exp(coef(mod.Orn))

# Ad esempio: exp(beta.hat) di log assets e' 1.5705816. Quindi se log assets aumenta di 1
# (fermo restando il resto) il numero medio di interlocks aumenta del 57% [cioe' (1.57-1)x100%]

# Ad esempio: exp(beta.hat) di nationUK e' 0.6789410. Quindi passando da azienda canadese (baseline)
# ad azienda inglese (fermo restando il resto) il numero medio di interlocks diminuisce
# al 67.8% (quindi del 32.2%)


# provare a fare diagnostica
# provare a testare l'assenza di sovradispersione
# provare a trattare la eventuale sovradispersione
# (QUI SERVIREBBE UN ZERO-INFLATED POISSON REGRESSION MODEL: trattamento della
# sovradispersione tenendo conto che il numero di 0 osservati e' "eccessivo")




####################################
# Terzo esempio: poisson rate model
####################################

# studio dell'effetto delle radiazioni gamma sul numero di anomalie
# cromosomiche osservate.
# data set: dicentric

library(faraway)
data(dicentric)
str(dicentric)
help(dicentric)

# cells: numero di cellule esposte alle radiazioni
# ca: numero di  alterazioni cromosomiche
# doseamt: dose amount
# doserate: tasso di applicazione della dose di radiazioni (velocita')

# cells=ni non e' definito a priori, ma e' aleatorio (irradio una zona)

round(xtabs(ca / cells ~ doseamt + doserate, dicentric), 2)


par(mfrow = c(1, 2))
with(dicentric, interaction.plot(doserate, doseamt, ca / cells))

with(dicentric, interaction.plot(doseamt, doserate, ca / cells))

# doseamt assume solo 3 valori: lo trattiamo come factor (esplicativa qualitativa)
dicentric$effamt <- factor(dicentric$doseamt)

# l'effetto di doserate e' chiaramente moltiplicativo: proviamo anche il modello
# con log(doserate)

# poisson rate model con doserate
mod.rate <- glm(ca ~ offset(log(cells)) + doserate * effamt, family = poisson, dicentric)
summary(mod.rate)
# (quasi) tutto significativo, solo doserate ha p-value 8%
# devianza 73.6 superiore a 34 (df+2*sqrt(2*df))

par(mfrow = c(2, 2))
plot(mod.rate)
par(mfrow = c(1, 1))

# poisson rate model con log doserate
mod.rate2 <- glm(ca ~ offset(log(cells)) + log(doserate) * effamt, family = poisson, dicentric)
summary(mod.rate2)
# tutto significativo
# devianza 21.7

par(mfrow = c(2, 2))
plot(mod.rate2)
par(mfrow = c(1, 1))

# possiamo accettare questo modello? Sospetti sulla variabilita'?

phi.hat <- sum(residuals(mod.rate2, type = "pearson")^2) / df.residual(mod.rate2)
phi.hat


# interpretiamo: l'amount (effamt) e' chiaramente significativo.
# ad esempio: beta(effamt)5.hat=2.76109 significa che passando da amount 1 ad amount 5
# la risposta diventa mediamente
exp(2.76109)
# 15.8 volte tanto (viene moltiplicata per 15.8)
(exp(2.76109) - 1) * 100

# questo effetto e' amplificato dalla velocita` (rate) dato che le interazioni sono
# significative
# ad esempio: dare una dose alta (5) ad una (log)velocita` aumentata di una unita` (da 1 a 2
# cosi' come da 3 a 4 o altro passaggio) fornisce una risposta mediamente moltiplicata
# per
exp(0.19350)
(exp(0.19350) - 1) * 100
# 1.21 (aumento del 21%)



# __________________________________
#     BETA REGRESSION
# __________________________________

# Risposta beta per fenomeni con supporto (0, 1), tipicamente proporzioni
# (di attivita' finanziarie nel portafoglio,
# di tempo che dedicate quotidianamente ai social,
# di voti presi da Trump/Harris alle elezioni presidenziali, etc...)

#-------------------------------------------------
# Non essendo DE1, il modello non e' un GLM.
# Quindi tutta l'inferenza e' da costruire ad hoc!!
#-------------------------------------------------


# PRIMA DI PROCEDERE DOVETE:
#--------------------------
# (1) assicurarvi di avere la versione di R 4.3.0 o successive

# (2) installare il componente aggiuntivo Rtools43
# Ecco il link per scaricarlo https://cran.r-project.org/bin/windows/Rtools/

# (3) scaricare la library "FlexReg" (Install)


#########################
# Densita' della v.c. beta
#########################

library(gridExtra)
library(ggplot2)
library(FlexReg)

p1 <- curve.density(type = "Beta", mu = 0.5, phi = 5) + labs(title = "Graph 1")
p2 <- curve.density(type = "Beta", mu = 0.5, phi = 10) + labs(title = "Graph 2")
p3 <- curve.density(type = "Beta", mu = 0.3, phi = 5) + labs(title = "Graph 3")
p4 <- curve.density(type = "Beta", mu = 0.3, phi = 2) + labs(title = "Graph 4")

grid.arrange(p1, p2, p3, p4, ncol = 2)

#########################
# Esempio: dati elettorali
#########################


data(Election)
str(Election)
help(Election)
# unita' statistiche: 232 distretti elettorali italiani
attach(Election)
summary(Election)

#----------------
# modello lineare
#----------------

#---
# M5S
#---

mod.lin <- lm(M5S ~ AgeInd + ER + Illiteracy + Foreign)
(sum.1 <- summary(mod.lin))
#              Estimate   Std. Error  t value  Pr(>|t|)
# (Intercept)  0.8030822  0.0374512  21.443  < 2e-16 ***
# AgeInd      -0.0014777  0.0009194  -1.607  0.10941
# ER          -0.0076611  0.0006756 -11.339  < 2e-16 ***
# Illiteracy   0.0061363  0.0018999   3.230  0.00142 **
# Foreign     -0.0004449  0.0001362  -3.265  0.00126 **

step(mod.lin)
# con selezione criterion-based non si elimina alcuna covariata

# previsione
new <- data.frame(
  AgeInd = 26, ER = 95,
  Illiteracy = 0.1, Foreign = 190
)
predict(mod.lin, newdata = new)
# fuori dal range della risposta!!!


#-------------
# ALTRI PARTITI
#-------------

mod.lin2 <- lm(LEU ~ AgeInd + ER + Illiteracy + Foreign)
summary(mod.lin2)
#              Estimate   Std. Error t value Pr(>|t|)
# (Intercept)  2.609e-02  8.618e-03   3.028  0.00275 **
# AgeInd       1.500e-03  2.116e-04   7.089 1.71e-11 ***
# ER          -2.424e-04  1.555e-04  -1.559  0.12042
# Illiteracy  -7.118e-04  4.372e-04  -1.628  0.10490
# Foreign      3.417e-05  3.135e-05   1.090  0.27695


mod.lin3 <- lm(FDI ~ AgeInd + ER + Illiteracy + Foreign)
summary(mod.lin3)
#             Estimate   Std. Error  t value  Pr(>|t|)
# (Intercept)  5.494e-02  1.213e-02   4.530 9.56e-06 ***
# AgeInd       7.228e-04  2.978e-04   2.427   0.0160 *
# ER          -4.296e-04  2.188e-04  -1.963   0.0509 .
# Illiteracy  -1.036e-03  6.153e-04  -1.683   0.0937 .
# Foreign      9.805e-05  4.413e-05   2.222   0.0273 *



#----------------
# regressione beta
#----------------

mod.beta <- flexreg(M5S ~ AgeInd + ER + Illiteracy + Foreign, type = "Beta", seed = 1986)
summary(mod.beta)

# Coefficients (mean model with logit link):
#               Post. Mean  Post. SD    2.5%    Post. Median   97.5%
# (Intercept)     1.3422     0.1732     1.0102       1.3438     1.6928
# AgeInd         -0.0039     0.0046     -0.0129      -0.0038    0.0047
# ER             -0.0342     0.0032    -0.0404      -0.0342    -0.0280
# Illiteracy      0.0270     0.0091     0.0095       0.0270     0.0450
# Foreign        -0.0022     0.0007    -0.0035      -0.0021    -0.0008
#
# Coefficients (precision model with identity link):
#   Post. Mean Post. SD    2.5% Post. Median    97.5%
#   phi    87.6203   8.2129 72.4661      87.3519 104.6941
#
# Waic method:
#   Computed from 2500 by 231 log-likelihood matrix
#
# Estimate   SE
# elpd_waic    368.3 12.4
# p_waic         7.4  1.4
# waic        -736.7 24.9
