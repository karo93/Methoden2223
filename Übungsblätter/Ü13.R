library(mvtnorm)
set.seed(123)

coef1 <- matrix(nrow=1000, ncol=3)
coef2 <- matrix(nrow=1000, ncol=3)

SE1 <- matrix(nrow=1000, ncol=3)
SE2 <- matrix(nrow=1000, ncol=3)

for(i in 1:1000){
  
  # mitterlwert der X
  mu <- c(0.7, 0.3)
  
  # korrelierte X 
  Sigma1 <- matrix(c(1,0.99,0.99,1),nrow=2)
  
  # unkorrelierte  X
  Sigma2 <- matrix(c(1,0,0,1),nrow=2)
  
  X1 <- rmvnorm(n=100,mean = mu, sigma = Sigma1)
  X2 <- rmvnorm(n=100,mean = mu, sigma = Sigma2)
  
  y1 <- 3 + X1[,1] + X1[,2] + rnorm(100)
  y2 <- 3 + X2[,1] + X2[,2] + rnorm(100)
  
  lm_cor <- lm(y1 ~ X1)
  lm_no_cor <- lm(y2 ~ X2)
  
  coef1[i,] <- lm_cor$coefficients
  coef2[i,] <- lm_no_cor$coefficients
  
  SE1[i,] <- summary(lm_cor)$coefficients[,"Std. Error"]
  SE2[i,] <- summary(lm_no_cor)$coefficients[,"Std. Error"]
  
}

colMeans(SE1)
colMeans(SE2)


par(mfrow = c(3, 1))

plot(density(coef1[,1]),ylim=c(0,3.7), xlim=c(-1,4),col="steelblue")
lines(density(coef2[,1]))

plot(density(coef1[,2]),ylim=c(0,3.7), xlim=c(-1,4),col="steelblue")
lines(density(coef2[,2]))


plot(density(coef1[,3]),ylim=c(0,3.7),xlim=c(-1,4),col="steelblue")
lines(density(coef2[,3]))

# Wir stellen fest, dass die Varianz des OLS-Sch?tzers im Fall 
# hoher Korrelation unter den Regressoren viel gr??er ist als im Fall
# geringer Korrelation. Generell tendieren Regressionsmodelle mit hoher
# Multikollinearit?t zu gro?en Standardfehlern der Koeffizienten, wodurch
# z.B. die Macht statistischet Tests beeinflusst wird. 

# Aufgabe 2 

set.seed(1)
reject_multiple_t <- logical(100)
reject_F <- logical(100)

for(i in 1:100){
  X <- matrix(rexp(n=20*100,rate = 1),nrow=100, ncol=20)
  y <- rnorm(n=100, mean=20,sd = 1)
  
  fit <- summary(lm(y~X))
  reject_multiple_t[i] <- any(fit$coefficients[-1,4] <= 0.05)
  reject_F[i] <- (1-pf(fit$fstatistic[1],20,79)) <= 0.05
  
}

mean(reject_multiple_t)
mean(reject_F)

# Das beschriebene Vorgehen f?hrt zu dem Problem des multiplen Testens. 
# Bei jedem individuellen Test ist unter H0 die W'keit einen Fehler 1. Art
# zu begehen alpha*100% sein. Beim Durchf?hren vieler Tests vergr??ert sich die
# W'keit mindetsens einen fehler zu machen (alpha-Fehler Kumuliereung!) Bei 20 
# individuellen Tests liegen wir nur noch mit einer W'keit von (1-alpha)^20*100%
# bei allen Tests richtig. 

# Ein sinnvoller Test f?r HA w?re der F-test aus der Vorlesung. Wie wir sehen 
# entspricht hier die empirische Verwerfungsrate ungef?hr dem was wir erwarten. 


# Aufgabe 3

library(car)
set.seed(1)

X <- matrix(rexp(n=20*100,rate = 1),nrow=100, ncol=20)
y <- rnorm(n=100, mean=20,sd = 1)

fit <- lm(y~X)


# H_A
R1 <- diag(21)[-1,]
# R1 %*% beta = (0,...,0)'

#1. M?glichkeit
linearHypothesis(fit, R1)

#2.M?glichkeit ?ber summary(fit)
summary(fit)

# H0 kann nicht abgelehnt werden zum Signifikanzniveau 5% 

# H_B 
R2 <- diag(21)[c(-1,-7:-21),]
# R2 %*% beta = (0,...,0)' <-> beta1 = 0 , beta2 =0, ...beta5=0

linearHypothesis(fit, R2)
# H0 kann nicht abgelehnt werden zum Signifikanzniveau 5%. 

# H_C

R3 <- rbind(rep(0,21),rep(0,21))
R3[1,c(2,3)] <- c(1,2)

#beta4 = beta8
#  R3_Zeile * beta=   (1*beta4 + (-1)* beta8 =0

R3[2,c(5,9)] <- c(1,-1)
myrhs <- c(0,0)

linearHypothesis(fit, R3,rhs=myrhs)

### p-Wert knapp ?ber 0.05 -> Nullhypothese kann zum 5%-Niveau nicht
### abgelehnt werden. 



# Aufgabe 4

library(lmtest)
library(sandwich)

#a) 
set.seed(1)
x <- runif(500, 0, 10)
y <- rnorm(500, 0, x)

#b) 
plot(y ~ x)

#c)
mod  <- lm(y~x)
summary(mod)     
# H0 wird bei einem p-Wert von 0.114 nicht verworfen. 
# Der Test kommt also zum richtigen Ergebnis. 

robust_var <-vcovHC(mod, type = "HC1") # robuste Kovarianzmatrix sch?tzen
mod_robust <- coeftest(lm(y~x), vcov. = robust_var)
mod_robust
# H0 wird bei einem p-Wert von  0.1595 ebenfalls nicht verworfen. 
# Der Test macht also keinen Fehler. 
# Die Tests liegen also beide richtig. 
# Das muss aber nicht immer so sein. In der n?chsten Aufgabe
# wiederholen wir beide Tests 1000 mal, 
# um herauszufinden, welcher Test h?ufiger verwirft. 

#d) 
p_value        <- numeric(length = 1000)
p_value_robust <- numeric(length = 1000)

set.seed(1)
for(i in 1:1000){
  x <- runif(500, 0, 10)
  y <- rnorm(500, 0, x)
  mod               <- lm(y~x) 
  p_value[i]        <- coefficients(summary(mod))[2,4]
  p_value_robust[i] <- coeftest(mod, vcov. = hccm(mod, type = "hc1"))[2,4] 
}

mean(p_value < 0.05)
mean(p_value_robust < 0.05)

# Der nicht robuste Test verwirft in 7.2% der F?lle. Also zu oft. 
# Bei einem Signifikanzniveau von 0.05
# w?rden wir erwarten, dass in nur 5% der F?lle verworfen wird. 
# Der robuste Test verwirft in etwa 5% der F?lle. 

#e) 
p_value        <- numeric(length = 1000)
p_value_robust <- numeric(length = 1000)

set.seed(1)
for(i in 1:1000){
  x   <- runif(500, 0, 500)
  y   <- 0.1 * x + rnorm(500, 0, x)
  mod <- lm(y~x) 
  coef <- coefficients(summary(mod))
  coef_robust       <- coeftest(mod, vcov. = vcovHC(mod))
  p_value[i]        <- coef[2,4]
  p_value_robust[i] <- coef_robust[2,4] 
}

mean(p_value < 0.05)
mean(p_value_robust < 0.05)

# In diesem Beispiel wurden die Daten unter der Alternativhypothese erzeugt 
# (also beta ungleich 0).
# Wir sehen, dass der robuste Test seltener verwirft und demzufolge eine 
# geringere Macht hat als der nicht robuste Test. 

###Aufgabe 5 
#Betrachte nochmal Aufgabe 4 von Blatt 9
#load("liverdata.rda")
#Folgende Modelle wurden erstellt
model1<-lm(log(Length)~Age+I(Age^2), data=liver)
model2<-lm(I(Length/(Age)^(3/2)) ~Age+I(Age^2), data=liver)
model3<-lm(Length ~ Age, data=liver)

#Plotte nun Residuen gegen gefittete Werte

plot(model1$residuals ~ model1$fitted.values)
plot(model2$residuals ~ model2$fitted.values)
plot(model3$residuals ~ model3$fitted.values)

#basierend auf den Plots würden wir uns für das erste Modell entscheiden, da dort die 
#Residuen gleichmäßiger um null herum liegen.
#Bei Modell 2 und Modell 3 streuen die daten teilweise stark auseinander


### Aufgabe 6: Strukturbruchanalyse

library(AER)
# Daten laden & plotten
data(faithful)
plot(waiting ~ eruptions,data=faithful)
# Es fällt auf, dass der Datensatz in 2 Gruppen unterteilt ist. 
# Eine mit einer kurzen und eines mit einer langen Eruptionsdauer.

# Teilen des Datensatzes in 2 Gruppen
lang <- faithful[faithful$eruptions > 3.25, ]
kurz <- faithful[faithful$eruptions < 3.25, ]

# Strukturbruchanalyse

# Sch?tzen der Modelle 
rmod     <- lm(waiting~eruptions,data=faithful) # restringiertes Modell
lang_mod <- lm(waiting~eruptions,data=lang)     # Modell mit Teilgruppe lang 
kurz_mod <- lm(waiting~eruptions,data=kurz)     # Modell mit Teilgruppe kurz 

# Plot mit allen 3 Regressionsgeraden
plot(waiting~eruptions,data=faithful)
abline(rmod, col="red")
abline(lang_mod, col="blue")
abline(kurz_mod, col="blue")

# Chow-Test
RSSR <- sum(rmod$residuals^2)      # oder deviance(rmod)
SSR1 <- sum(lang_mod$residuals^2)  # oder deviance(lang_mod)
SSR2 <- sum(kurz_mod$residuals^2)  # oder deviance(kurz_mod)

k <- rmod$rank

# Test-Statistik
chow <- ((RSSR - SSR1 - SSR2)/k) / ((SSR1+SSR2) / (nrow(faithful) - 2 * k))
chow

# p-Wert
1 - pf(chow, k, (nrow(faithful) - 2*k))

# => p-Wert ist kleiner als 0.05. Wir lehnen daher die H_0 ab:
# Der Test kommt somit zum Ergebnis, dass ein Strukturbruch vorliegt.


