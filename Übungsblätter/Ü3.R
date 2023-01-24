########## Übungsblatt 1 ###########
data(mtcars)

#Aufgabe 1
#a) 
X <- cbind(rep(1, nrow(mtcars)), mtcars$hp, mtcars$wt)
y <- mtcars$mpg

#Formel für beta_hat
solve(t(X)%*%X)%*%t(X)%*%y

#b) Unter benutzung des lm()-Befehls 

mod<-lm(mpg ~ hp + wt, data = mtcars)

#Aufgabe 2
#a)
set.seed(1)
x <- runif(150, min = 0, max = 15)
u <- rnorm(150, mean = 0, sd = 4) # Varianz = 16 => Standardabweichung = 4
y <- 7 + 3*x + 4*x^3 + u

#b)
plot(y ~ x)

#c)
mod1 <- lm(y ~ x + I(x^3)) # wir brauchen x und x^3, da der
# Zusammenhang zwischen x und y
# quadratisch ist.
# Einzeichen der Regressionslinie (keine Gerade)
index <- order(x)
lines(x[index], fitted(mod1)[index], col = "red")

######################

#Aufgabe 3

# Lösung
# a) und b)
# Platzhalter für Simulationsergebnisse erzeugen
x_bar <- numeric(20000)
s_square <- numeric(20000)
s_square_unbiased <- numeric(20000)
# Simulation
for(i in 1:20000){
  sample <- rnorm(15, 3, 5)
  x_bar[i] <- mean(sample)
  s_square_unbiased[i] <- var(sample)
}
# c) Graphische Darstellung
# Mögliche Wege wären z.B. ein Histogramm oder eine Kerndichteschätzung.
# Hier wird eine Kernedichteschätzung genutzt.
par(mfrow = c(1,1)) # setzt die folgenden Plots nebeneinander
plot(density(x_bar), main = "Dichte von x_bar", # relevant
     col = "steelblue", lwd = 2)
abline(v = 3, lwd = 2)
abline(v = mean(x_bar), col = "steelblue", lwd = 2, lty = "dashed")
legend("topright" ,legend = c("Dichte", "Mittelwert",
                              "Wahrer Wert"),
       col = c("steelblue", "steelblue", "black"),
       lwd = c("2", "2", "2"), lty = c(1,2,1))
plot(density(s_square_unbiased), main = "Dichte von s_square_unbiased", # relevant
     col = "steelblue", lwd = 2)
abline(v = 25, lwd = 2)
abline(v = mean(s_square_unbiased), col = "steelblue", lwd = 2, lty = 2)

legend("topright" ,legend = c("Dichte", "Mittelwert",
                              "Wahrer Wert"),
       col = c("steelblue", "steelblue", "black"),
       lwd = c("2", "2", "2"), lty = c(1,2,1))


?legend

#d)
data.frame(mean(x_bar), mean(s_square_unbiased))

#e)
s_square_unbiased <- numeric(20000)
# Simulation
for(i in 1:20000){
  sample <- rnorm(15, 3, 5)
  s_square[i] <- mean((sample - x_bar[i])^2)
}
plot(density(s_square), main ="Dichte von s_square", col = "steelblue", lwd = 2)
abline(v = 25, lwd = 2)
abline(v = mean(s_square), col = "steelblue", lwd = 2, lty = "dashed")
legend("topright" ,legend = c("Dichte", "Mittelwert",
                              "Wahrer Wert"),
       col = c("steelblue", "steelblue", "black"),
       lwd = c("2", "2", "2"), lty = c(1,2,1))

########
mean(s_square)

#Aufgabe 4
#install.packages("AER)
library(AER)
data(CPS1988)

#a)
summary(CPS1988)

#b) Regression ohne Konstante

mod2<-lm(wage ~ education + experience + ethnicity - 1, data = CPS1988)


#c)
MOD3<-lm(wage ~ education + experience + ethnicity, data = CPS1988)

#d)
 plot(mod2$fitted.values - MOD3$fitted.values)
#e)
 
 lm(wage ~ education:ethnicity + experience + ethnicity, data = CPS1988)

 