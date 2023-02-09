library(AER) 
data("USCrudes")
USCfit1 <- lm(price ~ gravity + sulphur, data = USCrudes)
USCfit2 <- lm(gravity ~ sulphur, data = USCrudes)
USCfit3 <- lm(price ~ -1 + USCfit2$residuals, data = USCrudes) # ohne Konstante coefficients(USCfit1)
coefficients(USCfit3)
#Der Koeffizient für gravity aus USCfit1 und USCfit3 sind gleich. Theoretisch haben wir dies auch schon auf dem letzten Übungsblatt 3 Aufgabe 2 (b) gezeigt, wobei in diesem Beispiel
#y = price, X1 = ( intercept, surplus) und X2 = gravity ist.

USCfit4 <- lm(price ~ sulphur, data = USCrudes)
USCfit5 <- lm(USCfit4$residuals ~ -1 + USCfit2$residuals, data = USCrudes) # ohne Konstante coefficients(USCfit1)
coefficients(USCfit5)
# Residuen sind unterschiedlich
plot(residuals(USCfit1) - residuals(USCfit3),
     main = "Model 1 vs Model 3", ylab = "difference")

# Residuen sind "identisch"
plot(residuals(USCfit1) - residuals(USCfit5),
     main = "Model 1 vs Model 5", ylab = "difference")

#Die Residduen von USCfit1 und USCfit3 sind nicht gleich. Dies haben wir auch im letzten Theorieblatt gezeigt. Die Residduen von USCfit1 und USCfit5 sind jedoch gleich, 
#was wir ebensfalls in der selben Aufgabe  gezeigt haben. Dies folgt aus dem FWL Theorem.

#Aufgabe 2

x <- 1:20
y <- x + rnorm(20)
X_all <- cbind(1, x, x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10)
beta1 <- list()

for(i in 1:10){
  X <- X_all[ ,1:(i + 1)]
  beta1[[i]]<- solve(t(X) %*% X) %*% t(X) %*% y
}
# Funktioniert nur bis x^7 
beta2 <- list() 
for(i in 1:10){
  X <- X_all[ ,1:(i + 1)]
  beta2[[i]]<- lm(y ~ X - 1) }
# Funktioniert für alle betrachteten Regressoren

#Aufagbe 3
my_lm <- function(X, y){ 
R <-qr.R(qr(X))
Q <-qr.Q(qr(X))
q <- t(Q) %*% y
backsolve(R, q) 
}
# my_lm() vs lm()
x <- 1:20
y <- x + rnorm(20)
X <- cbind(1, x, x^2, x^3)
my_lm(X[ ,1:3], y)
lm(y ~ X[ ,1:3] -1)

#Aufgabe 4

set.seed(12)
df <- data.frame(
  x = runif(200, 0, 10) )
df$y <- 4 + 3.5 * df$x + rnorm(200, 0, 20)

# Zufällig 150 Beobachtungen auswählen
id_in <- sample(1:200, 150)

# Übrigen 50 Beobachtungen 
id_out <- setdiff(1:200, id_in)
mod <- lm(y ~ x, data = df[id_in, ]) # In-sample Prognosen
pred_in <- fitted(mod)
# Out-of-sample Prognosen
pred_out <- predict(mod, df[id_out, ])

#Berechnen den MSE für die in-sample und die out-of-sample Prognosen.

mse <- function(true, pred){ mean((true - pred)^2) } 
# In-sample MSE
(mse_in <- mse(df$y[id_in], pred_in))
#Out-of-sample MSE
(mse_out <- mse(df$y[id_out], pred_out))
