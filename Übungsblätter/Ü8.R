#Aufgabe 1 
#a)
set.seed(1)
y<-rnorm(150, 4, 5)
#b)
beta_ols<- lm(y~1) #da nur eine Variable 

#c)
n<-150
w <- c(rep((1+0.5)/n, n/2),rep((1-0.5)/n, n/2))
beta_tilde <- sum(y* w)

#d)
N<-5000
beta_ols <-numeric(N)
beta_tilde <-numeric(N)

for(i in 1:N){
  y<-rnorm(n,4,5)
  beta_ols[i] <- coefficients(lm(y~1))
  beta_tilde[i] <- sum(y*w)
}
plot(density(beta_ols), col="blue")
lines(density(beta_tilde), col="red")
#extra:legende
legend("topright", c(expression(hat(beta)), expression(tilde(beta))),
fill = c("blue", "red"))
# Die Enden der roten Kurve sind immer über der der blauen Kurve -> Ols Schätzer ist effizienter.
#Alternativ: Vergleiche Varianz beider Schätzer
var(beta_ols)
var(beta_tilde)

#Aufgabe 2

# 1.Fall sigma ist bekannt
#Konstruiere ein Zahlenbeispiel

set.seed(1)
n <- 10
sigma <- 4
beta <- 3
beta_h0 <- beta # Da H0 wahr ist
z_hat <- numeric(10000)
for(i in 1:10000){
  y<- beta + rnorm(n, 0, sigma)
  beta_hat <-mean(y)
  z_hat[i] <- (beta_hat-beta_h0) * sqrt(n)/(sigma) # Formel auf Folie 4-3
}

plot(density(z_hat), col="blue")  
curve(dnorm, -4,4, add=T, col="red")  
legend("topright", c("MC-Verteilung","N(0,1)"), fill = c("blue", "red"))

#2. Fall sigma ist unbekannt

set.seed(1)
n <- 10
sigma <- 4
beta <- 3
beta_h0 <- beta # Da H0 wahr ist

t_hat <- numeric(10000)
for(i in 1:10000){
  y<- beta + rnorm(n, 0, sigma)
  beta_hat <-mean(y)
  t_hat[i] <- (beta_hat-beta_h0) * sqrt(n)/(sd(y)) # Formel auf Folie 4-3
}

plot(density(t_hat),xlim=c(-4,4), col="blue")  
curve(dnorm, -4,4, add=T, col="red") 
curve(dt(x,df=n-1), -4,4, col="green", add=T)
legend("topright", c("MC-Verteilung","N(0,1)", "t(n-1)"), fill = c("blue", "red", "green"))


#Aufgabe 3
set.seed(1)
#1.
p1<-numeric(10000)

for(i in 1:10000){
  y <- rnorm(500)
  t <- mean(y)/ (sd(y) / sqrt(500)) # vgl. Formel für T-Statistik für beta=0
  p1[i] <- 2* (1-pnorm(abs(t))) # vgl. Folie 4-10
}

hist(p1)
# p-wert ist unter der H0 uniformverteilt bzw. gleichverteilt 

#2. 
set.seed(1)
#1.
p2<-numeric(10000)

for(i in 1:10000){
  y <- 0.03+ rnorm(500)
  t <- mean(y)/ (sd(y) / sqrt(500)) # vgl. Formel für T-Statistik für beta=0
  p2[i] <- 2* (1-pnorm(abs(t))) # vgl. Folie 4-10
}

hist(p2,500)

# p-werte werden wenn H0 nicht wahr ist, immer kleiner 
