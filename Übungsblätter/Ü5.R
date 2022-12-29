# Aufgabe 1 Blatt 5
set.seed(1)
female <-ifelse(runif(10000)>=0.5, 1, 0)
ability <- rnorm(10000)
discrimination <- female
occupation <- 1 + 2*ability + 0*female - 2*discrimination + rnorm(10000)
wage <- 1 - 1*discrimination + 1*occupation + 2*ability + rnorm(10000)

mod1<- lm(wage~female)
mod2<-lm(wage~female + occupation)
mod3<-lm(wage~female+occupation+ability)

#Aufgabe 2
# leerer Vektor beta1_hat als Platzhalter für geschätzen Parameter
beta1_hat <-numeric(100)
#Simutaion
for( i in 1:100){
  x1<-runif(100, min=0, max=10)
  x2<-runif(100, min=0, max=10)
  eps<- rnorm(100)
  y<-3 + 4*x1+3*x2+eps

#Model Schätzen
mod<- lm(y ~x1)
beta1_hat[i] <- mod$coefficients[2]
}
mean(beta1_hat)
# Der Wert ist sehr nahe am wahren Wert beta1, also kann man annehmen, dass der Schätzer
#unverzerrt ist. 


#Aufgabe b)

beta1_hatb <-numeric(100)
#Simutaion

for( i in 1:100){
  x1<-runif(100, min=0, max=10)
  x2<-0.2 * x1 + runif(100, 0, 10)
  eps<- rnorm(100)
  y<-3 + 4*x1+3*x2+eps
  
  #Model Schätzen
  modb<- lm(y ~x1)
  beta1_hatb[i] <- modb$coefficients[2]
}
mean(beta1_hatb)

# Diesmal weicht der Wert erheblich vom wahren Wert ab, also ist 
#der Schätzer vermutlich verzerrt. 


