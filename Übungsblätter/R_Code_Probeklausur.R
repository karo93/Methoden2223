### Aufgabe 2 c)

set.seed(123)

n <- 25                            # n=25 Beobachtungen
x1 <- runif(n)                     # Kovariable x1 wird aus einer Gleichverteilung im Intervall [0,1] gezogen mit n Beobachtungen
x2 <- rep(1,n)                     # x2 ist der Einservektor mit n=25 Beobachtungen
u <- rnorm(n, sd=.5)               # Fehlervektor u ist aus einer Normalverteilung mit n=25 Beobachtungen, Erwartungswert 0 und VARIANZ 0.25
                                   # also Standardabweichung 0.5! -> Hier kann es schnell zu Fehlern kommen!

beta <- 2                          # beta ist 2
gamma <- 0                         # gamma ist 0


# Wahren Beobachtungen y aus dem wahren Modell berechnen: 
y <- beta*x1 + gamma*x2 + u 

# Koeffizienten des Modells mit OLS schätzen:

# Kurze Regression (y wird nur auf x1 regressiert, daher -1 um Achsenabschnitt zu unterdrücken)
short.reg <- lm(y ~ x1-1)
beta_kurz <- coef(short.reg)
# beta=1.8626 mit p-Wert=3.65e-12

# Lange Regression (y wird regressiert auf x1 und x2 (da x2 bereits Einservektor auch hier - 1 um Achsenabschnitt zu unterdrücken))
long.reg <- lm(y ~ x1+x2-1)
beta_lang <- coef(long.reg)[1]

### weitere Möglichkeiten die lange Regression zu machen: 
lm(y ~ x1+x2) # Koeffizient für x2=NA, da Achsenabschnitt drin
lm(y~x1)      # Achsenabschnitt automatisch drin, daher Angabe von x2 nicht nötig


### Wiederholung 1000 mal: 

#Leerer Vektor mit 1000 Einträgen für beta_kurz:
beta_short <- numeric(1000)
#Leerer Vektor mit 1000 Einträgen für beta_lang:
beta_long <- numeric()

for (i in 1:1000)
{
  # x2, beta, gamm fix, daher nicht mit in der Schleife
  x1 <- runif(n)                     
  u <- rnorm(n, sd=.5)               
  # Wahren Beobachtungen y aus dem wahren Modell berechnen: 
  y <- beta*x1 + gamma*x2 + u 
  # Kurze Regression (y wird nur auf x1 regressiert, daher -1 um Achsenabschnitt zu unterdrücken)
  short.reg <- lm(y ~ x1-1)
  beta_short[i] <- coef(short.reg)   # der Koeffizient wird an der i-ten Stelle im Vektor beta_short gespeichert
  # Lange Regression (y wird regressiert auf x1 und x2 (da x2 bereits Einservektor auch hier - 1 um Achsenabschnitt zu unterdrücken))
  long.reg <- lm(y ~ x1+x2-1)
  beta_long[i] <- coef(long.reg)[1]  # der Koeffizient wird an der i-ten Stelle im Vektor beta_short gespeichert
}

### Stichrobenvarianzen berechnen: 
var(beta_short)
var(beta_long)

### Der Schätzer der langen Regression (zusätzliche nicht relevante Variablen eingefügt) hat eine größere Varianz
### als der Schätzer der kurzen "wahren" Regression ohne zusätzliche irrelevante Variablen. 
### Zusätzliche Regressoren reduzieren das Risiko des omitted variable bias, was (sehr) vorteilhaft ist.
### Auf der anderen Seite verringern sie die Präzision des Schätzers was weniger gut ist.

#d) Wenn x1 ein Mittelwert von Null hätte, dann wären x1 und x2 orthogonal zueinander, 
# sodass die Präzision des Schätzers nicht beeinträchtigt wird.

### Aufgabe 3

set.seed(123)
###a) kleines Beispiel zur visualisiering, wieso Beobachtungen kopieren nichts bringt
n<- 100
u <- rnorm(n)                  # Fehlervektor u  
beta <- c(1,2)                 # Vektor beta
X <- cbind(rep(1,n),rnorm(n))  # Matrix X
y <- X%*%beta + u # Beobachtungen y generieren
#nun fügen wir mehr Beobachtungen hinzu
X_tilde <- rbind(X,X)
y_tilde <- rbind(y,y)
beta_ols_tilde <-lm(y_tilde~X_tilde-1)
beta_ols <-lm(y~X-1)
beta_ols_tilde$coefficients
beta_ols$coefficients #beide werte sind gleich 

### b)
n <- 20                        # n=20 Beobachtungen
u <- rnorm(n)                  # Fehlervektor u  
X <- cbind(rep(1,n),rnorm(n))  # Matrix X
beta <- c(1,2)                 # Vektor beta

y <- X%*%beta + u   # Beobachtungen y generieren

# Koeffizienten schätzen
orig.reg <- lm(y~ X -1)
summary(orig.reg)

### c)
copy.factor <- -3

y.stern <- c(y, copy.factor*y)
X.stern <- rbind(X, copy.factor*X)

copy.reg <- lm(y.stern ~ X.stern -1)
summary(copy.reg)
summary(orig.reg)

### Geschätzte Koeffizenten ändern sich nicht! Bleiben gleich!

### d)
SSR.orig <- sum(resid(orig.reg)^2)
SSR.copy <- sum(resid(copy.reg)^2)

(1+copy.factor^2)*SSR.orig
# Residuenqudratsumme ändert sich genau um (1 + c^2) -> SSR wird größer! 


### e)

#Modell1
sqrt(diag(SSR.orig/(n-2)*solve(t(X)%*%X)))[2]
#Modell2
sqrt(diag(SSR.copy/(2*n-2)*solve(t(X.stern)%*%X.stern)))[2]

sqrt((n-2)/(2*n-2))*sqrt(diag(SSR.orig/(n-2)*solve(t(X)%*%X)))[2]

### Zusammenhang stimmt

#### Aufgabe 4

###a)
# Lineares Modell aufstellen
modell <- lm(LNC ~ LNP + LNY, data=cigarette)
summary(modell)
residuen <- modell$res
par(mfrow=c(1,1))
plot(cigarette$LNP,residuen)
# Es liegt ein Ausreißer vor ganz links im Plot ist ein Wert mit LNP<0, alle anderen >0. 
plot(cigarette$LNY,residuen)
# Es könnte Heteroskedastie vorliegen, für kleine Werte von LNY große Varianz, für große Werte eher kleinere Varianz

#Beim zweiten Plot ist Heteroskedastie zu erkennen; mit größerem LNY sinkt anscheinend die Varianz der Störgrößen. Es ist also fraglich,
#ob die Annahme der Homoskedastie sinnvoll ist.
#Im ersten Plot erkennen wir links oben einen Ausrei?er; dies ist der einzige Wert mit negativem LNP. 

###b)

summary(modell)$coefficients[2,4]<0.05
### H0 ablehnen

###Heteroskedastierobuste Standardfehler
library(sandwich)
library(lmtest)
robust_var <-vcovHC(modell, type = "HC1") # robuste Kovarianzmatrix sch?tzen
coeftest(modell, vcov. = robust_var)[2,4]<0.05
### H0 ebenfalls ablehnen

###c)
cigarette_s?d <- cigarette[cigarette$south==1,]
cigarette_ohnes?d <- cigarette[cigarette$south==0,]

suedmodell <- lm(LNC ~ LNP, data=cigarette_s?d)
restmodell <- lm(LNC ~ LNP, data=cigarette_ohnes?d)

## Durchführung des Chow-Tests

n <- length(cigarette$LNC)

CHOWz <- (sum(modell$res^2) - (sum(suedmodell$res^2)+sum(restmodell$res^2))) / 2
CHOWn <- (sum(suedmodell$res^2)+sum(restmodell$res^2))/(n-2*2)

CHOW <- CHOWz/CHOWn
pwert <- 1 - pf(CHOW,2,n-2*2) 
## 0.2204799, kein Hinweis auf einen Strukturbruch

###d)
cigarette[which(hatvalues(modell)> 3*mean(hatvalues(modell))),]
cigarette_filtered <- cigarette[-15,]

modell_filtered <- lm(LNC ~ LNP + LNY, data=cigarette_filtered)
summary(modell_filtered)
summary(modell)
### Koeffizient für LNP wird kleiner (etwas gr??erer Effekt da negativ)
### p-Wert für LNP wird kleiner 
### Ausreißer könnte Schätzung etwas verzerrt haben + gröere Varianz, größerer p-Wert





