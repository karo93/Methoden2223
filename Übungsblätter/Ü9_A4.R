#Aufgabe 4
#mit setwd("~/......") wird der Pfad bestimmt, wo der Datensatz zu finden ist
#load() lädt den Datensatz in die Umgebung
#a)
setwd("~/Documents/Lehre/Methoden der Ökonometrie ")
load("liverdata.rda")
#b) Plotten
plot(Length~Age, data=liver)
#c)
model1<-lm(log(Length)~Age+I(Age^2), data=liver)
model2<-lm(I(Length/(Age)^(3/2)) ~Age+I(Age^2), data=liver)
model3<-lm(Length ~ Age, data=liver)

#vergleiche das adj. R^2

summary(model1)$adj.r.squared
summary(model2)$adj.r.squared
summary(model3)$adj.r.squared



plot(model1$residuals ~ model1$fitted.values)
plot(model2$residuals ~ model2$fitted.values)
plot(model3$residuals ~ model3$fitted.values)

#ausgehend vom adj. R^2 würde man das erste modell präferieren

#d)
new<- data.frame(Age=1:40)
pred_m1 <- predict(model1, new)
?predict
lines(exp(pred_m1), col="blue")
pred_m2 <- predict(model2, new)
lines(pred_m2*new$Age^(3/2), col="green")
pred_m3 <- predict(model3, new)
abline(model3, col="red")
legend("topleft", c("Modell1", "Modell 2", "Modell 3"), fill= c("blue", "green","red"))


#Das adj. R^2 kann nicht verwendet werden um Modelle mit transformierten 
#und untransformierten y zu vergleichen!






