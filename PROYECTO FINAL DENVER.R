library(readr)
DatosDenver <- read_csv("C:/Users/pasqu/Downloads/Denver_b.csv", 
                            col_types = cols(...1 = col_skip()))
View(DatosDenver)

moda = function(DatosDenver){
  tab = table(DatosDenver)
  return(as.numeric(names(tab)[tab==max(tab)]))
}
rango = function(DatosDenver){
  tab = sort(DatosDenver)
  return( tail(tab, n=1) - head(tab, n=1) )
}
info = function(DatosDenver){
  return(c(summary(DatosDenver), Moda=moda(DatosDenver), Rango=rango(DatosDenver), Varianza=var(DatosDenver), Desv.Est.=sd(DatosDenver), RIQ=IQR(DatosDenver)))
}

#PREGUNTA 1. Realizar un analisis descriptivo de los datos.
#X3
info(DatosDenver$X3)
hist(DatosDenver$X3, main="% de niños menores de 18 años", ylab = "Frecuencias" , xlab = "% de niños menores de 18 años", right = F, col="blue")
boxplot(DatosDenver$X3, main="% de niños menores de 18 años" , col="blue")
#X5
info(DatosDenver$X5)
hist(DatosDenver$X5, main="Cambio porcentual ultimos 5 años", ylab = "Frecuencias" , xlab = "Cambio porcentual ultimos 5 años", right = F, col="orange")
boxplot(DatosDenver$X5, main="Cambio porcentual ultimos 5 años" , col="orange")
#X6
info(DatosDenver$X6)
hist(DatosDenver$X6, main="Tasa crimen por cada mil habs", ylab = "Frecuencias" , xlab = "Tasa crimen por cada mil habs", right = F, col="green")
boxplot(DatosDenver$X6, main="Tasa crimen por cada mil habs" , col="green")

#PREGUNTA 2. Realice un intervalo de confianza del 97 % para la media de cada variable en estudio
#Para X1
t.test(DatosDenver$X1, conf.level=0.97)$conf.int
#Para X2
t.test(DatosDenver$X2, conf.level=0.97)$conf.int
#Para X3
t.test(DatosDenver$X3, conf.level=0.97)$conf.int
#Para X4
t.test(DatosDenver$X4, conf.level=0.97)$conf.int
#Para X5
t.test(DatosDenver$X5, conf.level=0.97)$conf.int  
#Para X6
t.test(DatosDenver$X6, conf.level=0.97)$conf.int
#Para X7
t.test(DatosDenver$X7, conf.level=0.97)$conf.int

#Ahora, para saber la media

#Para X1
mean(DatosDenver$'X1')
#Para X2
mean(DatosDenver$'X2')
#Para X3
mean(DatosDenver$'X3')
#Para X4
mean(DatosDenver$'X4')
#Para X5
mean(DatosDenver$'X5')
#Para X6
mean(DatosDenver$'X6')
#Para X7
mean(DatosDenver$'X7')

#PREGUNTA 3. Prueba por hipotesis de que la media de X7 es negativa, con Ho=0.
t.test(DatosDenver$X7, mu = 0, alternative = "less", conf.level = 0.95)

#PREGUNTA 4. Prueba de bondad de ajuste para determinar si la variable X7 tiene distribuci ́on
#normal.

VariableX7 = DatosDenver$X7

#Ahora, hagamos la prueba de una distribucion normal

shapiro.test(VariableX7)


#Pregunta 5. Grafico de dispersion  y matriz de correlacion

cor(DatosDenver)
plot(DatosDenver)

#Pregunta 6. Estudiar si la correlacion entre X2 y X7 es positiva.
cor.test(DatosDenver$X2, DatosDenver$X7)

#Pregunta 7. muestreo para dividir los datos en dos subconjuntos, uno con 80 % y 20 % de los datos.
set.seed(101)
tam <- floor(0.8*nrow(DatosDenver))
muestreo=sample(seq_len(nrow(DatosDenver)),size=tam)
muestreo_80P=DatosDenver[muestreo,]
muestreo_20P=DatosDenver[-muestreo,]

m80pct <- muestreo_80P
m20pct <- muestreo_20P

m20pct

m80pct

#Pregunta 8. Con el subconjunto del 80 % de los datos, halle un modelo lineal que explique mejor la variable X7. Incluya todas las pruebas necesarias para llegar a este modelo, as ́ı como un an ́alisis de resid duoelmodelo final.
m1= lm(X7 ~ X1+X2+X3+X4+X5+X6, m80pct);
summary(m1);
###Como X1 y X5 tienen los p-valores más cercanos a 1, así que podemos eliminarlos
m2= lm(X7 ~ X2+X3+X4+X6, m80pct);
summary(m2)
###Como vemos, debemos eleminar X2 y X6 y asi llegar al modelo ajustado
m3= lm(X7 ~ X3+X4, m80pct);
summary(m3)

## Análisis de residuos
par(mfrow = c(2,2))
plot(m3)

