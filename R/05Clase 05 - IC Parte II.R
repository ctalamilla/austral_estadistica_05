##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

## TEOREMA CENTRAL DEL LÍMITE ##

rm(list = ls())

n <- 25
muestra1 <- rnorm(n,170,12)
media1 <- mean(muestra1)

muestra2 <- rnorm(n,170,12)
media2 <- mean(muestra2)

muestra3 <- rnorm(n,170,12)
media3 <- mean(muestra3)

tabla = rbind(media1, media2, media3)
colnames(tabla) = c("Promedio")
tabla

## CREAMOS UNA FUNCIÓN QUE CAPTE ESTA DINAMICA ##
mediaMuestral <- function(n,mu,sigma){
  muestra <- rnorm(n,mu,sigma)
  media <- mean(muestra)
  return(media)
}

mediaMuestral(25,170,12)
mediaMuestral(25,170,12)
mediaMuestral(25,170,12)

## SE REPITE 10000 VECES
m <- 10000
muchasMedias <- replicate(m,mediaMuestral(25,170,12))

## TOMAMOS LAS PRIMERAS 20 ##
muchasMedias[1:20]

## PROMEDIO Y DESVIO DE LAS 10000 MUESTRAS
mean(muchasMedias)
sd(muchasMedias)

## GRAFICAMOS MEDIANTE UN HISTOGRAMA ##
hist(muchasMedias,
     xlab = "Media muestral", 
     ylab = "Frecuencia", 
     col  = "lightcyan",
     xlim = c(160,180),
     freq = FALSE,
     ylim = c(0,0.75),
     main = "Medias Muestrales",
     sub  = "10000 muestras de tamaño 25")

curve(dnorm(x,170,sd(muchasMedias)),xlim=c(160,180),col="blue",lwd=2,add=TRUE)

muchasMedias50 <- replicate(m,mediaMuestral(50,170,12))
muchasMedias100 <- replicate(m,mediaMuestral(100,170,12))
muchasMedias500 <- replicate(m,mediaMuestral(500,170,12))

mean(muchasMedias50); sd(muchasMedias50)
mean(muchasMedias100); sd(muchasMedias100)
mean(muchasMedias500); sd(muchasMedias500)

## GRAFICO CON 50

hist(muchasMedias50,
     xlab = "Media muestral", 
     ylab = "Frecuencia", 
     col  = "lightcyan",
     xlim = c(160,180),
     freq = FALSE,
     ylim = c(0,0.75),
     main = "Medias Muestrales",
     sub  = "10000 muestras de tamaño 50")

curve(dnorm(x,170,sd(muchasMedias50)),
      xlim=c(160,180),
      col="blue",
      lwd=2,
      add=TRUE)

## GRAFICO CON 100

hist(muchasMedias100,
     xlab = "Media muestral", 
     ylab = "Frecuencia", 
     col  = "lightcyan",
     xlim = c(160,180),
     freq = FALSE,
     ylim = c(0,0.75),
     main = "Medias Muestrales",
     sub  = "10000 muestras de tamaño 100")

curve(dnorm(x,170,sd(muchasMedias100)),
      xlim=c(160,180),
      col="blue",
      lwd=2,
      add=TRUE)

## GRAFICO CON 500

hist(muchasMedias500,
     xlab = "Media muestral", 
     ylab = "Frecuencia", 
     col  = "lightcyan",
     freq = FALSE,
     main = "Medias Muestrales",
     sub  = "10000 muestras de tamaño 500")

curve(dnorm(x,mean = mean(muchasMedias500),sd(muchasMedias500)),
      xlim=c(160,180),
      col="blue",
      lwd=2,
      add=TRUE)

library(lessR)
Histogram(muchasMedias, density = T, type = "normal")
Histogram(muchasMedias50, density = T, type = "normal")
Histogram(muchasMedias100, density = T, type = "normal")
Histogram(muchasMedias500, density = T, type = "normal")

## TAREA RECOMENDADA
## Vimos la media de variables normales se distribuye como una Normal. 
## ¿Ocurrirá lo mismo si las variables que se promedian no son normales? 
## Podemos aquí plantear el siguiente ejercicio, para que "descubran" 
## el teorema central del límite.

## Repetir el proceso anterior cuando la variable aleatoria original 
## Poisson de parámetro lambda =1.3
## Idem cuando es exponencial de parámetro beta =1.5
## Idem cuando es uniforme en el intervalo [5,10]
## Idem cuando es de Weibull de parámetros shape = 1.2 y  scale = 0.5.