##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTAD?STICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

rm(list = ls())

## CARGAR PAQUETES Y RUTA DE TRABAJO ##
library(rafalib)

path = "C:/.../"
setwd(path)

## DATOS ##
lizard = c(6.2, 6.6, 7.1, 7.4, 7.6, 7.9, 8, 8.3, 8.4, 8.5, 8.6,
           8.8, 8.8, 9.1, 9.2, 9.4, 9.4, 9.7, 9.9, 10.2, 10.4, 10.8,
           11.3, 11.9)

## FUNCIONES ##
var.interval <- function(data, conf.level = 0.95) {
  df = length(data) - 1#grados de libertad
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = var(data)
  c(df * v/chiupper, df * v/chilower)
}

norm.interval <- function(data, variance = var(data), conf.level = 0.95){
  z = qnorm((1 - conf.level)/2, lower.tail = FALSE)
  xbar = mean(data)
  sdx = sqrt(variance/length(data))
  c(xbar - z * sdx, xbar + z * sdx)
}

get.conf.int <- function(x){
  return(t.test(x)$conf.int)
}

## APLICAR LAS FUNCIONES ##

norm.interval(lizard, 2, conf.level = 0.99)
var.interval(lizard, conf.level = 0.975)

## PAR?METROS DEFINIDOS ##
n.draw = 100
mu = 9
n = 24
SD = sd(lizard)

draws = matrix(rnorm(n.draw * n, mu, SD), n)

conf.int <- apply(draws, 2, get.conf.int)
sum(conf.int[1, ] <= mu & conf.int[2, ] >= mu)

## GR?FICO DE INTERVALOS DE CONFIANZA ##

plot(range(conf.int), 
     c(0, 1 + n.draw), 
     type = "n", 
     xlab = "Longitud Promedio",
     ylab = "N? Corrida",
     main = "Intervalo de Confianza")

for(i in 1:n.draw){
  lines(conf.int[, i], rep(i, 2), lwd = 2)
  abline(v = 9, lwd = 2, lty = 2)
}

## OTRO EJEMPLO ##

datos <- read.csv("mice_pheno.csv")
chowPopulation <- datos[datos$Sex=="F" & datos$Diet=="chow",3]

mu_chow <- mean(chowPopulation)
print(mu_chow)

N <- 30
chow <- sample(chowPopulation,N)
print(mean(chow))

se <- sd(chow)/sqrt(N)
print(se)

pnorm(2) - pnorm(-2)

Q <- qnorm(1- 0.05/2)
interval <- c(mean(chow)-Q*se, mean(chow)+Q*se )
interval

interval[1] < mu_chow & interval[2] > mu_chow

# Sin embargo, podemos tomar otra muestra y es posible que no tengamos tanta suerte. 
# De hecho, la teor?a nos dice que cubriremos ??X el 95% del tiempo. 
# Debido a que tenemos acceso a los datos de la poblaci?n, 
# podemos confirmar esto tomando varias muestras nuevas:

N <- 100
B <- 250
Q <- qnorm(1- 0.05/2)
chow <- sample(chowPopulation,N)

mypar()
plot(mean(chowPopulation)+c(-7,7),
     c(1,1),
     type="n",
     xlab="weight",
     ylab="interval",
     ylim=c(1,B))
abline(v=mean(chowPopulation))

for(i in 1:B){
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- 
    mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}

## Tama?o de muestra peque?o y el CLT
## Para N = 30, el CLT funciona muy bien. 
## Sin embargo, si N = 5, 
## ?funcionan estos intervalos de confianza tambi?n? 
## Utilizamos el CLT para crear nuestros intervalos, y 
## con N = 5 puede que no sea una aproximaci?n tan ?til. 
## Podemos confirmar esto con una simulaci?n:

mypar()
plot(mean(chowPopulation)+c(-7,7),
     c(1,1),
     type="n",
     xlab="weight",
     ylab="interval",
     ylim=c(1,B))
abline(v=mean(chowPopulation))

Q <- qnorm(1- 0.05/2)
N <- 5
for (i in 1:B) {
  chow <- sample(chowPopulation,N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se)
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}

## A pesar de que los intervalos son m?s grandes 
## (estamos dividiendo por 5 - ??? en lugar de 30 ?????? ???), 
## vemos muchos m?s intervalos que no cubren ??X. 
## Esto se debe a que el TCL nos dice incorrectamente que la distribuci?n de la media (comida) 
## es aproximadamente normal cuando, de hecho, tiene una cola m?s gruesa 
## (las partes de la distribuci?n van a ? ???). 
## Este error nos afecta en el c?lculo de Q, que asume una distribuci?n normal y usa qnorm. 
## La distribuci?n t podr?a ser m?s apropiada. 
## Todo lo que tenemos que hacer es volver a ejecutar lo anterior, 
## pero cambiar la forma en que calculamos Q para usar qt en lugar de qnorm.

mypar()
plot(mean(chowPopulation) + c(-7,7), 
     c(1,1), 
     type="n",
     xlab="weight", 
     ylab="interval", 
     ylim=c(1,B))
abline(v=mean(chowPopulation))

## ya no es normal, as? que usa:
Q <- qt(1- 0.05/2, df=4)
N <- 5

for(i in 1:B){
  chow <- sample(chowPopulation, N)
  se <- sd(chow)/sqrt(N)
  interval <- c(mean(chow)-Q*se, mean(chow)+Q*se )
  covered <- mean(chowPopulation) <= interval[2] & mean(chowPopulation) >= interval[1]
  color <- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
}

## Limpieza de todos los objetos ##

rm(list = ls())

