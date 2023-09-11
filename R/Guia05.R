#Ejercicio 1
#La profundidad de las piletas de lona que fabrica la empresa “PILETITA S.A.”, se distribuye normalmente con un desvío estándar de 5 mm. Para estimar la profundidad media de las piletas, se tomó una muestra de 38 piletas, calculándose una profundidad promedio de 1250 mm. Realizar la estimación, con una confianza del 99%.

#profundidad ~ N 
sd_poblacion <- 5
#estimar profundidad media de las piletas, 99%
tamano_muestra <- 38
media_muestra <- 1250


confianza <- 0.99 
alpha <- 1- confianza

1-alpha/2
(1+confianza)/2


#valor critico
valor_critico = qnorm((1+confianza)/2) #Z
valor_critico = qnorm(1-alpha/2)
#desvio del estimador
error_estandar <- sd_poblacion / sqrt(tamano_muestra) #sd/raiz(n)

limInf = media_muestra - valor_critico*error_estandar
limSup = media_muestra + valor_critico*error_estandar
IC <- c(limInf, limSup)
IC


# Ejercicio 2
#El costo variable de construcción de un determinado tipo de vivienda prefabricada, por metro cuadrado, se distribuye normalmente. Se tomó una muestra de 12 viviendas con las que se calculó un costo variable promedio de $1440 y un desvío estándar de $135.

#Costo_m2 ~ N
muestra <- 12
promedio_muestra <- 1440
sd_muestra <- 135

#solo hay datos de la muestra, APLICAR Distribucion T

#a. ¿Entre qué valores estará el costo variable promedio del producto si se lo estima con una confianza del 95%?

#estimar media con un 95% 
confianza <- 0.95
alpha <- 1-confianza
grados_libertad <- muestra -1

error_estandar <- sd_muestra / sqrt(muestra) #sd/raiz(n)


qt(1-(1-confianza)/2, df= grados_libertad)# otra manera de obtener el valor de T

valor_critico <- qt((1-alpha/2), df= grados_libertad)
valor_critico

limInf = promedio_muestra - valor_critico*error_estandar
limSup = promedio_muestra + valor_critico*error_estandar
IC <- c(limInf, limSup)
IC

#Ejercicio n° 3
#Una muestra de 248 bicicletas indicó que el 19% de ellas tenía problemas en los frenos. Con una confianza del 92%, estime la proporción de bicicletas con problema en los frenos, con una confianza del 92%

tamaño_muestra <- 248
prob_problemas <- 0.19
confianza <- 0.92
alpha <- 1- confianza

#desvio del estimador

desvio <- sqrt(prob_problemas*(1-prob_problemas)/tamaño_muestra)

valor_critico <- qnorm(1-alpha/2)

limInf = prob_problemas - valor_critico*desvio
limSup = prob_problemas + valor_critico*desvio
IC <- c(limInf, limSup)
IC


# Ejercicio 4
#En una tornería se fabrican cilindros cuyo diámetro se distribuye normalmente con un desvío estándar de 1.5 mm Con una muestra de 10 cilindros se estimó el diámetro medio entre 20,5 y 21,8. Calcule la confianza de la estimación.

desvio_poblacion <- 1.5
tamaño_muestra <- 10
lim_inf <- 20.5
lim_sup <- 21.8
media_poblacion = (lim_sup + lim_inf)/2
error_media <- desvio_poblacion/sqrt(tamaño_muestra)


pInf = pnorm(20.5,mean=media_poblacion , sd= error_media)
pSup = pnorm(21.8,mean=media_poblacion , sd= error_media)

confianza = pSup - pInf
confianza

#Ejercicio 5
#La longitud de los durmientes utilizados en la industria ferroviaria a fines del siglo XIX se distribuye normalmente con un desvío estándar de 2cm.

#a. Calcularcuálseráeltamañodelamuestramínimoparaestimarlalongitud media, si se quiere un error de muestreo de como máximo 0,3 cm., con una confianza del 92%.

# long_durmientes ~ N

desvio_poblacion = 2
error_muestreo = 0.3
confianza = 0.92
alpha = 1 - confianza

valor_critico = qnorm(1-alpha/2)

# error = valor critico * desvio_la_muestra
# error = Z(1-alpha/2) * desvio_poblacion/sqrt(n)
# n = ((valor critico * desvio_poblacion)/error)**2

n = (valor_critico * desvio_poblacion / error_muestreo)**2
n

#b. Ídem anterior, pero con una confianza del 97%.

confianza = 0.97
alpha = 1 - confianza

valor_critico = qnorm(1-alpha/2)
n = (valor_critico * desvio_poblacion / error_muestreo)**2
n

#Ejercicio 6
#Una importante marca de ropa deportiva, contrató a un equipo de especialistas que realizó una encuesta, a fin de estimar qué proporción de personas mayores de 65 años, aceptarían un nuevo producto que se planea lanzar al mercado.
#Calcule el tamaño de la muestra mínimo a utilizar si se quiere un error de muestreo máximo de 2%, y un nivel de confianza del 99%, en base a los siguientes supuestos:

#problema de proporcion
#calcular el tamaño de la muestra
error = 0.02
confianza = 0.99
alpha = 1-confianza

valor_critico = qnorm(1-alpha/2)
# resolucion de a 
p_media = 0.82 #trabajos anteriores

#error = valor critico * sqrt(prop_media * (1 - prop_media)/n)

# n = valor_critico ** 2 * prop_media * (1 - prop_media) / error **2

n = valor_critico**2 * (p_media*(1-p_media)) / error**2
n

# resolucion de a 
# p_media + 0.5
p_media = 0.5

n = valor_critico**2 * (p_media*(1-p_media)) / error**2
n


#Ejercicio 7
#En una fábrica situada en Pacheco se fabrican llantas de aleación para automóviles, cuyo diámetro se distribuye normalmente con un desvío típico de 2 mm.

# diametro ~ N

sd_poblacion = 2

# (a)
n = 22
media_muestra = 150
confianza = 0.9
alpha = 1-confianza
valor_critico = qnorm(1-alpha/2)

desvio_media = sd_poblacion / sqrt(n)

limInf = media_muestra - valor_critico*desvio_media
limSup = media_muestra + valor_critico*desvio_media
IC <- c(limInf, limSup)
IC

# (b) calcular la confianza

n= 18
limite_inferior = 148.41
limite_superior = 150.85

media_muestra = (limite_superior+limite_inferior)/2

desvio_delmedia <- sd_poblacion/sqrt(n)


pInf = pnorm(limite_inferior,mean=media_muestra , sd= desvio_delmedia)
pSup = pnorm(limite_superior,mean=media_muestra , sd= desvio_delmedia)

confianza = pSup - pInf
confianza

# (c)
confianza = 0.95
alpha = 1- confianza
error = 0.8



valor_critico = qnorm(1-alpha/2)
n = (valor_critico * sd_poblacion / error)**2
n

#Ejercicio 8
#Los siguientes datos corresponden a los pagos en concepto de impuesto a las ganancias, los cuales se distribuyen normalmente, que fueron realizados por seis contribuyentes en el último periodo fiscal:

#imp ~ N
imp = c(854.8 , 754.3 , 698.2 , 789.2 , 401.9 , 642.6)

n = length(imp)
media_imp = mean(imp)
sd_imp = sd(imp)

confianza = .95
alpha = 1-confianza
grados_libertad = n-1

desvio_delmedia <- sd_imp / sqrt(n) #sd/raiz(n)


qt(1-(1-confianza)/2, df= grados_libertad)# otra manera de obtener el valor de T

valor_critico <- qt((1-alpha/2), df= grados_libertad)
valor_critico

limInf = media_imp - valor_critico*desvio_delmedia
limSup = media_imp + valor_critico*desvio_delmedia
IC <- c(limInf, limSup)
IC

# Ejercicio 9 
#El peso de los paquetes de caramelos envasados automáticamente se distribuye normalmente con un desvío de 1 gramos. Una muestra de 19 paquetes dio como resultado un peso promedio de 138 gramos. Estime, con una confianza del 95%, el peso medio de los paquetes.

# peso ~ N
sd_poblacion = 0.1
n = 19
media_muestra = 138
confianza =0.95
alpha = 1-confianza

desvio_delmedia = sd_poblacion/sqrt(n)
valor_critico = qnorm(1-alpha/2)

limInf = media_muestra - valor_critico*desvio_delmedia
limSup = media_muestra + valor_critico*desvio_delmedia
IC <- c(limInf, limSup)
IC

#Ejercicio 10

#El dueño de una sodería está interesado en estimar algunos parámetros relacionados con su inventario actual. De un lote de 250 sifones, se tomaron al azar 60 sifones y surgió la siguiente información:
#Se encontraron 22 sifones defectuosos, se determinó que el contenido medio era 950 cm. cúbicos con un desvío de 18 cm. cúbicos.

N = 250
n = 60
defectuosos = 22
contenido_medio = 950
sd_muestra = 18
grados_libertad = n-1

# (a)
confianza = 0.95
alpha = 1-confianza

desvio_media = sd_muestra/sqrt(n)

valor_critico <- qt((1-alpha/2), df= grados_libertad)
valor_critico

factor_ajuste= sqrt((N-n)/(N-1))

limInf = contenido_medio - valor_critico*desvio_media*factor_ajuste
limSup = contenido_medio + valor_critico*desvio_media*factor_ajuste
IC <- c(limInf, limSup)
IC


