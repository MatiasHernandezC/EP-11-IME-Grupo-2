# Grupo número 2
# Integrante Benjamín González
# Integrante Matías Hernández
# Integrante Ariel Núñez
# Integrante Carla Polanco

library(ggpubr)
library(tidyr)
library(dplyr)

# Se leen los datos a trabajar
datos <- read.csv2(file.choose(), stringsAsFactors = TRUE)

# --------------Definición de funciones importantes--------------

# Función para obtener una permutación.
# Argumentos:
# - i: iterador (para llamadas posteriores).
# - muestra_1, muestra_2: muestras.
# Valor:
# - lista con las muestras resultantes tras la permutación.

obtiene_permutacion <- function(i, muestra_1, muestra_2) {
  n_1 <- length(muestra_1)
  combinada <- c(muestra_1, muestra_2)
  n <- length(combinada)
  permutacion <- sample(combinada, n, replace = FALSE)
  nueva_1 <- permutacion[1:n_1]
  nueva_2 <- permutacion[(n_1+1):n]
  return(list(nueva_1, nueva_2))
}

# Función para calcular la diferencia de un estadístico de interés entre las
# dos muestras.
# Argumentos:
# - muestras: lista con las muestras.
# - FUN: nombre de la función que calcula el estadístico de interés.
# Valor:
# - diferencia de un estadístico para dos muestras.

calcular_diferencia <- function(muestras, FUN) {
  muestra_1 <- muestras[[1]]
  muestra_2 <- muestras[[2]]
  diferencia <- FUN(muestra_1) - FUN(muestra_2)
  return(diferencia)
}

# Función para calcular el valor p.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - valor_observado: valor del estadístico de interés para las muestras
#   originales.
# - repeticiones: cantidad de permutaciones a realizar.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# Valor:
# - el valorp calculado.

calcular_valor_p <- function(distribucion, valor_observado,
                             repeticiones, alternative) {
  if(alternative == "two.sided") {
    numerador <- sum(abs(distribucion) > abs(valor_observado)) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else if(alternative == "greater") {
    numerador <- sum(distribucion > valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  else {
    numerador <- sum(distribucion < valor_observado) + 1
    denominador <- repeticiones + 1
    valor_p <- numerador / denominador
  }
  
  return(valor_p)
}

# Función para graficar una distribución.
# Argumentos:
# - distribucion: distribución nula del estadístico de interés.
# - ...: otros argumentos a ser entregados a gghistogram y ggqqplot.

graficar_distribucion <- function(distribucion, ...) {
  observaciones <- data.frame(distribucion)
  
  histograma <- gghistogram(observaciones, x = "distribucion",
                            xlab = "Estadístico de interés",
                            ylab = "Frecuencia", bins = 30, ...)
  
  qq <- ggqqplot(observaciones, x = "distribucion", ...)
  
  # Crear una única figura con todos los gráficos de dispersión.
  figura  <- ggarrange(histograma, qq, ncol = 2, nrow = 1)
  print(figura)
}

# Función para hacer la prueba de permutaciones.
# Argumentos:
# - muestra_1, muestra_2: vectores numéricos con las muestras a comparar.
# - repeticiones: cantidad de permutaciones a realizar.
# - FUN: función del estadístico E para el que se calcula la diferencia.
# - alternative: tipo de hipótesis alternativa. "two.sided" para
#   hipótesis bilateral, "greater" o "less" para hipótesis unilaterales.
# - plot: si es TRUE, construye el gráfico de la distribución generada.
# - ...: otros argumentos a ser entregados a graficar_distribucion.

contrastar_hipotesis_permutaciones <- function(muestra_1, muestra_2,
                                               repeticiones, FUN,
                                               alternative, plot, ...) {
  cat("Prueba de permutaciones\n\n")
  cat("Hipótesis alternativa:", alternative, "\n")
  observado <- calcular_diferencia(list(muestra_1, muestra_2), FUN)
  cat("Valor observado:", observado, "\n")
  
  n_1 <- length(muestra_1)
  
  # Generar permutaciones.
  permutaciones <- lapply(1:repeticiones, obtiene_permutacion, muestra_1,
                          muestra_2)
  
  # Generar la distribución.
  distribucion <- sapply(permutaciones, calcular_diferencia, FUN)
  
  # Graficar la distribución.
  if(plot) {
    graficar_distribucion(distribucion, ...)
  }
  
  # Calcular el valor p.
  valor_p <- calcular_valor_p(distribucion, observado, repeticiones,
                              alternative)
  
  cat("Valor p:", valor_p, "\n\n")
}






# --------------PREGUNTAS (TODOS LOS GRUPOS) --------------

# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos 
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una 
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación 
# Monte Carlo. 

# Pregunta de investigación original propuesta:
# Un grupo de estudiantes de informática de la universidad de Santiago de Chile quiere saber si 
# la edad media en la que la gente logra pagar la totalidad de la casa propia es la misma para 
# hombres y mujeres

# Estadístico de interés: la media de la edad de las personas que logra pagar la totalidad de la 
# casa propia

# Hipótesis:
# H0: La edad media de las personas que logra pagar la totalidad de la casa propia es igual 
# para hombres y mujeres.

# HA: La edad media de las personas que logra pagar la totalidad de la casa propia es distinta 
# para hombres y mujeres.

# Denotando como uA al promedio de las edades hombres, y uB al promedio de las edades de las 
# mujeres, entonces matemáticamente las hipótesis quedan expresadas como:
# H0: uA - uB = 0 
# HA: uA - uB != 0 

# Tamaño muestra = 400
# Semilla = 1312
# alfa = 0.05
# Permutaciones = 1750
R = 1750
set.seed(1312)
alfa <- 0.05

# Se filtran datos de hombres y mujeres.
hombre <- datos %>% filter(sexo == "Hombre")
mujer <- datos %>% filter(sexo == "Mujer")

casaHombre <- hombre %>% filter(v9 == "Propio pagado")

casaMujer <- mujer %>% filter(v9 == "Propio pagado")

# Se tabula que se vea la edad de las personas que se necesita.
edadCasaHombre <- casaHombre[["edad"]]
edadCasaMujer <- casaMujer[["edad"]]

# Se crean las tablas de las muestras para trabajar.
muestraEdadCasaHombre <- sample(edadCasaHombre, 400)
muestraEdadCasaMujer <- sample(edadCasaMujer, 400)


contrastar_hipotesis_permutaciones(muestraEdadCasaHombre, 
                                   muestraEdadCasaMujer, 
                                   repeticiones = R,
                                   FUN = mean,
                                   alternative = "two.sided",
                                   plot = TRUE, 
                                   color = "red",
                                   fill = "red")

#-------------- CONCLUSIONES PREGUNTA 1 --------------

# Dado que se obtiene un valor p = 0.01142204 < alfa = 0.05, se rechaza H0 en favor a HA, por lo
# tanto se concluye con un 95% de confianza que la edad media de las personas que logra pagar la 
# totalidad de la casa propia es distinta para hombres y mujeres.

# También se puede apreciar en los gráficos, formados a partir de permutaciones, que los datos siguen 
# una distribución normal.



# ------------------------------ PREGUNTA 2 ----------------------------------

# 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más 
# de dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la 
# anterior, seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta 
# propuesta utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con 
# bootstrapping aunque este no sea necesario.  Algunos ejemplos (que no pueden ser ocupados en este 
# ejercicio) son: 
#
#   ▪ En promedio, el ingreso per cápita (ytotcorh / numper) en la Región Metropolitana 
# (region) es el mismo entre hombres y mujeres (sexo) no heterosexuales (r23). 

#   ▪ El ingreso per cápita promedio es similar en las cuatro macro zonas (norte grande, norte chico, 
#   central, sur y austral). 

#   ▪ El arriendo promedio que se paga por viviendas similares a la habitada (v19) tiene relación con 
#   el nivel educacional (educ) del jefe o la jefa del hogar. 

# ------------------------- Enunciado ------------------------------

# Bob el constructor esta haciendo un estudio y desea saber si el promedio del tamaño de las casas de la
#  region de Tarapaca es igual para personas con techo de planchas metalicas, losa hormigon y tejas.
#
# Para resolver este ejercicio utilice la tecnica de Bootstrapping, para mas de dos muuestras independientes,
# tambien considere un alfa = 0.05 y p = 1000

# ------------------------------------HIPOTESIS----------------------------------

# H0: El tamaño promedio de las casas de la region de tarapaca es igual para  
#     las personas que tienen techo de planchas metalicas, losa hormigon y tejas. 

# H1: El tamaño promedio de las casas de la region de tarapaca es diferente para  
#     las personas que tienen techo de planchas metalicas, losa hormigon y tejas.









