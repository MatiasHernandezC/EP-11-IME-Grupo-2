# Grupo número 2
# Integrante Benjamín González
# Integrante Matías Hernández
# Integrante Ariel Núñez
# Integrante Carla Polanco


datos <- read.csv2(file.choose(), stringsAsFactors = TRUE)


# PREGUNTAS (TODOS LOS GRUPOS) 
# 1. Propongan una pregunta de investigación original, que involucre la comparación de las medias de dos 
# grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla propia, seleccionen una 
# muestra aleatoria de hogares (250 < n < 500) y respondan la pregunta propuesta utilizando una simulación 
# Monte Carlo. 








# 2. Propongan una pregunta de investigación original, que involucre la comparación de las medias de más 
# de dos grupos independientes (más abajo se dan unos ejemplos). Fijando una semilla distinta a la 
# anterior, seleccionen una muestra aleatoria de hogares (400 < n < 600) y respondan la pregunta 
# propuesta utilizando bootstrapping. Solo por ejercicio académico, aplique un análisis post-hoc con 
# bootstrapping aunque este no sea necesario.  Algunos ejemplos (que no pueden ser ocupados en este 
# ejercicio) son: ▪ En promedio, el ingreso per cápita (ytotcorh / numper) en la Región Metropolitana 
# (region) es el mismo entre hombres y mujeres (sexo) no heterosexuales (r23). 
#   ▪ El ingreso per cápita promedio es similar en las cuatro macro zonas (norte grande, norte chico, 
#   central, sur y austral). 
#   ▪ El arriendo promedio que se paga por viviendas similares a la habitada (v19) tiene relación con 
#   el nivel educacional (educ) del jefe o la jefa del hogar. 