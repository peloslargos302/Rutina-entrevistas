# Analisis de disposición de Fortalecimiento Estratégico
# El texto se obtuvo de la codificación hecha en dedoose

# Importamos la función para el análisis de la disposición
source("funcion_disposicion.R")

# Lectura códigos dedoose
## Lectura del código 

## Los archivos se encuentran en el repositorio de Github
fortalezas <- scan(file = "https://raw.githubusercontent.com/jorgeorenos/Rutina_encuestras/modificaciones/C%C3%B3digos%20Dedoose/1111Fortalezas%20uso%20de%20herramientas%20de%20gesti%C3%B3n.txt",
     what = character(), fileEncoding = "UTF-8", sep = "\n", allowEscapes = T)

# Obtenemos los índices donde se encuentran las respuestas
# Esto debido a que cada texto es una línea diferente en el archivo de texto
primeras <- seq(from = 8, to = 48, by = 8)
segundas <- seq(from = 57, to = 129, by = 8)

# Concatenamos los índices para obtener las respuestas en un solo vector
indice_respuestas <- c(primeras, segundas)

# Obtenemos las respuestas
respuestas  <- fortalezas[indice_respuestas]

# Ralizamo el análisis de disposición
## Creamos una lista donde se almacenan los analisis de cada respuesta

analisis <- list()

# Ciclo para analizar cada respuesta

for (i in c(1:length(respuestas))) {
  
  analisis[[i]] <- disposicion(respuestas[i])
  
}
rm(i)

# Guardando resultados 
valores <- c()

for (i in c(1:length(respuestas))) {
  
  print(paste("Valor de la respuesta", i, analisis[[i]][[2]]))
  valores[i] <- analisis[[i]][[2]]
}