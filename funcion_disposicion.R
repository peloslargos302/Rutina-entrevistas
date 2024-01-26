# Construcción de la función para el análisis de sentimientos.

disposicion <- function(texto, diccionario = "nrc"){
  #Cargando la librería tidyverse.
  library(tidyverse)
  library()
  #Extrae cada palabra de los textos.
  texto <- tolower(texto)
  palabras <- unlist(strsplit(texto, "\\W"))
  palabras <- palabras[which(palabras != "")]
  
  #Dependiendo de la elección del diccionario, carga el diccionario y asigna una lista a la ponderación y el valor
  if (diccionario == "nrc") {
  nrc <- read.csv("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/diccionario_nrc.csv",
                  fileEncoding = "latin1")
  
  Ponderacion <- nrc[which(nrc$Spanish.Word %in% palabras), c("Spanish.Word", "intensidad","positivas", 
                                                              "negativas", "Emotion.Intensity.Score")]
  names(Ponderacion)[1] <- "Palabra"
  
  valor <- sum(Ponderacion$positivas) + sum(Ponderacion$negativas)  
  } 
  
  else if(diccionario == "afinn"){
    
    afinn <- read.csv("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/diccionario_afinn_modificado.csv",
                      fileEncoding = "latin1")
    
    Ponderacion <- afinn[which(afinn$Palabra %in% palabras), c("Palabra", "Puntuacion", "positivas", "negativas")]
    valor <- sum(Ponderacion$Puntuacion)
    
  }
  
  return(list(Ponderacion, valor))
}