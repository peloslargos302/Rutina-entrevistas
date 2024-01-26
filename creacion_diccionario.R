# Descargamos la libreria tidyverse para trabajar los datos
library(tidyverse)

# Descargando el diccionario "afinn"
## Diccionario traducido del repositorio https://github.com/jboscomendoza/rpubs.git

download.file("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv",
              destfile = "Diccionarios/diccionario_afinn.csv")

# Cargando el diccionario para extrer positivas y negativas
afinn <- read.csv("Diccionarios/diccionario_afinn.csv", fileEncoding = "latin1")

# Mutate para generar nuevas columas
## Columnas con el valor absoluto de cada palabra.
afinn <- afinn %>% mutate("positivas" = ifelse(Puntuacion >= 0, Puntuacion, 0),
                                      "negativas" = ifelse(Puntuacion < 0, abs(Puntuacion), 0))

# Identificación de palabras positivas y negativas
positivas <- afinn %>% filter(Puntuacion >= 0) %>% select(Palabra, Puntuacion)
negativas <- afinn %>% filter(Puntuacion < 0) %>% select(Palabra, Puntuacion)

# comprobamos que la imputación fue correcta
sum(afinn$negativas) + sum(negativas$Puntuacion)
sum(afinn$positivas) - sum(positivas$Puntuacion)

write.csv(afinn, file = "Diccionarios/diccionario_afinn_modificado.csv",
          fileEncoding = "latin1")


# CREACION DICCIONARIO "NRC"

# Cargando el diccionario
nrc <- read.table("Diccionarios/Spanish-NRC-Emotion-Intensity-Lexicon-v1.txt", header = TRUE, sep = "\t")

# Pasamos las palabras en español a minúsculas
nrc$Spanish.Word <- tolower(nrc$Spanish.Word)


## Vamos a separar las intensidades de las emocines en positivas y negativas
## Segiremos el siguiente esquema
### anger = negativa, anticipation = positiva, disgust = negativa, fear = negativa, joy = positiva
### sadness = negativa, surprise = positiva, trust = positiva

nrc <- nrc %>% mutate(negativas = ifelse(Emotion == "anger", Emotion.Intensity.Score, 
                                  ifelse(Emotion == "disgust", Emotion.Intensity.Score,
                                  ifelse(Emotion == "fear", Emotion.Intensity.Score,
                                  ifelse(Emotion == "sadness", Emotion.Intensity.Score, 0)))))

nrc <- nrc %>% mutate(positivas = ifelse(Emotion == "anticipation", Emotion.Intensity.Score, 
                                  ifelse(Emotion == "joy", Emotion.Intensity.Score,
                                  ifelse(Emotion == "surprise", Emotion.Intensity.Score,
                                  ifelse(Emotion == "trust", Emotion.Intensity.Score, 0)))))

# confirmamos si clasificamos de forma correcta
# separamos intensidades positivas y negativas
positivas <- nrc[which(nrc$Emotion %in% c("anticipation", "joy", "surprise", "trust")),3]
negativas <- nrc[which(nrc$Emotion %in% c("anger", "disgust", "fear", "sadness")), 3]

nrc$negativas <- nrc$negativas*-1

nrc <- nrc %>% mutate(intensidad = positivas + negativas)

sum(nrc$positivas) - sum(positivas)
sum(nrc$negativas) + sum(negativas)

# Debido a que dan cero las restas, la selección se hizo de forma correcta

# Escribimos el nuevo diccionario nrc
write.csv(nrc, file = "Diccionarios/diccionario_nrc.csv",
          fileEncoding = "latin1")
