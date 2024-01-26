######DIRECCIONAMIENTO ESTRATÉGICO######

#Cargamos la librería a utilizar.
library(syuzhet)


#Aquí podemos introducir las respuestas de la entrevista realizada.
respuesta <- ("Aquí está mi cambio.")


#Se extraen las palabras de la respuesta cargada.
texto_palabras <- get_tokens(respuesta)
#Nos muestra las primeras palabras de la respuesta cargada.
head(texto_palabras)
#Nos devuelve el número de palabras en la respuesta cargada.
length(texto_palabras)


#Se extraen las oraciones de la respuesta cargada, 
oraciones_vector <- get_sentences(respuesta)
#Nos muestra el número de oraciones en la respuesta cargada.
length(oraciones_vector)

#Mostrar las palabras de la respuesta gargada.
texto_palabras
#Mostrar las oraciones de la respuesta cargada.
oraciones_vector


#Se genera el diccionario, éste consta de dos columnas; una para colocar la palabra, y otra para colocar el valor que se le asigna a la palabra en cuestión.
DICCIONARIO <- data.frame(
  "word" = c("prioridad","bueno","malo", "mala gestión", "poco", "engorroso", "cumplimiento", "no", "sí", "cumple", "ayudar", "ayudamos", "juntos"),
  "value" = c(1, 3,-2,-5,-1,-2,1,-1,1,1,4,4,3))


#Con esta instrucción se convierte el diccionario en un archivo .csv
write.csv(x=DICCIONARIO, file = "DICCIONARIO.csv", row.names = FALSE)


#Se le asigna la variable DICCIONARIO_DE al archivo .csv
DICCIONARIO_DE <- read.csv("DICCIONARIO.csv")
#Se le asigna a la variable method, el método customizado. Esto, para poder utilizar el diccionario personalizado.
method <- "custom"
#Se le asigna a la variable DE_oraciones, la función get_sentiment. Aquí es donde se pondera la oración, según el diccionario personalizado.
DE_oraciones <- get_sentiment(oraciones_vector, method = method, lexicon = DICCIONARIO_DE)

#Muestra una gráfica de las ponderaciones de la respuesta cargada.
Dplot(DE_oraciones,
     type="l",
     main = "Estrategia de Direccionamiento",
     sub = "Análisis de gestión en respuesta",
     xlab = "Oraciones", ylab = "DIRECCIONAMIENTO ESTRATÉGICO "
     )


####Prueba####
#En esta prueba, analizamos palabra por palabra de la respuesta cargada, y se ponderan según el diccionario personalizado.
oracion <- oraciones_vector[1]

frases_oracion <- get_tokens(oracion)

valores = c()

for(i in  c(1:length(frases_oracion))){
  
  if (frases_oracion[i] %in% DICCIONARIO$word){
    valores[i] <- DICCIONARIO$value[which(DICCIONARIO$word == frases_oracion[i])]
  
    } else {
    valores[i] <- 0
  }
    
}


# Prueba 2
## Aquí obtenemos el 2 que ofrece get_sentiment
sum(DICCIONARIO[which(DICCIONARIO$word %in% frases_oracion), "value"])