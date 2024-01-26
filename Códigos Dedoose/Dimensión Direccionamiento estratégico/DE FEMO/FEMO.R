####LIMPIAMOS EL ÁREA DE TRABAJO
rm(list = ls())

####USAR COMO FUENTE LA funcion_disposicion.R
source("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/funcion_disposicion.R")



####CARGAMOS CADA UNO DE LOS CÓDIGOS PARA CADA INDICADOR DE LA DIMENSIÓN EN CUESTIÓN.
comunicacion <- scan("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/DE FEMO/Comunicacion.txt",
             what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T) #EL VECTOR estr GUARDA CARACTERES SEPARADOS POR SALTOS DE LÍNEA

innovacion <- scan("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/DE FEMO/innovacion.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T) #EL VECTOR innov GUARDA CARACTERES SEPARADOS POR SALTOS DE LÍNEA

vision <- scan("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/DE FEMO/vision.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T) #EL VECTOR clima GUARDA CARACTERES SEPARADOS POR SALTOS DE LÍNEA

compromiso <- scan("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/DE FEMO/compromiso.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T) #EL VECTOR mecan GUARDA CARACTERES SEPARADOS POR SALTOS DE LÍNEA

clima <- scan("C:/Users/tecnico_ine/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/DE FEMO/clima.txt",
                what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T) #EL VECTOR gestion GUARDA CARACTERES SEPARADOS POR SALTOS DE LÍNEA


####ANÁLISIS DE COMUNICACION

analisis_comunicacion <- c() #SE CREA EL VECTOR analisis_estr 
for (i in seq_along(comunicacion)) { #ESTE FOR RECORRE PALABRA POR PALABRA DEL INDICADOR estr
  valor <- disposicion(comunicacion[i], diccionario = "afinn") #LA LISTA VALOR, GUARDA LOS VALORES DE LAS PALABRAS DETECTADAS
  analisis_comunicacion[i] <- valor[[2]] #SE LE ASIGNAN LOS VALORES DETECTADOS AL VECTOR analisis_estr
}

####ANÁLISIS DE INNOVACIÓN
analisis_innovacion <- c() #SE CREA EL VECTOR analisis_innov
for (i in seq_along(innovacion)) { #ESTE FOR RECORRE PALABRA POR PALABRA DEL INDICADOR innov
  valor <- disposicion(innovacion[i], diccionario = "afinn") #LA LISTA VALOR, GUARDA LOS VALORES DE LAS PALABRAS DETECTADAS
  analisis_innovacion[i] <- valor[[2]] #SE LE ASIGNAN LOS VALORES DETECTADOS AL VECTOR analisis_innov
}

####ANÁLISIS DE VISION PROSPECTIVA
analisis_vision <- c() #SE CREA EL VECTOR analisis_clima
for (i in seq_along(vision)) { #ESTE FOR RECORRE PALABRA POR PALABRA DEL INDICADOR clima
  valor <- disposicion(vision[i], diccionario = "afinn") #LA LISTA VALOR, GUARDA LOS VALORES DE LAS PALABRAS DETECTADAS
  analisis_vision[i] <- valor[[2]] #SE LE ASIGNAN LOS VALORES DETECTADOS AL VECTOR analisis_clima
}

####ANÁLISIS DE COMPROMISO
analisis_compromiso <- c() #SE CREA EL VECTOR analisis_mecan
for (i in seq_along(compromiso)) { #ESTE FOR RECORRE PALABRA POR PALABRA DEL INDICADOR mecan
  valor <- disposicion(compromiso[i], diccionario = "afinn") #LA LISTA VALOR, GUARDA LOS VALORES DE LAS PALABRAS DETECTADAS
  analisis_compromiso[i] <- valor[[2]] #SE LE ASIGNAN LOS VALORES DETECTADOS AL VECCTOR analisis_mecan
}

####ANÁLISIS DE CLIMA ORGANIZACIONAL
analisis_clima <- c() #SE CREA EL VECTOR analisis_gestion
for (i in seq_along(clima)) { #ESTE FOR RECORRE PALABRA POR PALABRA DEL INDICADOR gestion
  valor <- disposicion(clima[i], diccionario = "afinn") #LA LISTA VALOR, GUARDA LOS VALORES DE LAS PALABRAS DETECTADAS
  analisis_clima[i] <- valor[[2]] #SE LE ASIGNAN LOS VALORES DETECTADOS AL VECTOR analisis_gestion
}


#EL VECTOR primeros GUARDA LAS SUMAS PARA CADA INDICADOR
primeros <- c(sum(analisis_comunicacion), sum(analisis_innovacion), sum(analisis_vision),sum(analisis_compromiso),sum(analisis_clima))

primeros
rm(valor)


c <- mean(primeros)
t1 <- ((2*c)/pi)*(atan(primeros/c))
t1
t2<- 0.5*((t1/c)+1)
t2


####TRANSFORMACION METODOLOGÍA 1

analisis_comunicacion_df <- list() #SE GUARDA UN DATAFRAME EN LA LISTA

for (i in seq_along(comunicacion)) {
  valor <- disposicion(comunicacion[i], diccionario = "afinn")
  analisis_comunicacion_df[[i]] <- valor[[1]]
}

analisis_comunicacion_df <- analisis_comunicacion_df[analisis_comunicacion!=0]


vp_1 <- 0
vn_1 <- 0
np_1 <- 0
nn_1 <- 0

for (i in seq_along(analisis_comunicacion_df)){
  vp_1 <- vp_1+sum(analisis_comunicacion_df[[i]]$positivas)
  vn_1 <- vn_1+sum(analisis_comunicacion_df[[i]]$negativas)
  np_1 <- np_1+sum(analisis_comunicacion_df[[i]]$negativas==FALSE)
  nn_1 <- nn_1+sum(analisis_comunicacion_df[[i]]$positivas==FALSE)
}

razon_comunicacion <- (vp_1-vn_1)/(np_1+nn_1)

analisis_innovacion_df <- list()
for (i in seq_along(innovacion)) {
  valor <- disposicion(innovacion[i], diccionario = "afinn")
  analisis_innovacion_df[[i]] <- valor[[1]]
}
analisis_innovacion_df <-  analisis_innovacion_df[analisis_innovacion!=0]
vp_2 <- 0
vn_2 <- 0
np_2 <- 0
nn_2 <- 0
for (i in seq_along(analisis_innovacion_df)){
  vp_2 <- vp_2+sum(analisis_innovacion_df[[i]]$positivas)
  vn_2 <- vn_2+sum(analisis_innovacion_df[[i]]$negativas)
  np_2 <- np_2+sum(analisis_innovacion_df[[i]]$negativas==FALSE)
  nn_2 <- nn_2+sum(analisis_innovacion_df[[i]]$positivas==FALSE)
}

razon_innovacion <- (vp_2-vn_2)/(np_2+nn_2)

analisis_clima_df <- list()
for (i in seq_along(clima)) {
  valor <- disposicion(clima[i], diccionario = "afinn")
  analisis_clima_df[[i]] <- valor[[1]]
}
analisis_clima_df <-  analisis_clima_df[analisis_clima!=0]
vp_3 <- 0
vn_3 <- 0
np_3 <- 0
nn_3 <- 0
for (i in seq_along(analisis_clima_df)){
  vp_3 <- vp_3+sum(analisis_clima_df[[i]]$positivas)
  vn_3 <- vn_3+sum(analisis_clima_df[[i]]$negativas)
  np_3 <- np_3+sum(analisis_clima_df[[i]]$negativas==FALSE)
  nn_3 <- nn_3+sum(analisis_clima_df[[i]]$positivas==FALSE)
}

razon_clima <- (vp_3-vn_3)/(np_3+nn_3)


analisis_vision_df <- list()
for (i in seq_along(vision)) {
  valor <- disposicion(vision[i], diccionario = "afinn")
  analisis_vision_df[[i]] <- valor[[1]]
}
analisis_vision_df <-  analisis_vision_df[analisis_vision!=0]
vp_4 <- 0
vn_4 <- 0
np_4 <- 0
nn_4 <- 0
for (i in seq_along(analisis_vision_df)){
  vp_4 <- vp_4+sum(analisis_vision_df[[i]]$positivas)
  vn_4 <- vn_4+sum(analisis_vision_df[[i]]$negativas)
  np_4 <- np_4+sum(analisis_vision_df[[i]]$negativas==FALSE)
  nn_4 <- nn_4+sum(analisis_vision_df[[i]]$positivas==FALSE)
}

razon_vision <- (vp_4-vn_4)/(np_4+nn_4)


analisis_compromiso_df <- list()
for (i in seq_along(compromiso)) {
  valor <- disposicion(compromiso[i], diccionario = "afinn")
  analisis_compromiso_df[[i]] <- valor[[1]]
}
analisis_compromiso_df <-  analisis_compromiso_df[analisis_compromiso!=0]
vp_5 <- 0
vn_5 <- 0
np_5 <- 0
nn_5 <- 0
for (i in seq_along(analisis_compromiso_df)){
  vp_5 <- vp_5+sum(analisis_compromiso_df[[i]]$positivas)
  vn_5 <- vn_5+sum(analisis_compromiso_df[[i]]$negativas)
  np_5 <- np_5+sum(analisis_compromiso_df[[i]]$negativas==FALSE)
  nn_5 <- nn_5+sum(analisis_compromiso_df[[i]]$positivas==FALSE)
}

razon_compromiso <- (vp_5-vn_5)/(np_5+nn_5)


razones <- c(razon_comunicacion, razon_innovacion, razon_vision, razon_compromiso, razon_clima)
razones
c <- 2
y1 <- (2*(c)/pi)*(atan(razones/(2.5)))
y1
y2 <- (1/(2*c))*(y1+c)
y2



####TRANSFORMACIONES METODOLOGIA 2

analisis_comunicacion_df <- list() #porque vamos a guardar dataframes

for (i in seq_along(comunicacion)) {
  valor <- disposicion(comunicacion[i], diccionario = "afinn")
  analisis_comunicacion_df[[i]] <- valor[[1]]
}

analisis_comunicacion_df <- analisis_comunicacion_df[analisis_comunicacion!=0]


vp_1 <- 0
vn_1 <- 0
np_1 <- 0
nn_1 <- 0

for (i in seq_along(analisis_comunicacion_df)){
  vp_1 <- vp_1+sum(analisis_comunicacion_df[[i]]$positivas)
  vn_1 <- vn_1+sum(analisis_comunicacion_df[[i]]$negativas)
  np_1 <- np_1+sum(analisis_comunicacion_df[[i]]$negativas==FALSE)
  nn_1 <- nn_1+sum(analisis_comunicacion_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_comunicacion <- (vp_1/np_1)-(vn_1/nn_1)

analisis_innovacion_df <- list()
for (i in seq_along(innovacion)) {
  valor <- disposicion(innovacion[i], diccionario = "afinn")
  analisis_innovacion_df[[i]] <- valor[[1]]
}
analisis_innovacion_df <-  analisis_innovacion_df[analisis_innovacion!=0]
vp_2 <- 0
vn_2 <- 0
np_2 <- 0
nn_2 <- 0
for (i in seq_along(analisis_innovacion_df)){
  vp_2 <- vp_2+sum(analisis_innovacion_df[[i]]$positivas)
  vn_2 <- vn_2+sum(analisis_innovacion_df[[i]]$negativas)
  np_2 <- np_2+sum(analisis_innovacion_df[[i]]$negativas==FALSE)
  nn_2 <- nn_2+sum(analisis_innovacion_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_innovacion <- (vp_2/np_2)-(vn_2/nn_2)

analisis_clima_df <- list()
for (i in seq_along(clima)) {
  valor <- disposicion(clima[i], diccionario = "afinn")
  analisis_clima_df[[i]] <- valor[[1]]
}
analisis_clima_df <-  analisis_clima_df[analisis_clima!=0]
vp_3 <- 0
vn_3 <- 0
np_3 <- 0
nn_3 <- 0
for (i in seq_along(analisis_clima_df)){
  vp_3 <- vp_3+sum(analisis_clima_df[[i]]$positivas)
  vn_3 <- vn_3+sum(analisis_clima_df[[i]]$negativas)
  np_3 <- np_3+sum(analisis_clima_df[[i]]$negativas==FALSE)
  nn_3 <- nn_3+sum(analisis_clima_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_clima <- (vp_3/np_3)-(vn_3/nn_3)


analisis_vision_df <- list()
for (i in seq_along(vision)) {
  valor <- disposicion(vision[i], diccionario = "afinn")
  analisis_vision_df[[i]] <- valor[[1]]
}
analisis_vision_df <-  analisis_vision_df[analisis_vision!=0]
vp_4 <- 0
vn_4 <- 0
np_4 <- 0
nn_4 <- 0
for (i in seq_along(analisis_vision_df)){
  vp_4 <- vp_4+sum(analisis_vision_df[[i]]$positivas)
  vn_4 <- vn_4+sum(analisis_vision_df[[i]]$negativas)
  np_4 <- np_4+sum(analisis_vision_df[[i]]$negativas==FALSE)
  nn_4 <- nn_4+sum(analisis_vision_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_vision <- (vp_4/np_4)-(vn_4/nn_4)


analisis_compromiso_df <- list()
for (i in seq_along(compromiso)) {
  valor <- disposicion(compromiso[i], diccionario = "afinn")
  analisis_compromiso_df[[i]] <- valor[[1]]
}
analisis_compromiso_df <-  analisis_compromiso_df[analisis_compromiso!=0]
vp_5 <- 0
vn_5 <- 0
np_5 <- 0
nn_5 <- 0
for (i in seq_along(analisis_compromiso_df)){
  vp_5 <- vp_5+sum(analisis_compromiso_df[[i]]$positivas)
  vn_5 <- vn_5+sum(analisis_compromiso_df[[i]]$negativas)
  np_5 <- np_5+sum(analisis_compromiso_df[[i]]$negativas==FALSE)
  nn_5 <- nn_5+sum(analisis_compromiso_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_compromiso <- (vp_5/np_5)-(vn_5/nn_5)


promedios_absolutos <- c(Promedio_absoluto_comunicacion, Promedio_absoluto_innovacion, Promedio_absoluto_vision, Promedio_absoluto_compromiso, Promedio_absoluto_clima)
promedios_absolutos
c <- 2
y1 <- (2*(c)/pi)*(atan(promedios_absolutos/(2.5)))
y1
y2 <- (1/(2*c))*(y1+c)
y2

