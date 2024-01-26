# Limpiamos el área de trabajo
rm(list = ls())
source("C:/Users/salay/Desktop/Rutina_encuestras/funcion_disposicion.R")

estr <- scan("C:/Users/salay/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/PRUEBA 4/ED.txt",
             what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T)

innov <- scan("C:/Users/salay/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/PRUEBA 4/BI.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T)

clima <- scan("C:/Users/salay/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/PRUEBA 4/CO.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T)

mecan <- scan("C:/Users/salay/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/PRUEBA 4/MC.txt",
              what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T)

gestion <- scan("C:/Users/salay/Desktop/Rutina_encuestras/Códigos Dedoose/Dimensión Direccionamiento estratégico/PRUEBA 4/GPR.txt",
                what = character(), sep = "\n", fileEncoding = "UTF-8", allowEscapes = T)


# Análisis estrategia de direccionamiento
analisis_estr <- c()
for (i in seq_along(estr)) {
  valor <- disposicion(estr[i], diccionario = "afinn")
  analisis_estr[i] <- valor[[2]]
}

# Análisis innovación y disposición al cambio
analisis_innov <- c()
for (i in seq_along(innov)) {
  valor <- disposicion(innov[i], diccionario = "afinn")
  analisis_innov[i] <- valor[[2]]
}

# Análisis clima organizacional
analisis_clima <- c()
for (i in seq_along(clima)) {
  valor <- disposicion(clima[i], diccionario = "afinn")
  analisis_clima[i] <- valor[[2]]
}

# Análisis mecanismos de comunicación
analisis_mecan <- c()
for (i in seq_along(mecan)) {
  valor <- disposicion(mecan[i], diccionario = "afinn")
  analisis_mecan[i] <- valor[[2]]
}

# Análisis enfoque en gestión por procesos
analisis_gestion <- c()
for (i in seq_along(gestion)) {
  valor <- disposicion(gestion[i], diccionario = "afinn")
  analisis_gestion[i] <- valor[[2]]
}

rm(valor)

primeros <- c(sum(analisis_estr), sum(analisis_innov), sum(analisis_clima),sum(analisis_mecan),sum(analisis_gestion))

primeros

####TRANSFORMACION METODOLOGÍA 1

analisis_estr_df <- list() #porque vamos a guardar dataframes

for (i in seq_along(estr)) {
  valor <- disposicion(estr[i], diccionario = "afinn")
  analisis_estr_df[[i]] <- valor[[1]]
}

analisis_estr_df <- analisis_estr_df[analisis_estr!=0]


vp_1 <- 0
vn_1 <- 0
np_1 <- 0
nn_1 <- 0

for (i in seq_along(analisis_estr_df)){
  vp_1 <- vp_1+sum(analisis_estr_df[[i]]$positivas)
  vn_1 <- vn_1+sum(analisis_estr_df[[i]]$negativas)
  np_1 <- np_1+sum(analisis_estr_df[[i]]$negativas==FALSE)
  nn_1 <- nn_1+sum(analisis_estr_df[[i]]$positivas==FALSE)
}

razon_ED <- (vp_1-vn_1)/(np_1+nn_1)

analisis_innov_df <- list()
for (i in seq_along(innov)) {
  valor <- disposicion(innov[i], diccionario = "afinn")
  analisis_innov_df[[i]] <- valor[[1]]
}
analisis_innov_df <-  analisis_innov_df[analisis_innov!=0]
vp_2 <- 0
vn_2 <- 0
np_2 <- 0
nn_2 <- 0
for (i in seq_along(analisis_innov_df)){
  vp_2 <- vp_2+sum(analisis_innov_df[[i]]$positivas)
  vn_2 <- vn_2+sum(analisis_innov_df[[i]]$negativas)
  np_2 <- np_2+sum(analisis_innov_df[[i]]$negativas==FALSE)
  nn_2 <- nn_2+sum(analisis_innov_df[[i]]$positivas==FALSE)
}

razon_BI <- (vp_2-vn_2)/(np_2+nn_2)

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

razon_CO <- (vp_3-vn_3)/(np_3+nn_3)


analisis_mecan_df <- list()
for (i in seq_along(mecan)) {
  valor <- disposicion(mecan[i], diccionario = "afinn")
  analisis_mecan_df[[i]] <- valor[[1]]
}
analisis_mecan_df <-  analisis_mecan_df[analisis_mecan!=0]
vp_4 <- 0
vn_4 <- 0
np_4 <- 0
nn_4 <- 0
for (i in seq_along(analisis_mecan_df)){
  vp_4 <- vp_4+sum(analisis_mecan_df[[i]]$positivas)
  vn_4 <- vn_4+sum(analisis_mecan_df[[i]]$negativas)
  np_4 <- np_4+sum(analisis_mecan_df[[i]]$negativas==FALSE)
  nn_4 <- nn_4+sum(analisis_mecan_df[[i]]$positivas==FALSE)
}

razon_MC <- (vp_4-vn_4)/(np_4+nn_4)


analisis_gestion_df <- list()
for (i in seq_along(gestion)) {
  valor <- disposicion(gestion[i], diccionario = "afinn")
  analisis_gestion_df[[i]] <- valor[[1]]
}
analisis_gestion_df <-  analisis_gestion_df[analisis_gestion!=0]
vp_5 <- 0
vn_5 <- 0
np_5 <- 0
nn_5 <- 0
for (i in seq_along(analisis_gestion_df)){
  vp_5 <- vp_5+sum(analisis_gestion_df[[i]]$positivas)
  vn_5 <- vn_5+sum(analisis_gestion_df[[i]]$negativas)
  np_5 <- np_5+sum(analisis_gestion_df[[i]]$negativas==FALSE)
  nn_5 <- nn_5+sum(analisis_gestion_df[[i]]$positivas==FALSE)
}

razon_GPR <- (vp_5-vn_5)/(np_5+nn_5)


razones <- c(razon_ED, razon_BI, razon_CO, razon_MC, razon_GPR)
razones
c <- 2
y1 <- (2*(c)/pi)*(atan(razones/(1.8)))
y1
y2 <- (1/(2*c))*(y1+c)
y2



####TRANSFORMACIONES METODOLOGIA 2

analisis_estr_df <- list() #porque vamos a guardar dataframes

for (i in seq_along(estr)) {
  valor <- disposicion(estr[i], diccionario = "afinn")
  analisis_estr_df[[i]] <- valor[[1]]
}

analisis_estr_df <- analisis_estr_df[analisis_estr!=0]


vp_1 <- 0
vn_1 <- 0
np_1 <- 0
nn_1 <- 0

for (i in seq_along(analisis_estr_df)){
  vp_1 <- vp_1+sum(analisis_estr_df[[i]]$positivas)
  vn_1 <- vn_1+sum(analisis_estr_df[[i]]$negativas)
  np_1 <- np_1+sum(analisis_estr_df[[i]]$negativas==FALSE)
  nn_1 <- nn_1+sum(analisis_estr_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_ED <- (vp_1/np_1)-(vn_1/nn_1)

analisis_innov_df <- list()
for (i in seq_along(innov)) {
  valor <- disposicion(innov[i], diccionario = "afinn")
  analisis_innov_df[[i]] <- valor[[1]]
}
analisis_innov_df <-  analisis_innov_df[analisis_innov!=0]
vp_2 <- 0
vn_2 <- 0
np_2 <- 0
nn_2 <- 0
for (i in seq_along(analisis_innov_df)){
  vp_2 <- vp_2+sum(analisis_innov_df[[i]]$positivas)
  vn_2 <- vn_2+sum(analisis_innov_df[[i]]$negativas)
  np_2 <- np_2+sum(analisis_innov_df[[i]]$negativas==FALSE)
  nn_2 <- nn_2+sum(analisis_innov_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_BI <- (vp_2/np_2)-(vn_2/nn_2)

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

Promedio_absoluto_CO <- (vp_3/np_3)-(vn_3/nn_3)


analisis_mecan_df <- list()
for (i in seq_along(mecan)) {
  valor <- disposicion(mecan[i], diccionario = "afinn")
  analisis_mecan_df[[i]] <- valor[[1]]
}
analisis_mecan_df <-  analisis_mecan_df[analisis_mecan!=0]
vp_4 <- 0
vn_4 <- 0
np_4 <- 0
nn_4 <- 0
for (i in seq_along(analisis_mecan_df)){
  vp_4 <- vp_4+sum(analisis_mecan_df[[i]]$positivas)
  vn_4 <- vn_4+sum(analisis_mecan_df[[i]]$negativas)
  np_4 <- np_4+sum(analisis_mecan_df[[i]]$negativas==FALSE)
  nn_4 <- nn_4+sum(analisis_mecan_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_MC <- (vp_4/np_4)-(vn_4/nn_4)


analisis_gestion_df <- list()
for (i in seq_along(gestion)) {
  valor <- disposicion(gestion[i], diccionario = "afinn")
  analisis_gestion_df[[i]] <- valor[[1]]
}
analisis_gestion_df <-  analisis_gestion_df[analisis_gestion!=0]
vp_5 <- 0
vn_5 <- 0
np_5 <- 0
nn_5 <- 0
for (i in seq_along(analisis_gestion_df)){
  vp_5 <- vp_5+sum(analisis_gestion_df[[i]]$positivas)
  vn_5 <- vn_5+sum(analisis_gestion_df[[i]]$negativas)
  np_5 <- np_5+sum(analisis_gestion_df[[i]]$negativas==FALSE)
  nn_5 <- nn_5+sum(analisis_gestion_df[[i]]$positivas==FALSE)
}

Promedio_absoluto_GPR <- (vp_5/np_5)-(vn_5/nn_5)


promedios_absolutos <- c(Promedio_absoluto_ED, Promedio_absoluto_BI, Promedio_absoluto_CO, Promedio_absoluto_MC, Promedio_absoluto_GPR)
promedios_absolutos

c <- 2
y1 <- (2*(c)/pi)*(atan(promedios_absolutos/(2.5)))
y1
y2 <- (1/(2*c))*(y1+c)
y2



