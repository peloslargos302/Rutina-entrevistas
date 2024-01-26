source("funcion_disposicion.R")

# Ejemplo aplicado a un libro

texto <- scan(file = "https://raw.githubusercontent.com/programminghistorian/jekyll/gh-pages/assets/galdos_miau.txt", 
              fileEncoding = "UTF-8", what = character(), sep = "\n", allowEscapes = T)

analisis <- disposicion(texto)

summary(analisis[[1]])
analisis[[2]]

ggplot(data = analisis[[1]], aes(x = c(1:length(analisis[[1]]$intensidad)), y = analisis[[1]]$intensidad)) +
  geom_smooth(se = FALSE) +
  geom_hline(yintercept = 0)+
  labs(title = "Evoluición de la disposición",
       x = "",
       y = "Ponderación") +
  theme_classic(base_size = 20)


# Ejemplo aplicado a las entrevistas
respuestas <- read.csv("Entrevistas/Estructura ideal para la lectura de entrevistas-CSV.csv", sep = ";", fileEncoding = "UTF-8")

# Filtrando la característica "estrategia de direccionamiento"
estrategia <- respuestas %>% filter(Característica == "ESTRATEGIA DE DIRECCIONAMIENTO")

analisis_disp <- list()

for (i in seq_along(estrategia$Respuesta)) {
  analisis_disp[[i]] <- disposicion(estrategia$Respuesta[i])    
}

valor <- c()
for (i in seq_along(analisis_disp)) {
  print(paste("valor respuesta", i, analisis_disp[[i]][[2]]))  
  valor[i] <- analisis_disp[[i]][[2]]
}

x <- data.frame(x = 1:20, valor)

ggplot(data = x, aes(x = x, y = valor)) +
  geom_line(show.legend = TRUE) +
  geom_smooth(se = FALSE, show.legend = TRUE) +
  geom_hline(yintercept = 0) +
  labs(title = "Evolución de la disposición",
       x = "",
       y = "Ponderación") +
  theme_classic(base_size = 20)
