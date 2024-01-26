######DIRECCIONAMIENTO ESTRATÉGICO######

#Cargamos la librería a utilizar.
library(syuzhet)

respuesta <- ("Ahora ahora tal vez voy bueno bueno bueno bueno bueno a regresar un poco a lo que proponía de conocimientos básicos en Administración Pública ¿Por qué? Porque la reestructura actual eh busca como tal, tener una reestructura que responda al nuevo modelo de atención. Nosotros como el órgano técnico de la Administración pública tendríamos, o se busca ser un referente para toda la institucionalidad pública. Siendo un instituto tendríamos que dar un ordenamiento, tendríamos que ser el referente para ministerios para secretarías y demás instituciones. Sin embargo, creo muy sinceramente que estamos con un déficit de de de la capacidad de respuesta de las personas que actualmente están, hay personas que tal vez cumplen un nivel académico pero... no sé, les falta ese ese, les falta, no sé si experiencia en el ámbito público, o esa capacidad de respuesta porque, ahora bien, derivado lo que mencionas como era como como referentes tendríamos que tener unas 

delegaciones nítidas y personas con capacidad de de administrar recursos, personas capaces de ejecutar metas físicas, pero actualmente no lo tenemos ni la sede central. Y ahora bien, lo que te decía de las de las de las matrices engorrosas, pero una persona actualmente desconoce manejar un catálogo insumos. Entonces desde ahí desde ahí el funcionamiento estratégico... ya vamos mal, una persona que no conozca un catálogo insumos, una persona que no conozca el clasificador presupuestario para sector público; si bien es cierto no son bien aciertos, está bien eso lo tengo claro, pero si estamos en la administración pública tenemos que conocer cómo vamos a gastar si al final para todo lo que vayamos a hacer necesitamos plata... podemos tener las ideas, podemos tener... no sé, tener tener procedimientos, podemos tener soluciones, lo que sea para para llevar a cabo buenas actuaciones, pero si no no tenemos plata eso se queda papel, eso no se va a llevar a cabo. Entonces el manejo de los recursos debe ser indispensable. Y tenemos deficiencias en ese sentido.  Un requisito fundamental para la contratación de una persona para optar a un cargo público ya sea empleado público funcionario público, es el finiquito de reclamación en existencias de cargos en el caso de la contraloría, esto toda la persona quiere optar a un cargo lo tiene que tener. Sin embargo en algún momento se pretendió de que toda persona que iba a optar a un cargo público tuviera la certificación en conocimientos básicos de administración pública. A mi parecer hubiese sido muy interesante que sí se hubiese llevado a cabo, si hubiese sido un requisito fundamental que toda persona que optara al cargo lo tuviese o que optara o que optara al cargo y que se le diera un plazo de seis meses para tener ese ese esa certificación y posteriormente ya contratar o pasar el período de prueba porque porque hoy en día muchas personas a veces llenan el perfil académico, a veces llenan el perfil de la experiencia en teoría, sin embargo no tienen en realidad el el conocimiento, el sentir del funcionario público del 

servidor público, muchas personas a veces vienen desde afuera que no conocen bueno bueno bueno la administración pública como tal. Se si hacen por bueno bueno bueno bueno ejemplo si hablamos del área financiera el ámbito financiero que eso es mi competencia, es muy distinta a la contabilidad que se llevaba en empresas privadas a la contabilidad gubernamental. El ámbito presupuestario, el ciclo presupuestario que nosotros llevamos como en como estado es bueno bueno bueno bueno muy distinto a un ciclo de pago en empresas privadas. Por ejemplo la facturación, la documentación de respaldo y esa es información que obviamente no lo pueden tener a cabalidad en una certificación, pero sin embargo ya van teniendo nociones. Por ejemplo, aquí en en el estado también es un poco burócrata ese esos procedimientos pero personas que vienen de afuera ocuparon algunos cargos muchas veces de directivos piensan que que lo pueden hacer así, como que traen experiencias del ámbito privado del sector privado y no es así, tenemos que llevar un debido proceso entonces a veces es si una persona que viene como directivo no conoce eso, quiere como pasarse por alto esa esos procedimientos que muchas veces pueden resultar en hallazgos para los técnicos, para bueno bueno bueno bueno bueno bueno bueno los operativos.")

# Convertirmos la respuesta en palabras
# Todas las palabras se pasan a minusculas

palabras <- get_tokens(respuesta)

# Cargamos el diccionario
# Es un dataframe con 3 columnas
diccionario <- read.csv("https://raw.githubusercontent.com/jorgeorenos/Rutina_encuestras/modificaciones/Diccionarios/diccionario_afinn_modificado.csv",
                        fileEncoding = "latin1")

# Ahora obtenemos las valoraciones
# Buscamos las coincidencias del diccionario en la respuesta
valor_pregunta <- diccionario[which(diccionario$Palabra %in% palabras), c("Palabra","Puntuacion", "positivas", "negativas")] 

# Resumimos la información de dataframe
# vemos que la frase tiene una valoración más positiva que negativa. 
# Esto lo notamos debido a que la media de positiva es 1 y la de negativa es 0.5
summary(valor_pregunta)

# Obteniendo la valoración general

valor_general <- sum(valor_pregunta$Puntuacion)
valor_general

# prueba parafos
parrafos <- get_tokens(respuesta, pattern = "\\n")

plot(valor_pregunta$Puntuacion, type = "l",
     xlab = "", ylab = "Valoración", main = "Evolución del texto")
points(valor_pregunta$Puntuacion)