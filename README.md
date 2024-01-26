# Rutina encuestas
Trabajaremos el análisis de sentimientos aplicado a la meidición de la gestión institucional.
Es muy importante tener en cuenta en dónde están las carpetas para poder correr el archivo de R.


La ubicación de la rutina que elabora el conteo de las palabras es la siguiente:

Codigo Dedoose > Dimensión Direccionamiento estratégico > DE FEMO > FEMO.R

En el código del archivo FEMO.R, hay que hacer algunas modificaciones:

    1. En la línea 5, hay que cambiar la ruta del source, y adecuarla para que ubique el archivo funcion_disposicion.R.
    2. De la línea 10 a la 22, hay que cambiar el scan por la ruta en donde se encuentren los archivos que leerá la función disposición.