# -----------------------
# GUÍA SEMANA 2 - TUPaD
# -----------------------
# Este Script te guía en el uso de R para construir tablas de frecuencia
# con datos categóricos, discretos y contínuos.
# -----------------------

# 1. Cargamos la librería necesaria para leer archivos Excel.
if (!require(readxl)) install.packages("readxl")
library(readxl)

# 2. Carga de archivo Excel desde tu equipo.
# Aparecerá una ventana: buscá el archivo "Datos estudiantes de programación.xlsx"
archivo <- file.choose()
datos <- read_excel(archivo)

# Tabla de frecuencias para variable categórica.
# Contamos cuantas veces aparece cada lenguaje favorito.
tabla_lenguajes <- table(datos$Lenguaje_Favorito)

# Mostramos la tabla de frecuencias.
tabla_lenguajes

#Tabla de frecuencias para variable discreta: Proyectos_Completados
tabla <- table(datos$Proyectos_Completados)
f_acum <- cumsum(tabla)
f_rel <- prop.table(tabla)
f_rel_acum <- cumsum(f_rel)

tabla_frecuencia <- data.frame(
  Proyectos = names(tabla),
  Frec = as.vector(tabla),
  Frec_Acum = as.vector(f_acum),
  Frec_Rel = round(as.vector(f_rel), 3),
  Frec_Rec_Acum = round(as.vector(f_rel_acum), 3)
)

tabla_frecuencia

# Mostrar la tabla sin los índices de fila.
print(tabla_frecuencia, row.names = FALSE)


# ACTIVIDAD 2

# Fijamos la semilla para obtener siempre los mismos resultados.
set.seed(123)

# Generamos los datos: 47 tiempos simulados en minútos.
tiempos <- round(rnorm(47, mean = 55, sd = 15), 1)

# Reemplazamos valores negativos por 0.
tiempos <- ifelse(tiempos < 0, 0, tiempos)

# Mostramos los datos simulados.
tiempos

# Cantidad total de observaciones.
n <- length(tiempos)

# Número de clases según la regla de Sturges.
k <- ceiling(1 + 3.322 * log10(n))
k

rango <- range(tiempos)
amplitud <- ceiling((rango[2] - rango[1]) / k)
rango
amplitud

breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)
clases <- cut(tiempos, breaks = breaks, right = FALSE)
head(clases)

tabla_tiempos <- table(clases)
f_acum <- cumsum(tabla_tiempos)
f_rel <- prop.table(tabla_tiempos)
f_rel_acum <- cumsum(f_rel)

tabla_final <- data.frame(
  Intervalo = levels(clases),             # Los nombres de los intervalos.
  Frecuencia = as.vector(tabla_tiempos),  # Los datos sin nombres internos.
  Frec_Acumulada = as.vector(f_acum),
  Frec_Relativa = round(as.vector(f_rel), 3),
  Fec_Rel_Acum = round(as.vector(f_rel_acum), 3)
)

tabla_final
