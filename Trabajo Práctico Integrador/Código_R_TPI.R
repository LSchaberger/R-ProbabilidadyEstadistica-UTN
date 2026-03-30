# Instalamos paquete si aún no está para leer archivos xlsx.
if (!require(readxl)){
  install.packages("readxl")
  library(readxl) # Abre la librería.
}

# Abrimos archivo xlsx.
datos <- read_excel(file.choose())

# Visualizamos las variables existentes.
colnames(datos)


# Tabla de frecuencia de TIEMPO_SEMANAL_ESTUDIO_HS. (Cuantitativa Contínua).

n <- length(datos$TIEMPO_SEMANAL_ESTUDIO_HS) # Tamaño de la muestra.
n # Visualizamos en consola.

k <- ceiling(1 + log2(n)) #Regla de Sturges
k

# Creamos los intervalos para cada clase. Con right(FALSE) invertimos a [...)
clases <- cut(datos$TIEMPO_SEMANAL_ESTUDIO_HS, breaks = k, right = FALSE)

fa_tiempo <- table(clases)
fa_acum_tiempo <- cumsum(fa_tiempo)
fr_tiempo <- prop.table(fa_tiempo)
fr_acum_tiempo <- cumsum(fr_tiempo)

# Construimos la tabla visualmente.
tabla_tiempo <- data.frame(
  Intervalo_Hs = names(fa_tiempo), # Intervalo en horas.
  Frecuencia_Absoluta = as.vector(fa_tiempo),
  Frecuencia_Acumulada = as.vector(fa_acum_tiempo),
  Frecuencia_Relativa = round(as.vector(fr_tiempo), 4), # Dejamos 4 decimales con round()
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_tiempo), 4)
)
# Mostramos tabla.
print(tabla_tiempo, row.names = FALSE) # row.names = Falso porque no queremos los identificadores (etiquetas) de las observaciones.


# SATISFACCIÓN CON CARRERA ESTUDIANTES.
# Ordenamos primero la satisfacción de los estudiantes en niveles. (Cualitativa Ordinal).
datos$SATISF_CON_CARRERA <- factor(
  datos$SATISF_CON_CARRERA, levels = c("1", "2", "3", "4"),
  labels = c("Muy Satisfecho", "Satisfecho", "Insatisfecho", "Muy Insatisfecho"),
  ordered = TRUE # En ese orden tal cual, siendo "1" Muy Satisfecho a "4" Muy Insatisfecho.
  )

# Tabla de Frecuancias SATISF_CON_CARRERA.
fa_satisf <- table(datos$SATISF_CON_CARRERA)
fa_acum_satisf <- cumsum(fa_satisf)
fr_satisf <- prop.table(fa_satisf)
fr_acum_satisf <- cumsum(fr_satisf)

# Construimos la tabla visualmente.
tabla_satisfaccion <- data.frame(
  Niveles = names(fa_satisf),
  Frecuencia_Absoluta = as.vector(fa_satisf),
  Frecuencia_Acumulada = as.vector(fa_acum_satisf),
  Frecuencia_Relativa = round(as.vector(fr_satisf), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_satisf), 4)
)

print(tabla_satisfaccion, row.names = FALSE)
