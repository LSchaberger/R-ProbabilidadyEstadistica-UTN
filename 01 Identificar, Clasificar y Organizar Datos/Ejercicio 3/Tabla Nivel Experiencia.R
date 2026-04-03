if(!require(readxl)){
  install.packages("readxl")
  librery(readxl)
}
# Cargamos el archivo xlsx.
datos <- read_excel("~/Documents/R - Ejercicios/Semana 1 - PyE/Práctica3 - Tabla Frecuencia Ordinal/Datos Práctica Tabla de Frecuencias en R (V Ordinal).xlsx")

# Ordenamos las categorias nominales para que la acumulada sea correcta.
datos$Nivel_Experiencia <- factor(
  datos$Nivel_Experiencia,
  levels = c("Junior", "SemiSenior", "Senior"),
  ordered = TRUE
)

# Creamos la tabla de frecuencia de Nivel_Experiencia.
fa_exp <- table(datos$Nivel_Experiencia)
fa_acum_exp <- cumsum(fa_exp)
fr_exp <- prop.table(fa_exp)
fr_acum_exp <- cumsum(fr_exp)

tabla_nivel_exp <- data.frame(
  Nivel_Experiencia = names(fa_exp),
  Frecuencia_Absoluta = as.vector(fa_exp),
  Frecuencia_Acumulada = as.vector(fa_acum_exp),
  Frecuencia_Relativa = round(as.vector(fr_exp), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_exp), 4)
)
# Mostramos la tabla y ocultamos los índices por defecto de RStudio.
print(tabla_nivel_exp, row.names = FALSE)


# Ahora la tabla de Tickets_Soporte.
fa_tickets <- table(datos$Tickets_Soporte)
fa_acum_tickets <- cumsum(fa_tickets)
fr_tickets <- prop.table(fa_tickets)
fr_acum_tickets <- cumsum(fr_tickets)

tabla_tickets <- data.frame(
  Tickets = as.numeric(names(fa_tickets)),
  Frecuencia_Absoluta = as.vector(fa_tickets),
  Frecuencia_Acumulada = as.vector(fa_acum_tickets),
  Frecuencia_Relativa = round(as.vector(fr_tickets), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_tickets), 4)
)
print(tabla_tickets, row.names = FALSE)

total_tickets_resueltos <- sum(datos$Tickets_Soporte)
total_tickets_resueltos


# Por último la tabla de Tiempo_Conexion. Será una tabla agrupada, regla de Sturges.
n <- length(datos$Tiempo_Conexion) # Devuelve la cantidad de objetos dentro de la columna.
k <- ceiling(1 + log2(n)) # Regla de Sturges.
k

# Ahora se crean los intervalos.
clases <- cut(datos$Tiempo_Conexion, breaks = k, right = FALSE)

# Frecuencias.
fa_tiempo <- table(clases)
fa_acum_tiempo <- cumsum(fa_tiempo)
fr_tiempo <- prop.table(fa_tiempo)
fr_acum_tiempo <- cumsum(fr_tiempo)

# Tabla final.
tabla_tiempo <- data.frame(
  Intervalo = names(fa_tiempo),
  Frecuencia_Absoluta = as.vector(fa_tiempo),
  Frecuencia_Acumulada = as.vector(fa_acum_tiempo),
  Frecuencia_Relativa = round(as.vector(fr_tiempo), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_tiempo), 4)
)
# Ocultamos los índices por defecto para que no haya confusión.
print(tabla_tiempo, row.names = FALSE)