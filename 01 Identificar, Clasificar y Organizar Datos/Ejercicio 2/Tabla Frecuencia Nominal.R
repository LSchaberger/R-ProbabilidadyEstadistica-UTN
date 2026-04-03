# A continuación, encontrarás un archivo que simula los resultados de una encuesta aplicada a 103 programadores y programadoras que trabajan de forma remota en distintas empresas de desarrollo de software. El objetivo del relevamiento fue conocer sus hábitos laborales y herramientas más utilizadas. La plataforma de comunicación más utilizada. La cantidad de tickets de soporte que resolvieron en una semana. El tiempo promedio de conexión diaria a los sistemas de trabajo. Utilizá este archivo para construir tablas de frecuencia para cada una de las variables. Importante: la variable Plataforma_Trabajo es una variable categórica nominal, por lo que: Debés construir únicamente las columnas de frecuencia absoluta y frecuencia relativa. No corresponde calcular la frecuencia acumulada, ya que no existe un orden entre las categorías. En cambio, para las variables numéricas Tickets_Soporte y Tiempo_Conexion sí corresponde construir las tres columnas: frecuencia absoluta, frecuencia relativa y frecuencia acumulada. En el caso de Tiempo_Conexion, deberás agrupar los datos en clases utilizando la función cut().

if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

datos <- read_excel("~/Documents/R - Ejercicios/Semana 1 - PyE/Práctica2 - Tabla Frecuencia Nominal/Datos Práctica Tabla de Frecuencias en R (V Nominal).xlsx")

# Plataforma_Trabajo.
# Frecuencia absoluta.
fa_plataforma <- table(datos$Plataforma_Trabajo)

# Frecuencia relativa.
f_rel <- prop.table(fa_plataforma)

# Tabla final.
tabla_final <- data.frame(
  Plataformas = names(fa_plataforma),
  Frecuencia_Absoluta = as.vector(fa_plataforma),
  Frecuencia_Relativa = round(as.vector(f_rel), 4)
)

# Mostrar tabla.
tabla_final



# Ahora Tickets_Soporte.
# Frecuencia absoluta
fa_tickets <- table(datos$Tickets_Soporte) # Cantidad de personas que resolvieron X tickets.

# Frecuencia absoluta acumulada.
fa_acum_tickets <- cumsum(fa_tickets)

# Frecuencia relativa.
fr_tickets <- prop.table(fa_tickets)

# Frecuencia relativa acumulada.
fr_acum_tickets <- cumsum(fr_tickets)

# Tabla final Tickets.
tabla_final_tickets <- data.frame(
  Tickets = as.numeric(names(fa_tickets)),
  Frecuencia_Absoluta = as.vector(fa_tickets), # Cantidad de personas.
  Frecuencia_Acumulada = as.vector(fa_acum_tickets),
  Frecuencia_Relativa = round(as.vector(fr_tickets), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_tickets), 4)
)

# Mostrar tabla, desactivamos los índices por defecto de la izquierda de RStudio.
print(tabla_final_tickets, row.names = FALSE)

#Media Tickets.
mean(datos$Tickets_Soporte)
#Mediana.
median(datos$Tickets_Soporte)
#Moda.
tabla_tickets_moda <- table(datos$Tickets_Soporte)
as.numeric(names(tabla_tickets_moda)[tabla_tickets_moda == max(tabla_tickets_moda)])
#Cuartiles.
quantile(datos$Tickets_Soporte)
#Desvío estándar.
sd(datos$Tickets_Soporte)
#Coeficiente de variación.
cv_tickets <- sd(datos$Tickets_Soporte) / mean(datos$Tickets_Soporte) * 100
cv_tickets



# Ahora Tiempo_Conexion.
n <- length(datos$Tiempo_Conexion) # Devuelve cuantas variables tiene la columna (103).

k <- ceiling(1 + log2(n)) # Regla de Sturges.
k

# Creamos las clases (intervalos) con cut().
# FALSE para que invierta el sentido (..] y sea de esta forma: [..)
clases <- cut(datos$Tiempo_Conexion, breaks = k, right = FALSE)

# Frecuencia absoluta.
fa_tiempo <- table(clases)

# Frecuencia absoluta acumulada.
fa_acum_tiempo <- cumsum(fa_tiempo)

# Frecuencia relativa.
fr_tiempo <- prop.table(fa_tiempo)

# Frecuencia relativa acumulada.
fr_acum_tiempo <- cumsum(fr_tiempo)

tabla_tiempo <- data.frame(
  Intervalo = names(fa_tiempo),
  Frecuencia_Absoluta = as.vector(fa_tiempo), # Cantidad personas que se conectan en ese rango.
  Frecuencia_Acumulada = as.vector(fa_acum_tiempo),
  Frecuencia_Relativa = round(as.vector(fr_tiempo), 4),
  Frecuencia_Rel_Acumulada = round(as.vector(fr_acum_tiempo), 4)
)

# Mostramos la tabla.
print(tabla_tiempo, row.names = FALSE)


#Media Tiempo.
mean(datos$Tiempo_Conexion)
#Mediana.
median(datos$Tiempo_Conexion)
#Clase modal, porque es contínua.
clase_modal <- names(fa_tiempo)[which.max(fa_tiempo)]
clase_modal
#tabla_tiempo_moda <- table(datos$Tiempo_Conexion)
#as.numeric(names(tabla_tiempo_moda)[tabla_tiempo_moda == max(tabla_tiempo_moda)])

#Cuartiles.
quantile(datos$Tiempo_Conexion)
#Desvío estándar.
sd(datos$Tiempo_Conexion)
#Coeficiente de variación.
cv_tiempo <- sd(datos$Tiempo_Conexion) / mean(datos$Tiempo_Conexion) * 100
cv_tiempo
