#cargando la base de datos
library(readxl)
library(dplyr)
library(lubridate)
datos <- read_excel("BD 6 MARZO 2023.xlsx")

#completando fecha de inicio
for (i in 1:nrow(datos)){
  if(is.na(datos$inicio[i])){
    datos$inicio[i] <- datos$inicio[i-1]
  }
}

#eliminando aquellos que no tengan nivel 1
nombres <- c()
for (i in 1:nrow(datos)){
  set <- datos %>% filter(nombre == datos$nombre[i])
  if (!("TMO NIVEL 1" %in% set$nivel)){
    nombres <- append(nombres, datos$nombre[i])
  }
}

nombres <- unique(nombres)
datos <- datos %>% filter(!(nombre %in% nombres))

#arreglando fecha fin
datos$fin <- substr(datos$fin,1,10)
datos$fin <- as.POSIXlt.character(datos$fin, tz = "UTC", format = "%d/%m/%Y")

#reestructurando la base
datosnew <- data.frame("nombre" = NA, "documento" = NA, "TMO NIVEL 1" = NA,
                       "TMO NIVEL 2" = NA, "TMO NIVEL 3" = NA, "TMO NIVEL 4" = NA,
                       "TMO NIVEL 5" = NA, "TMO NIVEL 6" = NA, "inicio" = NA, "fin" = NA)

pacientes <- unique(datos$nombre)

for (paciente in pacientes){
  registros <- datos %>% filter(nombre == paciente) %>% arrange(nivel) %>%
    mutate(inicio = as.character(inicio), fin = as.character(fin))
  n <- nrow(registros)
  nombre <- paciente
  documento <- registros$doc[1]
  fechainicio <- registros$inicio[1]
  fechafin <- registros$fin[n]
  datospaciente <- c(nombre,documento,as.vector(registros$estado),
                     rep(NA,6-n),fechainicio,fechafin)
  datosnew <- rbind(datosnew,datospaciente)
}


#creando evento/censura
##definiendo una tolerancia
nombrestratcomp <- datos$nombre[datos$nivel=="TMO NIVEL 6"]
tiemposespera <- numeric(0)
for (nomb in nombrestratcomp){
  registrospaciente <- datos %>% filter(nombre == nomb) %>% arrange(inicio)
  despera <- difftime(registrospaciente$inicio[2:6],
                      registrospaciente$fin[1:5], units = "days")
  if (sum(na.omit(despera)<0) != 0){
    print(datos %>% filter(nombre == nomb) %>% select(doc) %>% unique() %>% as.numeric())
    print(which.min(despera))
  }
  if (sum(na.omit(despera) > 15) != 0) {print(nomb)}
  tiemposespera <- append(tiemposespera, na.omit(despera))
  
}

tolerancia <- tiemposespera[tiemposespera<=20] %>% mean() %>% as.numeric() %>% round()

datosnew$status <- NA

for (i in 2:nrow(datosnew)){
  dias <- difftime(date("2022-11-01"),datosnew$fin[i], units = "days")
  cancelado <- "Cancelado" %in% datosnew[i,3:8]
  nivel <- sum(!is.na(datosnew[i,3:8]))
  if (cancelado) {datosnew$status[i] <- "Evento"}
  if (nivel == 1){
    if (dias > (12 + tolerancia)){
      datosnew$status[i] <- "Evento"
    }
  }
  if (nivel == 2){
    if (dias > (8 + tolerancia)){
      datosnew$status[i] <- "Evento"
    }
  }
  if (nivel == 3){
    if (dias > (10 + tolerancia)){
      datosnew$status[i] <- "Evento"
    }
  }
  if (nivel == 4){
    if (dias > (5 + tolerancia)){
      datosnew$status[i] <- "Evento"
    }
  }
  if (nivel == 5){
    if (dias > (8 + tolerancia)){
      datosnew$status[i] <- "Evento"
    }
  }
}

datosnew$status[is.na(datosnew$status)] <- "Censura"

#Creando Categoría de gap
nombresgap <- c()
for (nomb in pacientes){
  registrospaciente <- datos %>% filter(nombre == nomb) %>% arrange(inicio)
  despera <- difftime(registrospaciente$inicio[2:6],
                      registrospaciente$fin[1:5], units = "days")
  if (sum(na.omit(despera) > 15) != 0) {
    print(nomb)
    nombresgap <- append(nombresgap, nomb)
    }
}

datostemp <- read_excel("Datosdepurados.xlsx")
datostemp$GAP <- NA

for (i in 1:nrow(datostemp)){
  if (datostemp$Nombre[i] %in% nombresgap){
    datostemp$GAP[i] <- "GAP > 15 Días"
  }
  else{
    datostemp$GAP[i] <- "GAP < 15 Días"
  }
}