#Preguntas: ¿Cuánto se demora en promedio un paciente para completar cada nivel?
#           ¿Cuánto se demora en promedio un paciente para terminar el tratamiento?

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

#arreglando fecha fin
datos$fin <- substr(datos$fin,1,10)
datos$fin <- as.POSIXlt.character(datos$fin, tz = "UTC", format = "%d/%m/%Y")

#Quitando los cancelados
datos <- datos %>% filter(estado == "Completado")

#Calculando días
datos$dias <- difftime(datos$fin,
                       datos$inicio, 
                       units = "days") %>%
  as.numeric()

#Calculando tiempo promedio por nivel
df_dias_promedio <- datos %>%
                    group_by(nivel) %>%
                    summarise_at(vars(dias), list(días_promedio = mean))

#Calculando tiempo promedio por tratamiento completo
Datosdepurados <- read_excel("Datosdepurados.xlsx")
Datosdepurados$dias <- difftime(Datosdepurados$`Fecha Fin`,
                                Datosdepurados$`Fecha Incio`, 
                                units = "days") %>%
  as.numeric() %>% round(2)
Datosdepurados %>% filter(!is.na(`TMO NIVEL 6`)) %>% select(dias) %>% summary() 
