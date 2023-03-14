library(dplyr)
library(survival)

#cargando datos
library(readxl)
Datosdepurados <- read_excel("Datosdepurados.xlsx")

#variable semanas
Datosdepurados$semanas <- difftime(Datosdepurados$`Fecha Fin`,
                                   Datosdepurados$`Fecha Incio`, 
                                   units = "weeks") %>%
  as.numeric() %>% round(2)

#definiendo evento para km
Datosdepurados$evento <- ifelse(Datosdepurados$Status == "Evento", 1, 0)

#Estimando S(t) via kaplan-meier
KM_fit <- survfit(Surv(semanas, evento) ~ 1, data = Datosdepurados)

#graficando S(t)
library(ggfortify)
library(plotly)

p <- autoplot(KM_fit, type="fill",surv.alpha=0.9, 
         surv.size=1.5,
         conf.int.alpha=0,
         censor.size=0,
         surv.colour="#666666")+ 
  labs(x = "\n Tiempo en el tratamiento (Semanas) ", 
       y = "Probabilidad de continuar \n", 
       title = "Curva de supervivencia estimada:\n TMO para pacientes con apnea del sueño",
       caption = "Usando el estimador de Kaplan-Meier") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 13),
        axis.title.y = element_text(face="bold",  size = 13),
        legend.title = element_text(face="bold", size = 11)) +
  geom_vline(xintercept = c(12,20,30,35,43), linetype="dashed", 
             color = "black", size=0.5) +
  geom_label(aes(x=12, y = 1, label = "Nivel 1")) +
  geom_label(aes(x=20, y = 1, label = "Nivel 2")) +
  geom_label(aes(x=30, y = 1, label = "Nivel 3")) +
  geom_label(aes(x=35, y = 1, label = "Nivel 4")) +
  geom_label(aes(x=43, y = 1, label = "Nivel 5")) +
  geom_label(aes(x=5, y = 0.25,
  label = "La probabilidad de que el paciente\n continúe a partir del nivel 1 es 37%")) +
  geom_segment(aes(x=0, xend=12,y=0.3704484,yend=0.3704484),
               linetype="dashed") +
  geom_segment(aes(x=12,xend=10,y=0.3704484, yend=0.3),
               arrow = arrow(length = unit(0.3,"cm")), size = 1, col = "red")

p2 <- autoplot(KM_fit, type="fill",surv.alpha=0.9, 
               surv.size=1.5,
               conf.int.alpha=0,
               censor.size=0,
               surv.colour="#666666")+ 
  labs(x = "\n Tiempo en el tratamiento (Semanas) ", 
       y = "Probabilidad de continuar \n", 
       title = "Curva de supervivencia estimada:\n TMO para pacientes con apnea del sueño",
       caption = "Usando el estimador de Kaplan-Meier") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 13),
        axis.title.y = element_text(face="bold",  size = 13),
        legend.title = element_text(face="bold", size = 11)) +
  geom_vline(xintercept = c(12,20,30,35,43), linetype="dashed", 
             color = "black", size=0.5) +
  geom_segment(aes(x=0, xend=12,y=0.3704484,yend=0.3704484),
               linetype="dashed")
  
ggplotly(p2)
