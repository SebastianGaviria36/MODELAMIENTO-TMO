library(dplyr)
library(survival)

#cargando datos
library(readxl)
Datosdepurados <- read_excel("Datosdepurados.xlsx")

#variable días
Datosdepurados$dias <- difftime(Datosdepurados$`Fecha Fin`,
                                   Datosdepurados$`Fecha Incio`, 
                                   units = "days") %>%
  as.numeric() %>% round(2)

#definiendo evento para km
Datosdepurados$evento <- ifelse(Datosdepurados$Status == "Evento", 1, 0)

#Estimando S(t) via kaplan-meier
KM_fit <- survfit(Surv(dias, evento) ~ 1, data = Datosdepurados)

#graficando S(t)
library(ggfortify)
library(plotly)

p <- autoplot(KM_fit, type="fill",surv.alpha=0.9, 
              surv.size=1.5,
              conf.int.alpha=0,
              censor.size=0,
              surv.colour="#666666")+ 
  labs(x = "\n Tiempo en el tratamiento (Días) ", 
       y = "Probabilidad de continuar \n", 
       title = "Curva de supervivencia estimada:\n TMO para pacientes con apnea del sueño",
       caption = "Usando el estimador de Kaplan-Meier") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 12),
        axis.title.y = element_text(face="bold",  size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  geom_vline(xintercept = c(21,36,57,74,94,121), linetype="dashed", 
             color = "black", size=0.5) +
  xlim(0,125) +
  #ylim(50,100) +
  geom_label(aes(x=21, y = 1, label = "Nivel 1")) +
  geom_label(aes(x=36, y = 1, label = "Nivel 2")) +
  geom_label(aes(x=57, y = 1, label = "Nivel 3")) +
  geom_label(aes(x=74, y = 1, label = "Nivel 4")) +
  geom_label(aes(x=94, y = 1, label = "Nivel 5")) +
  geom_label(aes(x=121, y = 1, label = "Nivel 6")) +
  geom_label(aes(x=15, y = 0.5,
                 label = "La probabilidad de que el paciente\n continúe a partir del nivel 1 es 84.2%")) +
  geom_segment(aes(x=0, xend=21,y=0.8421349,yend=0.8421349),
               linetype="dashed") +
  geom_segment(aes(x=21,xend=15,y=0.8421349, yend=0.55),
               arrow = arrow(length = unit(0.3,"cm")), size = 1, col = "red")

p2 <- autoplot(KM_fit, type="fill",surv.alpha=0.9, 
               surv.size=1.5,
               conf.int.alpha=0,
               censor.size=0,
               surv.colour="#666666")+ 
  labs(x = "\n Tiempo en el tratamiento (Días) ", 
       y = "Probabilidad de continuar \n", 
       title = "Curva de supervivencia estimada:\n TMO para pacientes con apnea del sueño",
       caption = "Usando el estimador de Kaplan-Meier") +
  xlim(0,125) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold",  size = 13),
        axis.title.y = element_text(face="bold",  size = 13),
        legend.title = element_text(face="bold", size = 11)) +
  geom_vline(xintercept = c(21,36,57,74,94,121), linetype="dashed", 
             color = "black", size=0.5) +
  geom_segment(aes(x=0, xend=21,y=0.8421349,yend=0.8421349),
               linetype="dashed")
  
ggplotly(p2)
