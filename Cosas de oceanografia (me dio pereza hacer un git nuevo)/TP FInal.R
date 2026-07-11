library(tidyverse)
library(readr)


# Data --------------------------------------------------------------------


Cuantificacion_nutrientes_bonito_xlsx_tidy_data <- read_csv("Data/Cuantificacion nutrientes bonito.xlsx - tidy data.csv")
View(Cuantificacion_nutrientes_bonito_xlsx_tidy_data)



# Calibration curves ------------------------------------------------------


Cuantificacion_nutrientes_bonito_xlsx_calibracion <- read_csv("Data/Cuantificacion nutrientes bonito.xlsx - calibracion.csv")
View(Cuantificacion_nutrientes_bonito_xlsx_calibracion)

nitritos_lm<-lm(Cuantificacion_nutrientes_bonito_xlsx_calibracion$absorbancia~Cuantificacion_nutrientes_bonito_xlsx_calibracion$concentracion+0,
                data = Cuantificacion_nutrientes_bonito_xlsx_calibracion,
                subset=(medicion=="Nitritos")
                )
summary(nitritos_lm)

Curva_calibracion_nitritos<-ggplot(subset(Cuantificacion_nutrientes_bonito_xlsx_calibracion,medicion=="Nitritos"),
                                   aes(x=concentracion,
                                       y=absorbancia)) +
 
  geom_smooth(method = "lm", formula = y ~ x+0, se=F)+
  geom_point(size=3, color="cyan3") +
  labs(title="Curva de calibración nitritos")+
  xlab(expression("Concentración"~(mu*mol~L^-1)))+
  ylab("Absorbancia")


Curva_calibracion_nitritos



Amonio_lm<-lm(Cuantificacion_nutrientes_bonito_xlsx_calibracion$absorbancia~Cuantificacion_nutrientes_bonito_xlsx_calibracion$concentracion+0,
                data = Cuantificacion_nutrientes_bonito_xlsx_calibracion,
                subset=(medicion=="Amonio")
)
summary(Amonio_lm)

Curva_calibracion_amonio<-ggplot(subset(Cuantificacion_nutrientes_bonito_xlsx_calibracion,medicion=="Amonio"),
                                   aes(x=concentracion,
                                       y=absorbancia)) +

  geom_smooth(method = "lm", formula = y ~ x+0, se=F)+
  geom_point(size=3, color="cyan3") +
  labs(title="Curva de calibración amonio")+
  xlab(expression("Concentración"~(mu*mol~L^-1)))+
  ylab("Absorbancia")


Curva_calibracion_amonio




fosforo_lm<-lm(Cuantificacion_nutrientes_bonito_xlsx_calibracion$absorbancia~Cuantificacion_nutrientes_bonito_xlsx_calibracion$concentracion+0,
                data = Cuantificacion_nutrientes_bonito_xlsx_calibracion,
                subset=(medicion=="fosforo")
)
summary(fosforo_lm)

Curva_calibracion_fosforo<-ggplot(subset(Cuantificacion_nutrientes_bonito_xlsx_calibracion,medicion=="fosforo"),
                                   aes(x=concentracion,
                                       y=absorbancia)) +
 
  geom_smooth(method = "lm", formula = y ~ x+0, se=F)+
  geom_point(size=3, color="cyan3") +
  labs(title="Curva de calibración fosforo")+
  xlab(expression("Concentración"~(mu*mol~L^-1)))+
  ylab("Absorbancia")


Curva_calibracion_fosforo




silicio_lm<-lm(Cuantificacion_nutrientes_bonito_xlsx_calibracion$absorbancia~Cuantificacion_nutrientes_bonito_xlsx_calibracion$concentracion+0,
               data = Cuantificacion_nutrientes_bonito_xlsx_calibracion,
               subset=(medicion=="silicio")
)
summary(silicio_lm)

Curva_calibracion_silicio<-ggplot(subset(Cuantificacion_nutrientes_bonito_xlsx_calibracion,medicion=="silicio"),
                                  aes(x=concentracion,
                                      y=absorbancia)) +
 
  geom_smooth(method = "lm", formula = y ~ x+0, se=F)+
  geom_point(size=3, color="cyan3") +
  labs(title="Curva de calibración silicio")+
  xlab(expression("Concentración"~(mu*mol~L^-1)))+
  ylab("Absorbancia")


Curva_calibracion_silicio
