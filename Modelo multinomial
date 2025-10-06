# Modelo-multinomial-
Clase 06/10/2025
# ==========================================================
# Universidad de Guanajuato
# Alumnas: itzel y Paloma
# Tema: Modelos categóricos multinomiales
# Ejemplo: Espectro politico  (México)
# ==========================================================

# (1) Instalar y cargar librerías
install.packages("nnet")
install.packages("marginaleffects")
install.packages("ggplot2")
install.packages("readxl")

library(nnet)
library(marginaleffects)
library(dplyr)
library(ggplot2)
library(readxl)

# (2) Cargar base de datos (archivo en tu carpeta de trabajo)
datos <- read_excel("baseles.xlsx")
head(datos)
str(datos)

# Variable dependiente: Preferencia (3 categorías: Izquierda, Centro, Derecha)
table(datos$Preferencia)

# (3) Ajustar modelo logit multinomial
mod_multinom <- multinom(Preferencia ~ Edad + Genero + Ingreso + Educacion, 
                         data = datos, trace = FALSE)

summary(mod_multinom)

# (4) Coeficientes e interpretación
exp(coef(mod_multinom))

# (5) Average Marginal Effects (AME)
ame_edad <- avg_slopes(mod_multinom, variables = "Edad", type = "probs")
ame_edad

ame_ingreso <- avg_slopes(mod_multinom, variables = "Ingreso", type = "probs")
ame_ingreso
