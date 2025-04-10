#Bases de datos en series de tiempo
#Libreria necesaria para leer la base de datos
library(readxl)
library(tidyverse)
library(lubridate)
library(dplyr)
library(tseries)
library(forecast)
library(ggplot2)

# Cargar la base de datos de excell
## Leer el archivo de excel 
excel_sheets("C:\\Users\\Jheremy\\Downloads\\GDP_EE.UU_Serie_de_tiempo1 - copia.xlsx")

# Leer datos
df <- read_xlsx("C:\\Users\\Jheremy\\Downloads\\GDP_EE.UU_Serie_de_tiempo1 - copia.xlsx")

head(df)

#Separar en conjuntos train / test
train = subset(df, observation_date>="1948-01-01" & observation_date<="2018-01-01")
test=subset(df, observation_date>="2019-02-01" & observation_date<="2024-10-01")

#Series de tiempo
s_train=ts(train$GDPC1_PCH, start = c(1948-01-01), frequency = 4)
s_test=ts(test$GDPC1_PCH, start = c(2019-02-01), frequency = 4)
plot(s_train)

#Suavizamiento exponencial
s.exp=ses(s_train,h=23)
plot(s.exp)
lines(s_test,col="darkblue")

#Método de Holt
s.holt=holt(s_train, h=23)
plot(s.holt)
lines(s_test, col="darkblue")

#Método de Holt-Winter
s.hw=forecast(HoltWinters(s_train), h=23)
plot(s.hw)
lines(s_test, col="darkblue")
summary(s.hw)
