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

# Convertir la columna observation_date a tipo Date
df$observation_date <- as.Date(df$observation_date)
# Verificar que la conversión fue exitosa
head(df)

# Ordenar los datos por la columna observation_date
df <- df %>% arrange(observation_date)
# Crear la serie de tiempo con frecuencia trimestral (4 trimestres por año)
gdp_ts <- ts(df$GDPC1_PCH, start = c(1948, 1), frequency = 4)
# Verificar el comienzo de la serie de tiempo
head(gdp_ts)

# Visualizar la serie de tiempo
plot(gdp_ts, main = "PIB Real (% de cambio trimestral)", xlab = "Año", ylab = "Cambio %")
#Prueba ADF para comprobar la estacionariedad
adf.test(gdp_ts) #(La serie es estacionaria p < 0.05)


#Paso 1. Graficar ACF y PACF [ar q (PACF) i ma p (ACF) (p , 0, q)]
# Graficar ACF y PACF
acf(gdp_ts, main="ACF de la serie de tiempo")
pacf(gdp_ts, main = "PACF de la serie de tiempo")



#Modelos para prueba con los autoregresivos entendiendo que la función central es cero
modelo1=arima(gdp_ts, order = c(1,0,1))
modelo1
tsdiag(modelo1)
Box.test(residuals(modelo1), type = "Ljung-Box")

modelo2=arima(gdp_ts, order = c(2,0,1))
modelo2
tsdiag(modelo2)
Box.test(residuals(modelo2), type = "Ljung-Box")

modelo3=arima(gdp_ts, order = c(3,0,1))
modelo3
tsdiag(modelo3)
Box.test(residuals(modelo3), type = "Ljung-Box")

modelo4=arima(gdp_ts, order = c(3,0,0))
modelo4
tsdiag(modelo4)
Box.test(residuals(modelo4), type = "Ljung-Box")

modelo5=arima(gdp_ts, order = c(2,0,0))
modelo5
tsdiag(modelo5)
Box.test(residuals(modelo5), type = "Ljung-Box")

modelo6=arima(gdp_ts, order = c(1,0,0))
modelo6
tsdiag(modelo6)
Box.test(residuals(modelo6), type = "Ljung-Box")

modelo7=arima(gdp_ts, order = c(0,0,1))
modelo7
tsdiag(modelo7)
Box.test(residuals(modelo7), type = "Ljung-Box")

modelo8=arima(gdp_ts, order = c(1,0,2))
modelo8
tsdiag(modelo8)
Box.test(residuals(modelo8), type = "Ljung-Box")

modelo9=arima(gdp_ts, order = c(1,0,3))
modelo9
tsdiag(modelo9)
Box.test(residuals(modelo9), type = "Ljung-Box")

modelo10=arima(gdp_ts, order = c(0,0,2)) #Mejor modelo
modelo10
tsdiag(modelo10)
Box.test(residuals(modelo10), type = "Ljung-Box")

modelo11=arima(gdp_ts, order = c(0,0,3))
modelo11
tsdiag(modelo11)
Box.test(residuals(modelo11), type = "Ljung-Box")



# Crear la tabla resumen
resultados_modelos <- data.frame(
  Modelo = paste0("modelo", 1:11),
  ARIMA = c("(1,0,1)", "(2,0,1)", "(3,0,1)", "(3,0,0)", "(2,0,0)", "(1,0,0)",
            "(0,0,1)", "(1,0,2)", "(1,0,3)", "(0,0,2)", "(0,0,3)"),
  AIC = c(943.78, 944.17, 944.57, 944.05, 942.27, 943.35, 944.30, 943.75, 945.62, 941.85, 943.69),
  LogLikelihood = c(-467.89, -467.09, -466.29, -467.02, -467.14, -468.67,
                    -469.15, -466.88, -466.81, -466.92, -466.85),
  Sigma2 = c(1.222, 1.215, 1.209, 1.215, 1.216, 1.228, 1.232, 1.214, 1.213, 1.214, 1.213),
  LjungBox_p = c(0.7948, 0.9873, 0.9374, 0.9762, 0.9608, 0.8209, 0.8231,
                 0.9913, 0.9921, 0.9621, 0.991)
)

# Ordenar por AIC (el mejor modelo es el que tiene menor AIC)
resultados_ordenados <- resultados_modelos[order(resultados_modelos$AIC), ]

# Ver los mejores modelos
print(resultados_ordenados)



# Predicción a 10 periodos
pred <- predict(modelo10, n.ahead = 10)

# Graficar serie original y predicción
ts.plot(gdp_ts, pred$pred, lty = c(1,2), col = c("blue", "red"))
lines(pred$pred + 2*pred$se, col = "red", lty = 3)  # upper bound
lines(pred$pred - 2*pred$se, col = "red", lty = 3)  # lower bound
legend("topleft", legend=c("Serie original", "Predicción", "Intervalos 95%"),
       col=c("blue", "red", "red"), lty=c(1,2,3))






# 1. Separar los datos
train <- window(gdp_ts, end = c(2019, 4))    # Serie hasta antes del COVID
test <- window(gdp_ts, start = c(2020, 1))   # Serie desde 2020 hasta 2024

# 2. Ajustar modelo ARIMA(0,0,2) con datos pre-COVID
modelo_arima <- arima(train, order = c(0, 0, 2))

# 3. Hacer predicción para h = longitud del test (por ejemplo 20 trimestres)
h_test <- length(test)
pred_arima <- forecast(modelo_arima, h = h_test)

# 4. Gráfico base: toda la serie real completa
plot(gdp_ts, col = "black", lwd = 2, ylim = range(gdp_ts, pred_arima$lower, pred_arima$upper),
     main = "Predicción contrafactual del PIB real (ARIMA) vs Serie Observada",
     ylab = "Tasa de variación del PIB (%)", xlab = "Año")

# 5. Agregar la predicción del modelo (línea roja)
lines(pred_arima$mean, col = "red", lwd = 2)

# 6. Agregar intervalos de predicción (líneas grises punteadas)
lines(pred_arima$lower[,2], col = "gray", lty = 2)
lines(pred_arima$upper[,2], col = "gray", lty = 2)

# 7. Agregar la serie observada post-COVID (2020 en adelante) en azul
lines(test, col = "blue", lwd = 2)

# 8. Agregar leyenda
legend("bottomleft", legend = c("Serie real (1950–2024)", 
                                "Predicción contrafactual (sin COVID)", 
                                "Serie observada post-2020", 
                                "Intervalos de predicción 95%"),
       col = c("black", "red", "blue", "gray"), lty = c(1,1,1,2), lwd = 2, bty = "n")

