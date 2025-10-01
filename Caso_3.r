 # ============================
# Taller de Business Analytics - Caso Tour Marketing (Opry) 
# Objetivo: Explorar la relación entre ventas y publicidad
# Autor: Juan Nicolás Velásquez Rey
# ============================
# 0. Preparación
# Cargar librerías
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)
library(stargazer)

# 1. Exploración inicial de los datos
df <- read_csv("Opry_data.csv")

# 2. Regresión naive (simple)
df$Ventas <- as.numeric(df$Ventas)
df$Gasto_Publicidad <- as.numeric(df$Gasto_Publicidad)
df$Flights_to_Nashville <- as.numeric(df$Flights_to_Nashville)
df$Google_Trends_Opry <- as.numeric(df$Google_Trends_Opry)
df$Organic <- as.numeric(df$Organic)
plot(modelo4$fitted.values, modelo4$residuals,
     xlab = "Valores ajustados",
     ylab = "Residuos",
     main = "Residuos vs Ajustados - Modelo extendido")
abline(h = 0, col = "red")
 1. ¿Cómo interpretan el intercepto?
# 1. ¿Cómo interpretan el intercepto?
el intercepto 799,600  muestra el valor esperado de las ventas cuando el gasto en publicidad es cero 
# 2. como interpreto el coheficiente de gasto publico ?
El coheficiente 6.76 muestra que por cada unidad que si invierte en poblicidad, las ventas aumentan en ese promedio
# 3. Es estadisticamente significativo 
si debido a que el p-value es menor que 0.01 lo que muestra que es altamente significativo. la publicidad influye en las ventas 
3) Convertir fecha y crear variables de tiempo
df$date_parsed <- dmy(df$Date)
df$year <- year(df$date_parsed)
df$month <- month(df$date_parsed)
library(lmtest)
bptest(modelo4)
#   Ventas ~ Gasto_Publicidad + Holliday_seasson
# Preguntas:
# 1. ¿Cómo cambia el coeficiente de Gasto_Publicidad respecto al modelo naive?
esto muetra qye el impacto de la publicidad sigue siendo positivo y significativo peor mas realista. 
# 2. ¿Cómo interpretan el coeficiente de Holliday_seasson?
El coeficiente de Holliday_seasson indica cuánto aumentan en promedio las ventas en temporada navideña (dic–ene), manteniendo constante el gasto en publicidad.

# 4) Crear dummy temporada navideña
df$Holliday_seasson <- ifelse(df$month %in% c(12,1), 1, 0)
# Preguntas:
# 1. ¿Cómo se interpreta ahora el coeficiente de Gasto_Publicidad?
como una elasticidad 
# 2. ¿Qué significa el intercepto en este modelo?
logaritmo del nivel esperado de ventas cuando el gasto en publicidad es 1 y fuera de temporada navideña.
# 5) Resumen de variables
summary(df$Ventas)
summary(df$Gasto_Publicidad)
# Preguntas:
# 1. Interprete el coeficiente de Gasto_Publicidad.
muetra el cambio promedio en ventas por cada unidad adicional en publicidad manteniendo constante el resto de variabeles 
# 2. Interprete el coeficiente de alguna dummy.
de la dummy navideña muetra la diferencia promedio de ventas durante los meces de enero respecto al resto del año, dejando el resto constante 

# 3. Interprete el coeficiente de la nueva variable que agregó.
muetra como cambia el nivel de ventas frente a la variacion en esa nueva variable controlada por los demmas. 

# 6) Gráfico de dispersión
ggplot(df, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Dispersión: Ventas vs Gasto Publicidad")

# 7) Modelo lineal simple
modelo1 <- lm(Ventas ~ Gasto_Publicidad, data = df)
summary(modelo1)
coeftest(modelo1, vcov = vcovHC(modelo1, type = "HC1"))

 # ¿Qué limitaciones tiene su modelo?
 solo explica una parte pequeña de las ventas (el R² es bajo). 
 Eso significa que hay otros factores importantes que no están incluidos, como la economía, las promociones, 
 los eventos especiales o la competencia. Además, asume que la relación entre publicidad y
  ventas es lineal, lo cual puede no ser totalmente cierto.
# - ¿Por qué creen que es un buen o mal modelo para predecir las ventas?
Es un buen modelo para ver la relación general entre publicidad y ventas, 
porque confirma que la publicidad sí influye de manera positiva.
Pero es un mal modelo para predecir con precisión, 
ya que no captura todas las variables que afectan las ventas 
y su poder de predicción es bajo. Para predecir de verdad, habría que incluir más información y 
probar modelos más completos.


