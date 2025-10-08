###############################################################
# Business Analytics - Caso Tour Marketing (Opry dataset)
# Exploración y regresión simple: Ventas ~ Gasto en Publicidad
# Autor: Juan Nicolás Velásquez Rey
###############################################################

# =============================================================
# 0. Preparación
# =============================================================

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(sandwich)
library(lmtest)
library(stargazer)

# =============================================================
# 1. Cargar y explorar los datos
# =============================================================

# Leer el archivo CSV con la ruta completa
df <- read_csv("/Users/charry/Desktop/casoharvard2/casoharvard3/caso_3/Opry_data.csv")

# Exploración inicial
glimpse(df)
summary(df)
head(df)

# Convertir variables a numéricas
df$Ventas <- as.numeric(df$Ventas)
df$Gasto_Publicidad <- as.numeric(df$Gasto_Publicidad)
df$Flights_to_Nashville <- as.numeric(df$Flights_to_Nashville)
df$Google_Trends_Opry <- as.numeric(df$Google_Trends_Opry)
df$Organic <- as.numeric(df$Organic)

# =============================================================
# 2. Gráfico de dispersión
# =============================================================

ggplot(df, aes(x = Gasto_Publicidad, y = Ventas)) +
  geom_point(color = "darkblue", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(title = "Relación entre Gasto en Publicidad y Ventas",
       x = "Gasto en Publicidad (USD)",
       y = "Ventas (USD)") +
  theme_minimal()

# =============================================================
# 3. Modelo de regresión simple
# =============================================================

modelo1 <- lm(Ventas ~ Gasto_Publicidad, data = df)
summary(modelo1)

# Errores robustos (White)
coeftest(modelo1, vcov = vcovHC(modelo1, type = "HC1"))

# =============================================================
# 4. Diagnóstico gráfico del modelo
# =============================================================

## --- OPCIÓN 1: Gráficas individuales (una por una) ---
plot(modelo1, which = 1)  # Residuos vs Ajustados
plot(modelo1, which = 2)  # QQ Plot de residuos
plot(modelo1, which = 3)  # Escala-Localización
plot(modelo1, which = 5)  # Residuos estandarizados vs leverage

## --- OPCIÓN 2: Las 4 gráficas al tiempo (en un solo run) ---
par(mfrow = c(2,2))       # Dividir ventana en 2x2
plot(modelo1)             # Muestra las 4 juntas
par(mfrow = c(1,1))       # Restaurar a una sola ventana

# =============================================================
# 5. Interpretación modelo 1
# =============================================================
# 1. El intercepto (~799,600) muestra el valor esperado de las ventas
#    cuando el gasto en publicidad es cero.
# 2. El coeficiente de Gasto_Publicidad (~6.76) indica que por cada USD
#    adicional invertido en publicidad, las ventas aumentan en promedio
#    en 6.76 USD.
# 3. Es estadísticamente significativo (p-value < 0.01), lo que indica
#    que la publicidad influye positivamente en las ventas.

# =============================================================
# 6. Variables de tiempo y dummy navideña
# =============================================================

df$date_parsed <- dmy(df$Date)
df$year <- year(df$date_parsed)
df$month <- month(df$date_parsed)
df$Holliday_seasson <- ifelse(df$month %in% c(12, 1), 1, 0)

# =============================================================
# 7. Modelo extendido
# =============================================================

modelo4 <- lm(Ventas ~ Gasto_Publicidad + Holliday_seasson, data = df)
summary(modelo4)

# Test de heterocedasticidad (Breusch-Pagan)
bptest(modelo4)

# =============================================================
# 8. Diagnóstico gráfico del modelo extendido
# =============================================================

## --- OPCIÓN 1: Individual ---
plot(modelo4, which = 1)
plot(modelo4, which = 2)
plot(modelo4, which = 3)
plot(modelo4, which = 5)

## --- OPCIÓN 2: Todas al tiempo ---
par(mfrow = c(2,2))
plot(modelo4)
par(mfrow = c(1,1))

# =============================================================
# 9. Reflexión final
# =============================================================
# El modelo simple muestra que la publicidad tiene una relación positiva
# con las ventas, pero el bajo R² indica que explica solo una parte de la
# variabilidad total. Para mejorar las predicciones, se deben incluir
# más variables (promociones, competencia, eventos, economía, etc.).
# ==========================================================
