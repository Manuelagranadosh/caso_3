# ==========================
# CASO 3 - ANALISIS EN R
# ==========================

# 1) Instalar y cargar paquetes ----
required_pkgs <- c(
  "readr","dplyr","lubridate","ggplot2",
  "car","sandwich","lmtest","stargazer","broom","MASS"
)

for(p in required_pkgs){
  if(!requireNamespace(p, quietly = TRUE)){
    install.packages(p, repos="https://cloud.r-project.org")
  }
}

library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(car)
library(sandwich)
library(lmtest)
library(stargazer)
library(broom)
library(MASS)

# 2) Cargar datos ----
df <- read_csv("Opry_data.csv", col_types = cols(.default = "c"))

# Forzar numéricos en columnas clave
num_cols <- c("Gasto_Publicidad","Ventas","Flights_to_Nashville",
              "Google_Trends_Opry","Google_Trend_Nashville",
              "Ordenes","Organic")
num_cols <- intersect(num_cols, names(df))
for(cn in num_cols){
  df[[cn]] <- as.numeric(df[[cn]])
}

cat("Dimensiones: ", dim(df), "\n")
print(head(df))
print(sapply(df, class))

# 3) Parsear fecha ----
if("Date" %in% names(df)){
  df$date_parsed <- dmy(df$Date)
  if(all(is.na(df$date_parsed))){
    df$date_parsed <- ymd(df$Date)
  }
} else {
  stop("No se encontró columna 'Date'. Ajusta el nombre en el script.")
}

print(head(df[, c("Date","date_parsed")]))

# 4) Crear variables de tiempo y dummy ----
df$year  <- year(df$date_parsed)
df$month <- month(df$date_parsed)
df$Holliday_seasson <- ifelse(df$month %in% c(12,1), 1, 0)

# 5) Inspección rápida ----
summary(select(df, any_of(c("Ventas","Gasto_Publicidad"))))

