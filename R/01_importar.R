# R/01_importar.R
source("R/00_setup.R")

datos <- readxl::read_excel("data/airbnb.xlsx", sheet = "BASE_DATOS")

stopifnot(nrow(datos) == 4000, ncol(datos) == 21)
stopifnot(sum(duplicated(datos$ID_Reserva)) == 0)
stopifnot(all(datos$PrecioPorNoche >= 0, na.rm = TRUE))

message("✔ Importación OK: ", nrow(datos), " filas × ", ncol(datos), " columnas")