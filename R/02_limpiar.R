# R/02_limpiar.R
source("R/01_importar.R")

datos <- datos %>%
  mutate(
    Ciudad          = factor(Ciudad),
    TipoAlojamiento = factor(TipoAlojamiento),
    TipoPropiedad   = factor(TipoPropiedad),
    MetodoReserva   = factor(MetodoReserva),
    Superanfitrion  = factor(Superanfitrion),
    CategoriaZona       = factor(CategoriaZona, ordered = TRUE,
                                 levels = c("Economica","Media","Media-Alta","Premium")),
    PoliticaCancelacion = factor(PoliticaCancelacion, ordered = TRUE,
                                 levels = c("Estricta","Moderada","Flexible")),
    Temporada           = factor(Temporada, ordered = TRUE,
                                 levels = c("Baja","Media","Alta")),
    NivelDemanda        = factor(NivelDemanda, ordered = TRUE,
                                 levels = c("Baja","Media","Alta","Muy alta")),
    NivelValoracion     = factor(NivelValoracion, ordered = TRUE,
                                 levels = c("Bajo","Regular","Bueno","Muy bueno","Excelente"))
  )

# --- Nulos ---
tabla_na <- datos %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Porcentaje") %>%
  filter(Porcentaje > 0) %>%
  arrange(desc(Porcentaje))

write.csv(tabla_na, "outputs/tablas/tabla_na.csv", row.names = FALSE)

# --- Atípicos IQR ---
vars_num <- c("PrecioPorNoche","NumeroNochesReservadas","CostoLimpieza",
              "PrecioTotalReserva","TamanoM2","NumeroHabitaciones",
              "NumeroHuespedes","NumeroResenas","CalificacionPromedio",
              "TasaRespuestaAnfitrion")

detectar_atipicos <- function(x) {
  x <- x[!is.na(x)]
  q1 <- quantile(x, .25); q3 <- quantile(x, .75); iqr <- q3 - q1
  li <- q1 - 1.5*iqr;    ls <- q3 + 1.5*iqr
  n  <- sum(x < li | x > ls)
  c(lim_inf = round(li,1), lim_sup = round(ls,1),
    n_atipicos = n, pct_atipicos = round(n/length(x)*100, 2))
}

tabla_atipicos <- as.data.frame(t(sapply(datos[vars_num], detectar_atipicos))) %>%
  tibble::rownames_to_column("Variable") %>%
  arrange(desc(pct_atipicos))

write.csv(tabla_atipicos, "outputs/tablas/tabla_atipicos.csv", row.names = FALSE)

# --- Guardar datos limpios ---
saveRDS(datos, "data/datos_procesados.rds")

message("✔ Limpieza OK")