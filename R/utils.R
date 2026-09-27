# R/utils.R — utilidades compartidas
suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(dplyr)
  library(tidyr)
  library(forcats)
})

tema_reporte <- theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40"),
    panel.grid.minor = element_blank(),
    legend.position  = "bottom"
  )
theme_set(tema_reporte)

guardar <- function(g, nombre, ancho = 8, alto = 5) {
  ggsave(
    filename = file.path("outputs/figuras", nombre),
    plot = g, width = ancho, height = alto,
    dpi = 300, bg = "white"
  )
}

# Escala de precio reutilizable (rojos Airbnb-ish)
pal_precio <- c("#FEE0D2","#FC9272","#DE2D26","#A50F15")