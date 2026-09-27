# ABP1 — Airbnb México (AED)

Análisis exploratorio de 4,000 reservas de Airbnb en 5 ciudades mexicanas.

## Reproducir

```r
renv::restore()
source("R/03_graficos.R")
quarto::quarto_render("presentacion.qmd")