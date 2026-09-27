# R/03_graficos.R
source("R/02_limpiar.R")

# --- Estáticos (para PDF / respaldo) ---

g_ciudad <- datos %>% count(Ciudad) %>%
  mutate(pct = n/sum(n)) %>%
  ggplot(aes(fct_reorder(Ciudad, pct), pct, fill = Ciudad)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = percent(pct, 0.1)), hjust = -0.1, size = 4) +
  scale_y_continuous(labels = percent, limits = c(0, .4)) +
  coord_flip() +
  labs(x = NULL, y = "Porcentaje de reservas",
       title = "CDMX concentra 1 de cada 3 reservas")
guardar(g_ciudad, "fig_ciudad.png")

g_tipo <- ggplot(datos, aes(fct_infreq(TipoAlojamiento))) +
  geom_bar(fill = "#E86AA6") +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.4, size = 4) +
  labs(x = NULL, y = "Frecuencia absoluta",
       title = "Departamento completo domina la oferta")
guardar(g_tipo, "fig_tipo.png")

g_precio <- ggplot(datos, aes(PrecioPorNoche)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = "steelblue", color = "white") +
  geom_density(color = "orange", linewidth = 1) +
  scale_x_continuous(labels = label_comma()) +
  labs(x = "Precio por noche (MXN)", y = "Densidad",
       title = "Distribución sesgada a la derecha")
guardar(g_precio, "fig_precio.png")

g_box_ciudad <- ggplot(datos, aes(Ciudad, PrecioPorNoche, fill = Ciudad)) +
  geom_boxplot(show.legend = FALSE, outlier.color = "red", alpha = .7) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = "Precio por noche (MXN)",
       title = "CDMX y Monterrey lideran; Oaxaca y Puebla son más baratas")
guardar(g_box_ciudad, "fig_box_ciudad.png")

g_box_tipo <- ggplot(datos, aes(TipoAlojamiento, PrecioPorNoche, fill = TipoAlojamiento)) +
  geom_boxplot(show.legend = FALSE, outlier.color = "red", alpha = .7) +
  scale_y_continuous(labels = label_comma()) +
  labs(x = NULL, y = "Precio por noche (MXN)",
       title = "Casa completa: el segmento más caro") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
guardar(g_box_tipo, "fig_box_tipo.png")

g_apiladas <- ggplot(datos, aes(Ciudad, fill = TipoAlojamiento)) +
  geom_bar(position = "fill", color = "white", linewidth = .2) +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = NULL, y = "Porcentaje", fill = NULL,
       title = "Composición del alojamiento por ciudad") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
guardar(g_apiladas, "fig_apiladas.png")

heat_data <- datos %>%
  group_by(Ciudad, TipoAlojamiento) %>%
  summarise(precio = mean(PrecioPorNoche, na.rm = TRUE), .groups = "drop")

g_heat <- ggplot(heat_data, aes(TipoAlojamiento, Ciudad, fill = precio)) +
  geom_tile(color = "white") +
  geom_text(aes(label = comma(round(precio))), size = 3.5) +
  scale_fill_gradient(low = "#FEE0D2", high = "#A50F15", labels = comma) +
  labs(x = NULL, y = NULL, fill = "Precio\nmedio (MXN)",
       title = "Casa completa es la más cara en todas las ciudades") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))
guardar(g_heat, "fig_heat.png", ancho = 9, alto = 5)

# --- Interactivos (para revealjs) ---

g_ciudad_i  <- ggplotly(g_ciudad,  tooltip = c("x","y"))
g_precio_i  <- ggplotly(g_precio,  tooltip = c("x","y"))
g_boxc_i    <- ggplotly(g_box_ciudad, tooltip = c("x","y"))
g_boxt_i    <- ggplotly(g_box_tipo,   tooltip = c("x","y"))
g_apil_i    <- ggplotly(g_apiladas,   tooltip = c("x","fill","y"))
g_heat_i    <- ggplotly(g_heat, tooltip = c("x","y","fill"))

# Mapa leaflet por ciudad
coords <- data.frame(
  Ciudad = c("Ciudad de Mexico","Guadalajara","Monterrey","Oaxaca","Puebla"),
  lat = c(19.4326, 20.6597, 25.6866, 17.0732, 19.0414),
  lon = c(-99.1332, -103.3496, -100.3161, -96.7266, -98.2063)
)

resumen_mapa <- datos %>%
  group_by(Ciudad) %>%
  summarise(precio = mean(PrecioPorNoche, na.rm = TRUE),
            n = n(), .groups = "drop") %>%
  left_join(coords, by = "Ciudad")

mapa <- leaflet(resumen_mapa) %>%
  addProviderTiles(providers$OpenStreetMap) %>%
  addCircleMarkers(
    ~lon, ~lat,
    radius = ~sqrt(n)/2.5,
    color = "darkred", fillOpacity = .6,
    popup = ~paste0("<b>", Ciudad, "</b><br>",
                    "Reservas: ", n, "<br>",
                    "Precio medio: $", round(precio), " MXN")
  )

# Matriz de correlación como imagen
png("outputs/figuras/fig_corr.png", width = 2000, height = 1600, res = 250)
M <- cor(datos[vars_num], use = "pairwise.complete.obs")
corrplot(M, method = "color", type = "upper",
         addCoef.col = "black", number.cex = .7,
         col = colorRampPalette(c("#C53740","#FFFFFF","#22763F"))(200),
         tl.col = "black", tl.srt = 45, mar = c(0,0,2,0))
dev.off()

message("✔ Gráficos y objetos interactivos listos")