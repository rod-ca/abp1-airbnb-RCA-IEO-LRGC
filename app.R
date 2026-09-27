library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

datos <- readRDS("data/datos_procesados.rds")

ui <- fluidPage(
  titlePanel("Explorador Airbnb México"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("ciudades", "Ciudades",
                         choices  = levels(datos$Ciudad),
                         selected = levels(datos$Ciudad)),
      checkboxGroupInput("tipos", "Tipo de alojamiento",
                         choices  = levels(datos$TipoAlojamiento),
                         selected = levels(datos$TipoAlojamiento)),
      sliderInput("rango", "Rango de precio (MXN)",
                  min = 0, max = 7070,
                  value = c(0, 7070), step = 100)
    ),
    mainPanel(
      plotlyOutput("box", height = "360px"),
      plotlyOutput("heat", height = "420px")
    )
  )
)

server <- function(input, output, session) {
  filtrado <- reactive({
    req(input$ciudades, input$tipos)
    datos %>%
      filter(Ciudad %in% input$ciudades,
             TipoAlojamiento %in% input$tipos,
             between(PrecioPorNoche, input$rango[1], input$rango[2]))
  })
  
  output$box <- renderPlotly({
    ggplotly(
      ggplot(filtrado(), aes(Ciudad, PrecioPorNoche, fill = Ciudad)) +
        geom_boxplot(show.legend = FALSE, outlier.color = "red", alpha = .7) +
        theme_minimal() +
        labs(x = NULL, y = "Precio por noche (MXN)")
    )
  })
  
  output$heat <- renderPlotly({
    d <- filtrado() %>%
      group_by(Ciudad, TipoAlojamiento) %>%
      summarise(p = mean(PrecioPorNoche, na.rm = TRUE), .groups = "drop")
    ggplotly(
      ggplot(d, aes(TipoAlojamiento, Ciudad, fill = p)) +
        geom_tile(color = "white") +
        geom_text(aes(label = round(p)), size = 3) +
        scale_fill_gradient(low = "#FEE0D2", high = "#A50F15") +
        theme_minimal() +
        labs(x = NULL, y = NULL, fill = "MXN")
    )
  })
}

shinyApp(ui, server)