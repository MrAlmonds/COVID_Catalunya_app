library(shiny)
library(plotly)
library(DT)
library(shinythemes)
library(xts)
library(dygraphs)
library(dplyr)
library(shinyWidgets)
require(xts)


#source("Functions.R", encoding = "utf-8")
source("Main.R", encoding = "utf-8")
Sys.setlocale("LC_ALL", "Spanish")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Evolució del COVID-19 a Catalunya"),
  hr(),
  dygraphOutput("graf"),
  hr(),
  fluidRow(

    column(4,
           selectInput('aga', 'Seleccionar AGA', c("Totes", agas)),
           selectInput('graf', 'Seleccionar mètrica',
                       c("Positius confirmats",
                         "Exitus confirmats"))
    ),
    column(4,
           awesomeCheckbox(
             inputId = "cum",
             label = "Casos acumulats", 
             value = F,
             status = "danger")
          )
  )
)


server <- function(input, output) {
  
  df_filtro_aga <- reactive({
    
    FiltraDatos(metrica = input$graf,
                aga = input$aga,
                acum = input$cum)})
  
  output$graf <- renderDygraph({
    message("renderDygraph")
    dygraph(df_filtro_aga()) %>%
      dyOptions(colors = c("#f27e16", "#00a51f", "#800080"), digitsAfterDecimal = 0, animatedZooms = T) %>%
      dyRangeSelector(height = 40) %>%
      dyLegend(hideOnMouseOut = T, labelsSeparateLines = F, width = 1000) %>%
      dyAxis("y", label = "Casos") %>%
      dyEvent("2020-7-08", "Mascareta obligatòria    ", labelLoc = "top") %>%
      dyEvent("2020-12-27", "Inici vacunació    ", labelLoc = "top") %>%
      
      dyEvent("2020-10-25", "Confinament municipal    ", labelLoc = "top") %>%
      
      dyEvent("2021-06-26", "Mascareta no obligatòria    ", labelLoc = "top") %>%
      
      dyHighlight(highlightSeriesOpts = list(strokeWidth = 3)) %>% 
      dyUnzoom() %>% 
      dyCrosshair(direction = "vertical")
  })

  
}

shinyApp(ui = ui, server = server)
