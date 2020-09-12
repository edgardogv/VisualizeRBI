library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
ui <- dashboardPage(title = "edgardogv.com", skin = "blue",
                    dashboardHeader(title = "VisualizeRBI"),
                    dashboardSidebar(
                      sidebarMenu(id = "menu",
                                  menuItem("Lectura de Datos", tabName = "lectura", 
                                           icon = icon("eye")),
                                  menuItem("Visualización de Datos", tabName = "visualizacion", 
                                           icon = icon("dashboard")),
                                  conditionalPanel("input.menu == 'visualizacion'", 
                                                   uiOutput("control_ejex"),
                                                   conditionalPanel("input.eje_x != 'Ninguna'",
                                                                    uiOutput("control_ejey")          
                                                   )
                                  )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "lectura",
                                box(title = "Configuración", width = 3, 
                                    fileInput("datos", "Cargar Datos:",
                                              accept = c(".csv", ".txt", ".xlsx", ".xls"),
                                              buttonLabel = "Cargar...",
                                              placeholder = "Ningún archivo seleccionado"),
                                    uiOutput("controles_excel")
                                ),
                                tabBox(title = "Vista Previa", width = 9,
                                       tabPanel("Estructura",
                                                verbatimTextOutput("estructura_crudos")
                                       ),
                                       tabPanel("Datos Crudos",
                                                DTOutput("datos_crudos")
                                       )
                                )
                        ),
                        tabItem(tabName = "visualizacion",
                                box(title = "Gráfico", width = 6,
                                    plotlyOutput("grafico")
                                ),
                                box(title = "Datos", width = 6,
                                    DTOutput("datos_crudos_2")
                                )
                        )
                      )
                    )
)
server <- function(input, output) {
  
  output$controles_excel <- renderUI({
    validate(need(!is.null(input$datos$datapath), ""))
    if(tolower(tools::file_ext(input$datos$datapath)) %in% c("xls", "xlsx")){
      hojas <- excel_sheets(input$datos$datapath)
      sliderInput("hoja_excel", "Hoja del Documento:",
                  min = 1, max = length(hojas), value = 1, step = 1)
    }
  })
  
  output$control_ejex <- renderUI({
    tagList(
      selectInput("eje_x", "Eje X", choices = c("Ninguna", colnames(datos_cargados())),
                  selected = "Ninguna", multiple = F),
      checkboxInput("convertir", "A Texto", F)
    )
  })
  
  output$control_ejey <- renderUI({
    selectInput("eje_y", "Eje Y", choices = c("Ninguna", colnames(datos_cargados())),
                selected = "Ninguna", multiple = F)
  })
  
  datos_cargados <- reactive({
    validate(need(!is.null(input$datos$datapath), "Esperando por archivo..."))
    if(tolower(tools::file_ext(input$datos$datapath)) %in% c("xls", "xlsx")){
      datos <- read_excel(input$datos$datapath, sheet = input$hoja_excel)
    } else {
      datos <- fread(input$datos$datapath)
    }
    return(datos)
  })
  
  output$datos_crudos <- renderDT({
    return(datos_cargados())
  }, options = list(scrollX = TRUE))
  
  output$estructura_crudos <- renderPrint({
    str(datos_cargados())
  })
  
  output$grafico <- renderPlotly({
    datos <- as.data.frame(datos_cargados())
    validate(need(input$eje_x != "Ninguna", "Seleccione la variable a graficar en el eje X."))
    if(input$convertir){
      datos[, input$eje_x] <- as.character(datos[, input$eje_x])
    }
    if(is.numeric(datos[, input$eje_x]) & input$eje_y == "Ninguna"){
      g <- ggplot(datos, aes_string(x = input$eje_x))+
        geom_density(color="darkblue", fill="lightblue")
    } else if(any(is.character(datos[, input$eje_x]) | 
                  is.factor(datos[, input$eje_x])) & 
              input$eje_y == "Ninguna"){
      tab <- table(datos[, input$eje_x]) %>%
        as.data.frame()
      colnames(tab) <- c(input$eje_x, "Total")
      g <- ggplot(tab, aes_string(x=input$eje_x, y = "Total")) +
        geom_bar(stat="identity", fill="steelblue")+
        theme_minimal()
    } else if(is.numeric(datos[, input$eje_x]) & is.numeric(datos[, input$eje_y])){
      g <- ggplot(datos, aes_string(x = input$eje_x, y = input$eje_y)) +
        geom_point() +
        theme_minimal()
    } else if(any(is.character(datos[, input$eje_x]) | 
                  is.factor(datos[, input$eje_x])) & 
              is.numeric(datos[, input$eje_y])){
      g <- ggplot(datos, aes_string(x = input$eje_x, y = input$eje_y)) +
        geom_boxplot(aes_string(fill = input$eje_x)) +
        theme_minimal()
    } else {
      g <- NULL
    }
    return(ggplotly(g))
  })
  
  output$datos_crudos_2 <- renderDT({
    return(datos_cargados())
  }, filter = "top", options = list(scrollX = TRUE))
  
}
shinyApp(ui = ui, server = server)