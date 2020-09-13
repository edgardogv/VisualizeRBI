library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)

source("funciones/num_txt_detect.R")

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
                                  ),
                                  menuItem("Filtros", tabName = "filtros", icon = icon("filter")),
                                  conditionalPanel("input.menu == 'filtros'",
                                                   actionButton("reset", "Restablecer Filtros"),
                                                   uiOutput("sel_vars"),
                                                   uiOutput("filtros")
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
                                fluidRow(
                                  infoBoxOutput("var_info", width = 4),
                                  valueBoxOutput("val_box1", width = 4),
                                  valueBoxOutput("val_box2", width = 4)
                                ),
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
server <- function(input, output, session) {
  
  tipo_grafico <- NULL
  makeReactiveBinding("tipo_grafico")
  
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
  
  output$sel_vars <- renderUI({
    df <- as.data.frame(datos_cargados())
    res_deteccion <- num_txt_detect(df)
    vars_num <- selectInput("varsNum", "Variables Numéricas",
                            choices = c("Ninguna", colnames(df)[res_deteccion$Numericas]),
                            selected = "Ninguna")
    vars_txt <- selectInput("varsTxt", "Variables Texto",
                            choices = c("Ninguna", colnames(df)[res_deteccion$Texto]),
                            selected = "Ninguna")
    
    return(tagList(
      vars_num,
      vars_txt
    ))
  })
  
  output$filtros <- renderUI({
    df <- as.data.frame(datos_cargados())
    filtro_txt <- NULL
    filtro_num <- NULL
    if(!is.null(input$varsTxt)){
      if(input$varsTxt != "Ninguna"){
        filtro_txt <- checkboxGroupInput("filtroTxt", paste("Filtro",  input$varsTxt),
                                         choices = unique(df[, input$varsTxt]))
      }
    }
    
    if(!is.null(input$varsNum)){
      if(input$varsNum != "Ninguna"){
        minimo <- min(df[, input$varsNum], na.rm = T)
        maximo <- max(df[, input$varsNum], na.rm = T)
        filtro_num <- sliderInput("filtroNum", paste("Filtro", input$varsNum),
                                  min = minimo, max = maximo, value = c(minimo, maximo))
      }
    }
    
    return(tagList(
      filtro_num,
      filtro_txt
    ))
  })
  
  observeEvent(input$reset,{
    df <- as.data.frame(datos_cargados())
    if(!is.null(input$varsNum)){
      if(input$varsNum != "Ninguna"){
        minimo <- min(df[, input$varsNum], na.rm = T)
        maximo <- max(df[, input$varsNum], na.rm = T)
        updateSliderInput(session, "filtroNum", paste("Filtro", input$varsNum),
                                  min = minimo, max = maximo, value = c(minimo, maximo))
      }
    }
    
    if(!is.null(input$varsTxt)){
      if(input$varsTxt != "Ninguna"){
        updateCheckboxGroupInput(session, "filtroTxt", paste("Filtro",  input$varsTxt),
                                         choices = unique(df[, input$varsTxt]))
      }
    }
  })
  
  datos_filtrados <- reactive({
    datos_filt <- as.data.frame(datos_cargados())
    if(!is.null(input$filtroNum)){
      datos_filt <- datos_filt %>%
        filter(
          .data[[input$varsNum]] >= input$filtroNum[1] &
            .data[[input$varsNum]] <= input$filtroNum[2]
        )
    } 
    if(!is.null(input$filtroTxt)){
      datos_filt <- datos_filt %>%
        filter(
          .data[[input$varsTxt]] %in% input$filtroTxt
        )
    }
    return(datos_filt)
  })
  
  output$grafico <- renderPlotly({
    datos <- as.data.frame(datos_filtrados())
    validate(need(input$eje_x != "Ninguna", "Seleccione la variable a graficar en el eje X."))
    if(input$convertir){
      datos[, input$eje_x] <- as.character(datos[, input$eje_x])
    }
    if(is.numeric(datos[, input$eje_x]) & input$eje_y == "Ninguna"){
      g <- ggplot(datos, aes_string(x = input$eje_x))
      if(!is.null(input$filtroTxt)){
        g <- g + geom_density(aes_string(fill = input$varsTxt, alpha = 0.4))
      } else {
        g <- g + geom_density(color="darkblue", fill="lightblue")
      }
      tipo_grafico <<- "density"
    } else if(any(is.character(datos[, input$eje_x]) | 
                  is.factor(datos[, input$eje_x])) & 
              input$eje_y == "Ninguna"){
      if(!is.null(input$filtroTxt)){
        tab <- table(datos[, input$eje_x], datos[, input$varsTxt]) %>%
          as.data.frame()
        colnames(tab) <- c(input$eje_x, input$varsTxt, "Total")
        g <- ggplot(tab, aes_string(x=input$eje_x, y = "Total", fill = input$eje_x))+
          geom_bar(stat = "identity")+
          facet_wrap(as.formula(paste("~", input$varsTxt)))
      } else {
        tab <- table(datos[, input$eje_x]) %>%
          as.data.frame()
        colnames(tab) <- c(input$eje_x, "Total")
        g <- ggplot(tab, aes_string(x=input$eje_x, y = "Total"))+
          geom_bar(stat = "identity", aes_string(fill = input$eje_x))
      }
      tipo_grafico <<- "barras"
    } else if(is.numeric(datos[, input$eje_x]) & is.numeric(datos[, input$eje_y])){
      g <- ggplot(datos, aes_string(x = input$eje_x, y = input$eje_y))
      if(!is.null(input$filtroTxt)){
        g <- g + geom_point(aes_string(col = input$varsTxt))
      } else {
        g <- g + geom_point()
      }
      tipo_grafico <<- "puntos"
    } else if(any(is.character(datos[, input$eje_x]) | 
                  is.factor(datos[, input$eje_x])) & 
              is.numeric(datos[, input$eje_y])){
      g <- ggplot(datos, aes_string(x = input$eje_x, y = input$eje_y)) +
        geom_boxplot(aes_string(fill = input$eje_x))
      if(!is.null(input$filtroTxt)){
        g <- g + facet_wrap(as.formula(paste("~", input$varsTxt)))
      } 
      tipo_grafico <<- "cajas"
    } else {
      g <- NULL
    }
    return(ggplotly(g, source = "grafico"))
  })
  
  datos_filtrados_grafico <- reactive({
    datos <- datos_filtrados()
    sel_grafico <- NULL
    if(!is.null(tipo_grafico)){
      if(tipo_grafico == "barras"){
        sel_grafico <- event_data("plotly_click", source = "grafico")
      } else if(tipo_grafico == "puntos") {
        sel_grafico <- event_data("plotly_relayout", source = "grafico")
      } 
      if(tipo_grafico == "barras" & !is.null(sel_grafico)){
        if(!is.null(input$filtroTxt)){
          tab <- table(datos[, input$eje_x], datos[, input$varsTxt]) %>%
            as.data.frame()
          colnames(tab) <- c(input$eje_x, input$varsTxt, "Total")
          sel_cat1 <- tab[tab$Total == sel_grafico$y, input$eje_x]
          sel_cat2 <- tab[tab$Total == sel_grafico$y, input$varsTxt]
          if(length(sel_cat1) > 0 & length(sel_cat2) > 0) {
            datos <- datos %>%
              filter(
                .data[[input$eje_x]] == sel_cat1,
                .data[[input$varsTxt]] == sel_cat2
              )
          }
        } else {
          tab <- table(datos[, input$eje_x]) %>%
            as.data.frame()
          colnames(tab) <- c(input$eje_x, "Total")
          sel_cat <- tab[tab$Total == sel_grafico$y, input$eje_x]
          if(length(sel_cat) > 0) {
            datos <- datos %>%
              filter(
                .data[[input$eje_x]] == sel_cat
              )
          }
        }
      } else if(tipo_grafico == "puntos" & !is.null(sel_grafico)){
        if(!any(names(sel_grafico) %in% c("width", "height"))){
          datos <- datos %>%
            filter(
              .data[[input$eje_x]] >= sel_grafico$`xaxis.range[0]`,
              .data[[input$eje_x]] <= sel_grafico$`xaxis.range[1]`,
              .data[[input$eje_y]] >= sel_grafico$`yaxis.range[0]`,
              .data[[input$eje_y]] <= sel_grafico$`yaxis.range[1]`
            )
        }
      }
    }
    return(datos)
  })
  
  output$datos_crudos_2 <- renderDT({
    return(datos_filtrados_grafico())
  }, 
  filter = "top", 
  selection  = list(target = "column", mode = 'single'),
  options = list(scrollX = TRUE))
  
  info_cajas <- reactive({
    lista_res <- list(
      Nombre = NULL,
      Nombre_Medida1 = NULL,
      Medida1 = NULL,
      Nombre_Medida2 = NULL,
      Medida2 = NULL
    )
    #cells_selected, rows_selected, row_last_clicked
    if(!is.null(input$datos_crudos_2_columns_selected)){
      datos <- datos_filtrados_grafico()
      cols <- input$datos_crudos_2_columns_selected
      variable_sel <- datos[, cols]
      if(is.numeric(variable_sel)){
        lista_res <- list(
          Nombre = colnames(datos)[cols],
          Nombre_Medida1 = "Media",
          Medida1 = round(mean(variable_sel, na.rm = T), 1),
          Nombre_Medida2 = "Desv. Est.",
          Medida2 = round(sd(variable_sel, na.rm = T), 2)
        )
      } else if(is.character(variable_sel)){
        t <- table(variable_sel)
        props <- round((t/sum(t))*100, 1)
        p1 <- sort(props, decreasing = T)[1]
        lista_res <- list(
          Nombre = colnames(datos)[cols],
          Nombre_Medida1 = "Categorías",
          Medida1 = length(unique(variable_sel)),
          Nombre_Medida2 = names(p1),
          Medida2 = p1
        )
      }
    }
    return(lista_res)
  })
  
  output$var_info <- renderInfoBox({
    if(!is.null(info_cajas()$Nombre)){
      infoBox(
        "Variable",
        info_cajas()$Nombre
      )
    } else {
      infoBox(
        "Datos",
        ifelse(is.null(input$datos$name),
               "",
               input$datos$name)
      )
    }
  })
  
  output$val_box1 <- renderValueBox({
    if(!is.null(info_cajas()$Nombre)){
      valueBox(
        info_cajas()$Medida1,
        info_cajas()$Nombre_Medida1
      )
    } else {
      valueBox(
        ncol(datos_filtrados_grafico()),
        "Total Columnas"
      )
    }
  })
  
  output$val_box2 <- renderValueBox({
    if(!is.null(info_cajas()$Nombre)){
      valueBox(
        info_cajas()$Medida2,
        info_cajas()$Nombre_Medida2
      )
    } else {
      valueBox(
        nrow(datos_filtrados_grafico()),
        "Total Filas"
      )
    }
  })
  
  session$onSessionEnded(function() {
    stopApp()
  })
  
}
shinyApp(ui = ui, server = server)
