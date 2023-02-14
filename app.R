# Importar librerias
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(flexdashboard)) install.packages("flexdashboard", repos = "http://cran.us.r-project.org")


load('fit_rf_balanced.Rda') # EL MODELO DEBE ESTAR EN LA CARPETA
css <- HTML("
.html-widget.gauge svg {
  height: 400px;
  width: 800px;
}")

# Interface del usuario ####
ui <- fluidPage(theme = shinytheme("flatly"),
                
                
                # header
                headerPanel('Credit Scoring App'),
                
                # Input values
                sidebarPanel(
                    
                    tags$label(h3('Variables')),
                    
                    
                    selectInput("Empleado",
                                label = "El deudor está empleado?",
                                choices = list('No' = 0,
                                               'Si'= 1),
                                selected = 'Si'),
                    
                    numericInput("Tiempo_empleo", 
                                 label = "Cantidad de años empleado en el puesto actual?", 
                                 value = 4,
                                 step = 1),
                    
                    numericInput("Saldo_cuenta", 
                                 label = "Saldo en la cuenta del empleado", 
                                 value = 145),
                    
                    numericInput("Valor_prestamo", 
                                 label = "Valor del préstamo", 
                                 value = 2600),
                    
                    numericInput("Cuentas_otros", 
                                 label = "Cantidad de extensiones de la cuenta", 
                                 value = 1,
                                 step = 1),
                    
                    numericInput("Autocontrol", 
                                 label = "Autocontrol del deudor", 
                                 value = 50,
                                 step = 1),
                    
                    numericInput("Impulsividad", 
                                 label = "Impulsividad del deudor", 
                                 value = 50,
                                 step = 1),
                    
                    numericInput("Confianza", 
                                 label = "Confianza en el deudor", 
                                 value = 50,
                                 step = 1),
                    
                    actionButton("submitbutton", "Submit", 
                                 class = "btn btn-primary")
                    #plotOutput("plot2")
                    
                ),
                
                mainPanel(
                    
                    tags$label(h3('Estado de la aplicación')), # Output Text Box
                    verbatimTextOutput('contents'),
                    tableOutput('tabledata'), # Tabla de predicciones
                    gaugeOutput("gauge"),
                    
                    tags$head(tags$style(css)),
                    fluidRow(column(
                        width =  12,
                        gaugeOutput('Scale')
                    )
                    )
                    
                )
)

# Server ####
server<- function(input, output, session) {
    
    # Input Data
    datasetInput <- reactive({  
        
        df <- data.frame(
            "Empleado" = input$Empleado,
            "Tiempo_empleo" = input$Tiempo_empleo,
            "Saldo_cuenta" = input$Saldo_cuenta,
            "Valor_prestamo" = input$Valor_prestamo,
            "Cuentas_otros" = input$Cuentas_otros,
            "Autocontrol" = input$Autocontrol,
            "Impulsividad" = input$Impulsividad,
            "Confianza" = input$Confianza)
        
        input <- df
        write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = T)
        
        test <- read.csv(paste("input", ".csv", sep=""), header = T)
        test$Empleado <- factor(test$Empleado)
        
        prediccion <- predict(fit_rf_balanced,test)
        levels(prediccion) <- c('No default', 'Default')
        
        output$gauge = renderGauge({
            gauge(predict(fit_rf_balanced,test, type = 'prob')$`1`*100, 
                  min = 0, 
                  max = 100, 
                  sectors = gaugeSectors(success = c(0, 30), 
                                         warning = c(30, 50),
                                         danger = c(50, 100),
                                         colors = c("success", "warning", "danger" = 'red')), symbol = '%', label = paste("Peligro de default"))
        })
        
        Output <- data.frame(Predicción = prediccion, `Probabilidad de Default` = paste0(round(predict(fit_rf_balanced,test, type = 'prob')$`1`,3)), check.names = FALSE)
        
        
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton>0) { 
            isolate(cat("Predicción completada"))
        } else {
            return(cat("La app está lista para predecir"))
        }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton>0) { 
            isolate(datasetInput()) 
        } 
    })
    session$allowReconnect(TRUE)
    
    # output$plot2<-renderPlot({
    #     ggplot(varImp(fit_rf_balanced))+theme_minimal()},height = 400,width = 600)
    # # errores
    
}

# shinyApp(ui = ui, server = server)
runApp(list(ui = ui, server = server), launch.browser = TRUE)

