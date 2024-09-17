shinyUI(
fluidPage(
#     titlePanel(h1("Variografía en R",align = "center")),
    sidebarLayout(
        sidebarPanel(width = 3,
            fluidRow(       #InputFile desde aquí
                fileInput('datafile', "Cargar datos", multiple = FALSE, 
                        accept = c("text/csv","text/plain")),
                fluidRow(
                    column(8,
                        checkboxInput('header', 'Encabezado', TRUE),
                        column(5,   radioButtons('sep', 'Separador',
                                c(Comma=',', Semicolon=';', Tab='\t', Space=' '),',')),
                        column(7,   radioButtons('quote', 'Quote', c(None='',
                                'Double Quote'='"', 'Single Quote'="'"),'"'))
                    ),
                    column(4, p('Núm. columna:', align = "center"),
                        fluidRow(column(2,h6("X")),column(8,numericInput('colX', label = NULL, value = 1))),
                        fluidRow(column(2,h6("Y")),column(8,numericInput('colY', label = NULL, value = 2))),
                        fluidRow(column(2,h6("Z")),column(8,numericInput('colZ', label = NULL, value = 3)))
                    )
                )
            ), #InputFile hasta aquí
            fluidRow(h4("Semivariograma experimental"), #parametros smvg experimental desde
                column(4, checkboxInput('userlags', h6('Ajustar bines manualmente'), FALSE)),
                column(8, 
                       sliderInput("nlag", label = "Número de Lags", min = 1, max = 100, value = 10), 
                       numericInput("cutoff", label = h6("Distancia máxima [m]"), value = 20000))
                
            ), #parametros smvg experimental hasta
            fluidRow(h4("Modelo teórico"), #parametros smvg teorico desde
                column(4, 
                     checkboxInput('addmodel', h6('Agregar modelo teórico'), FALSE)
                ),
                column(8, 
                     selectInput("model", label = "Seleccione modelo",
                                 choices = as.list(as.character(gstat::vgm()$long)),
                                 selected = as.character(gstat::vgm()$long[2]))
                )
            ),
            fluidRow(
              checkboxInput('modelinits', h6('Ajustar automáticamente (valores de inicio)'), FALSE),
                column(6,numericInput("sill", label = h6("Sill"), value = 2000000)),
                column(6,numericInput("range", label = h6("Range"), value = 25000))
            ),
            fluidRow(
                column(6,numericInput("nugget", label = h6("Nugget"), value = 0)),
                column(6,numericInput("kappa", label = h6("Kappa"), value = 1))
            ) #parametros smvg teorico hasta
        ),
        mainPanel(h5(textOutput("text1"), align="center"),
                  fluidRow(
                  h5("Vista rápida de los datos cargados", align="center"), 
                  verbatimTextOutput("dataOut")),
            fluidRow(
                column(12,plotOutput("plot1"),
                       h5("Resumen del modelo", align="center"), 
                       tableOutput("dataModel")
                
            )
        )
    )
), 
helpText(align="center", 
  p("Esta aplicación está orientada al aprendizaje sobre variogramas en R. Los datos utilizados fueron proporcionados por Pablo Cabrera Barona. El código y datos se encuentran disponibles en ", 
    a("Github", href = "https://github.com/gavg712/geostatistics/tree/master/unigis_m9/variogApp")))
))
