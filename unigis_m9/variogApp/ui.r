shinyUI(
fluidPage(
#     titlePanel(h1("Variografía en R",align = "center")),
    sidebarLayout(
        sidebarPanel(width = 4,
            fluidRow(       #InputFile desde aquí
                fileInput(  'datafile', "Cargar datos", multiple = FALSE, 
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
                column(3, checkboxInput('userlags', h6('Ajustar bines manualmente'), FALSE)),
                column(6, sliderInput("nlag", label = "Número de Lags", min = 1,
                      max = 100, value = 10))
            ), #parametros smvg experimental hasta
            fluidRow(h4("Modelo teórico"), #parametros smvg teorico desde
                column(4, 
                     checkboxInput('addmodel', h6('Agregar modelo teórico'), FALSE)
                ),
                column(6, 
                     selectInput("model", label = "Seleccione modelo",
                                 choices = list("matern", "exponential", "gaussian",
                                 "spherical", "circular", "cubic", "wave", "power",
                                 "powered.exponential", "cauchy", "gencauchy", 
                                 "gneiting", "gneiting.matern", "pure.nugget"),
                                 selected = "exponential")
                )
            ),
            fluidRow(
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
                column(8,plotOutput("plot1"),
                helpText(p("Esta aplicación está orientada al aprendizaje sobre variogramas en R. Los datos utilizados fueron proporcionados por Pablo Cabrera Barona. El código y datos se encuentran disponibles en ", a("Github", href = "https://github.com/gavg712/geostatistics/tree/master/unigis_m9/variogApp")))
                ),
                column(3,h5("Reporte", align="center"), tableOutput("dataOut"))
            )
        )
    )
))
