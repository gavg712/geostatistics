shinyUI(fluidPage(
    titlePanel(h1("Varigramas en R",align = "center")),
    sidebarLayout(
        sidebarPanel(p("Modeifique los valores para ajustar el resultado")
            tags$hr(),
            fluidRow(h4("Semivariograma experimental"),
                column(4, checkboxInput('userlags', h6('Ajustar bines manualmente'), FALSE)),
                column(8, sliderInput("nlag", label = "Número de Lags", min = 1,
                                max = 100, value = 10))
            ),
            tags$hr(),
            fluidRow(h4("Modelo teórico"),
                column(4, 
                     checkboxInput('addmodel', h6('Agregar modelo teórico'), FALSE)
                ),
                column(8, 
                     selectInput("model", label = "Seleccione modelo",
                                 choices = list("","matern", "exponential", "gaussian",
                                 "spherical", "circular", "cubic", "wave", "power",
                                 "powered.exponential", "cauchy", "gencauchy", 
                                 "gneiting", "gneiting.matern", "pure.nugget"),
                                 selected = "")
                )
            ),
            fluidRow(
                column(6,numericInput("sill", label = h6("Sill"), value = 2000000)),
                column(6,numericInput("range", label = h6("Range"), value = 25000))
            ),
            fluidRow(
                column(6,numericInput("nugget", label = h6("Nugget"), value = 0)),
                column(6,numericInput("kappa", label = h6("Kappa"), value = 1))
            )
        ),
        mainPanel(h5(textOutput("text1"), align="center"),
            fluidRow(
                column(7,plotOutput("plot1"),
                helpText(p("Esta aplicación está orientada al aprendizaje sobre variogramas en R. Los datos utilizados fueron proporcionados por Pablo Cabrera Barona. El código y datos se encuentran disponibles en ", a("Github", href = "https://github.com/gavg712/geostatistics/tree/master/unigis_m9/varioApp")))
                ),
                column(4,h5("Reporte", align="center"), tableOutput("report"))
            )
        )
    )
))
