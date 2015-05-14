library(shiny)
#source("helpers.r")
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output){
  df <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    df <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)    
  })
    output$dataOut <- renderTable({
      inFile <- input$datafile
      if (is.null(inFile))
        return(NULL)
      summary(df())
    })
  require(geoR)
# Grafico del semivariograma
   output$plot1 <- renderPlot({
     inFile <- input$datafile
     if (is.null(inFile))
       return(NULL)
     #Construir el dataframe para representar en el variograma
     db <- df()
     dt.variog <- data.frame(db[,input$colX], db[,input$colY], db[,input$colZ])
     distancias <- dist(dt.variog[,1:2])
     ##Control de tipo de binneo
     vg <- if(input$userlags == FALSE){
       variog(coords=dt.variog[,1:2], data=dt.variog[,3], breaks="default")
     }else{
       breaks =    seq(from = 0, 
                       l = input$nlag+1, 
                       by = max(distancias)/input$nlag)
       variog(coords = dt.variog[,1:2], data = dt.variog[,3], 
              option=c("bin"), breaks = breaks)
     }
     #Grafico http://shiny.rstudio.com/reference/shiny/latest/tableOutput.html
       plot(   vg,type = "p", pch=20,
               main = paste("Semivariograma: Experimental + Modelo ", input$model), 
               xlab="Distancia", ylab="Semivarianza")
       if (input$addmodel == TRUE){
       lines.variomodel(   cov.model=input$model, 
                           cov.pars=c(input$sill, input$range), 
                           nugget=input$nugget, 
                           max.dist = 100000, 
                           kappa=kappa,
                           lwd = 3)
       }
   })
#    
#    output$report <- renderTable({
#        #Control de tipo de binneo
#        vg <- if(input$userlags == FALSE){
#                variog(coords=dt.variog[,1:2], data=dt.variog[,3], breaks="default")
#                }else{
#                    breaks =    seq(from = 0, 
#                                l = input$nlag+1, 
#                                by = max(distancias)/input$nlag)
#                    variog(coords = dt.variog[,1:2], data = dt.variog[,3], 
#                        option=c("bin"), breaks = breaks)
#                }
#        data.frame("Distancia.lag"=vg$u, "Semivarianza"=vg$v, "Pares"=vg$n)
#        })
##interpolación


})
