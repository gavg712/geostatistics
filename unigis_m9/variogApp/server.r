library(shiny)
library(units)
library(dplyr)
library(sf)
#source("helpers.r")

options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output){
  
  df <- reactive({
    inFile <- input$datafile
    if (is.null(inFile))
      return(NULL)
    df <- read.table(inFile$datapath, 
                     header=input$header, 
                     sep=input$sep, 
                     quote=input$quote)    
  })
    output$dataOut <- renderPrint({
      inFile <- input$datafile
      if (is.null(inFile))
        return("Aún no has cargado datos...")
      dplyr::glimpse(df())
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
     names(dt.variog) <- names(db)[c(input$colX,input$colY, input$colZ)]
     dt.variog <- st_as_sf(dt.variog, coords = names(db)[1:2], remove = FALSE, crs = 32717)
     distancias <- units::drop_units(st_distance(dt.variog))
     bbx <- st_bbox(dt.variog)
     diagonal <- units::drop_units(bbx %>% 
                                     st_as_sfc() %>%  
                                     st_cast("POINT") %>% 
                                     st_distance() %>% 
                                     max())
     cutoff.bkp <- cutoff <- diagonal/3
     width <- cutoff/15
     
     output$pointOut <- renderUI({
       div(HTML(paste0(
         'X-min: ', bbx[1], "<br/>",
         'Y-min: ', bbx[2], "<br/>",
         'X-max: ', bbx[3], "<br/>",
         'Y-max: ', bbx[4], "<br/>",
         'Número de puntos: ', nrow(dt.variog), "<br/>",
         "Distancia máxima: ", max(distancias), "<br/>", 
         "Diagonal de los datos: ", diagonal, "<br/>", 
         "Cutoff sugerido: ", cutoff.bkp, "<br/>", 
         "Cutoff actual: ", cutoff, "<br/>")))
     })
     
     ##Control de tipo de binneo
     if(input$userlags){
       cutoff <- input$cutoff
       width <- cutoff/input$nlag
     }
     
     # formula variogram
     form <- paste0(names(db)[3], "~1") %>% as.formula()
     
     # variograma y modelos
     vg <- variogram(form, dt.variog, width = width, cutoff = cutoff)
     mdls <- set_names(as.character(gstat::vgm()$short), as.character(gstat::vgm()$long))
     
     # Agregar modelo teórico
     if(input$addmodel){
       fit.vg <- vgm(psill = input$sill,
                     model = mdls[[input$model]], 
                     range = input$range, 
                     nugget = input$nugget,
                     kappa = input$kappa)
       if(input$modelinits){
         teo.vg <- vgm(psill = input$sill,
                       model = mdls[[input$model]], 
                       range = input$range, 
                       nugget = input$nugget,
                       kappa = input$kappa)
         fit.vg <- fit.variogram(vg, model = teo.vg)
       }
       
       output$dataModel <- renderTable({
         print(fit.vg)
       })
       
       plot(vg, model = fit.vg, 
            xlab = "Distancia [m]",
            ylab = "Semivarianza", 
            main = paste("Semivariograma: Experimental + Modelo ", input$model))
     } else {
       plot(vg, 
            main = "Semivariograma: Experimental", 
            xlab = "Distancia [m]",
            ylab = "Semivarianza")
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
