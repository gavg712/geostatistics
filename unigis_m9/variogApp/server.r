source("helpers.r")
options(shiny.maxRequestSize=30*1024^2)
shinyServer(function(input, output, session) {
        df = read.table("data/Elevacion.txt", header=TRUE,sep=",",)
require(geoR)
        #Construir el dataframe para representar en el variograma
        dt.variog <- data.frame(df[,1], df[,2], df[,3])
        distancias <- dist(dt.variog[,1:2])

# Grafico del semivariograma
    output$plot1 <- renderPlot({
        #Control de tipo de binneo
        vg <- if(input$userlags == FALSE){
                variog(coords=dt.variog[,1:2], data=dt.variog[,3], breaks="default")
                }else{
                    breaks =    seq(from = 0, 
                                l = input$nlag+1, 
                                by = max(distancias)/input$nlag)
                    variog(coords = dt.variog[,1:2], data = dt.variog[,3], 
                        option=c("bin"), breaks = breaks)
                }
        #Graficohttp://shiny.rstudio.com/reference/shiny/latest/tableOutput.html
        plot(   vg,type = "p", pch=20,
                main = paste("Variograma: Experimental + Modelo ", input$model), 
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
    
    output$report <- renderTable({
        #Control de tipo de binneo
        vg <- if(input$userlags == FALSE){
                variog(coords=dt.variog[,1:2], data=dt.variog[,3], breaks="default")
                }else{
                    breaks =    seq(from = 0, 
                                l = input$nlag+1, 
                                by = max(distancias)/input$nlag)
                    variog(coords = dt.variog[,1:2], data = dt.variog[,3], 
                        option=c("bin"), breaks = breaks)
                }
        data.frame("Distancia lag"=vg$u, "Semivarianza"=vg$v, "Pares"=vg$n)
        })
})
