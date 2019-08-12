
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
library(shiny)
library(shinythemes)
library(gamlss)

#Open file####
#68 lineas inservibles 
datos <- read.table("www/AirSea_ST_0001.asc", skip = 68,header = T)
#Fix the base####
datos1 <- datos[datos$SSH != -99,c("YYYYMM","SSH")]
datos1$YYYYMM <- ymd(datos1$YYYYMM,truncated = 2)
datos1$Month <- month(datos1$YYYYMM)
datos1$Year <- year(datos1$YYYYMM)
datos1 <- datos1[,c("Year","Month","SSH")]
#Means Year and Month####
MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(SSH))
MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(SSH))


datos2 <- datos[datos$SSH != -99,c("SSH")]


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  

# Gráfico 1 ---------------------------------------------------------------

  
  plotInput1 <- reactive({
    titulo <- ifelse(input$titulo1=="","Ciclo Mensual",input$titulo1)
    ejex <- ifelse(input$ejex1=="","Mes",input$ejex1)
    ejey <- ifelse(input$ejey1=="","Altura",input$ejey1)
    
    
    ggplot(data = MM, aes(x=Month, y=medias)) + 
      geom_boxplot(data = datos1,aes(x=Month,y=SSH,group=Month), 
                   fill = "#aee7e8", outlier.color = "#24009c") + 
      scale_y_continuous(name = ejey) + 
      scale_x_continuous(name = ejex, breaks = 1:12 , labels = 
                           c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep",
                             "Oct","Nov","Dic")) +
      labs(tag = titulo) + 
      theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 20),
            plot.tag.position = "top")  +
      geom_point(color = "#c72c41")  + geom_line(color = "#c72c41")
    
  })
  
  
  output$plot1<- renderPlot({
    print(plotInput1())
  })
  
  
  
  output$downloadPlot1 <- downloadHandler(
    filename = input$titulo1,#function() { paste(input$titulo1, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput1(), device = "pdf")
    }
  )
  
  

# Gráfico 2 ---------------------------------------------------------------

  Quant <- data.frame(Probs=seq(0,1,0.0001),
                      Cuantil_P=quantile(datos2,probs = seq(0,1,0.0001)))
  
  Ajuste <- fitDist(y = datos2, type = "realplus",trace=FALSE)
  
  #output$plot2 <- renderPlot({
    
  plotInput2 <- reactive({
    
    #Data analysis####
    
    
    
    
    #eval = Evalua expresiones
    #parse = convierte textos en expresiones sin evaluar
    #paste0 = concatena textos sin espacios
    Quant$Cuantil_T <- eval(parse(text = paste0("q",Ajuste$family[1],
                                                "(p=seq(0,1,0.0001),mu=",
                                                Ajuste$mu,
                                                ",sigma=",Ajuste$sigma,")")))
    #Graphic####
    d_0.01 <- round(Quant$Cuantil_P[101],2)
    d_0.05 <- round(Quant$Cuantil_P[501],2)
    d_0.95 <- round(Quant$Cuantil_P[9501],2)
    d_0.99 <- round(Quant$Cuantil_P[9901],2)
    
    titulo2 <- ifelse(input$titulo2=="",paste("Análisis de extremos: ",Ajuste$family[2]),input$titulo2)
    ejex2 <- ifelse(input$ejex2=="","Datos",input$ejex2)
    ejey2 <- ifelse(input$ejey2=="","Probabilidad acumulada",input$ejey2)
    
    ggplot(data = Quant) +
      geom_line(aes(x=Cuantil_P,y=Probs), color = "#FF9770", size=1) +
      geom_line(aes(x=Cuantil_T,y=Probs), color = "#70D6FF", size=1) +
      scale_x_continuous(name = ejex2,breaks = seq(0,ceiling(max(datos2)),5)) +
      scale_y_continuous(name = ejey2) +
      labs(tag = titulo2) +
      theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 15),
            plot.tag.position = "top") +
      scale_fill_manual(values = c("#FF9770","#70D6FF")) +
      geom_text(aes(x=d_0.01,y=0.01),label=d_0.01)+
      geom_text(aes(x=d_0.05,y=0.05),label=d_0.05)+
      geom_text(aes(x=d_0.95,y=0.95),label=d_0.95)+
      geom_text(aes(x=d_0.99,y=0.99),label=d_0.99)

    
    
  #})
    #plot(datos2)
    
  })
  
  
  output$plot2<- renderPlot({
    print(plotInput2())
  })
  
  
  
  output$downloadPlot2 <- downloadHandler(
    filename = input$titulo2,#function() { paste(input$titulo1, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput2(), device = "pdf")
    }
  )
  
# Gráfico 3 ---------------------------------------------------------------
  
  plotInput3 <- reactive({
  #output$plot3<- renderPlot({
  titulo3 <- ifelse(input$titulo3=="",paste("Mejor distribución: :",Ajuste$family[2]),input$titulo3)
  ejex3 <- ifelse(input$ejex3=="","Datos",input$ejex3)
  ejey3 <- ifelse(input$ejey3=="","Densidad",input$ejey3)

  histDist(datos2, family=as.name(Ajuste$family[1]), col.hist = "white",
           col.main = "Black", line.col = "blue",border.hist = "black",
           main= titulo3,
           xlab=ejex3,
           ylab=ejey3,
           col.axis = "black", col.lab = "black")
    
  })  
  
  output$plot3<- renderPlot({
    print(plotInput3())
  })
  
  
  
  output$downloadPlot3 <- downloadHandler(
    filename = input$titulo3,#function() { paste(input$titulo1, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput3(), device = "pdf")
    }
  )

# Gráfico 4 ---------------------------------------------------------------

  
  plotInput4 <- reactive({
  #output$plot4<- renderPlot({
    
    #Means Year and Month####
    MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(SSH))
    MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(SSH))
    
    titulo4 <- ifelse(input$titulo4=="","Ciclo anual",input$titulo4)
    ejex4 <- ifelse(input$ejex4=="","Año",input$ejex4)
    ejey4 <- ifelse(input$ejey4=="","Altura",input$ejey4)
    
    ggplot(data = MY, aes(x=Year, y=medias)) + 
      geom_boxplot(data = datos1,aes(x=Year,y=SSH,group=Year), 
                   fill = "#aee7e8", outlier.color = "#24009c") +
      scale_y_continuous(name = ejey4) + 
      scale_x_continuous(name = ejex4, breaks = seq(min(MY$Year),max(MY$Year),1)) + 
      labs(tag = titulo4) + 
      theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 20),
            plot.tag.position = "top")  +
      geom_point(color = "#c72c41") + geom_line(color = "#c72c41")
    
  })
  
  output$plot4<- renderPlot({
    print(plotInput4())
  })
  
  
  
  output$downloadPlot4 <- downloadHandler(
    filename = input$titulo4,#function() { paste(input$titulo1, '.pdf', sep='') },
    content = function(file) {
      ggsave(file, plot = plotInput4(), device = "pdf")
    }
  )
  
  # output$pdfview <- renderUI({
  #   
  #   tags$iframe(width = "560", height = "315",url_link = "https://www.youtube.com/embed/0fKg7e37bQE")
  # })
  
})
