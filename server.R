#Librerias usadas

library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
library(shiny)
library(shinythemes)
library(gamlss)

# #Open file####
# #68 lineas inservibles 
# datos <- read.table("www/AirSea_ST_0001.asc", skip = 68,header = T)
# #Fix the base####
# datos1 <- datos[datos$SSH != -99,c("YYYYMM","SSH")]
# datos1$YYYYMM <- ymd(datos1$YYYYMM,truncated = 2)
# datos1$Month <- month(datos1$YYYYMM)
# datos1$Year <- year(datos1$YYYYMM)
# datos1 <- datos1[,c("Year","Month","SSH")]
# #Means Year and Month####
# MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(SSH))
# MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(SSH))
# 
# 
# datos2 <- datos[datos$SSH != -99,c("SSH")]








# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  
  # Datos -------------------------------------------------------------------
  
  #Definición de la lectura de datos reactiva
  dataset <- reactive({
    #Pregunta si la base ha sido cargada para poder continuar
    
    req(input$file1)
    #Lectura de la base en txt
    datos<-read.table(input$file1$datapath,
                      header = input$header)#,
    #sep = input$sep,dec = input$dec)
    
    datos
    
  })
  
  
  reactive({
    if(input$accion==TRUE){
      datos<-dataset()
      #datos1 <- datos[datos[,input$ycol] != -99,c(input$xcol,input$ycol)]
      #datos1$YYYYMM <- ymd(datos1[,input$xcol],truncated = 2)
      
      #datos1$Month <- month(datos1[,input$xcol])
      #datos1$Year <- year(datos1[,input$xcol])
      #datos1$Y<-datos1[,input$ycol]
      #datos1 <- datos1[,c("Year","Month","Y")]
      #Means Year and Month####
      #MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(Y) )
      #MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(Y))
      
      
      #datos2 <- datos[datos[,input$ycol] != -99,input$ycol]
      
      #Quant <- data.frame(Probs=seq(0,1,0.0001),
      #                    Cuantil_P=quantile(datos2,probs = seq(0,1,0.0001)))
      
      #Ajuste <- fitDist(y = datos2, type = "realplus",trace=FALSE)
      
    }
    
  })
  
  
  output$contents <- renderTable({
    
    head(dataset())[,1:9] 
  })
  
  
  #Actualiza los menus desplegables
  observeEvent(dataset(), {
    updateSelectInput(session, "xcol", choices=colnames(dataset()))
    updateSelectInput(session, "ycol", choices=colnames(dataset()))
  })
  
  
  
  
  
  #Número de columnas de los datos, la cual servira para los paneles condicionales
  output$datos <- reactive({
    if(is.null(input$file1)){
      TRUE
    }else{
      FALSE
    }
    #is.null(dataset())
    
  })
  
  
  #Argumento para poder usar los outputs en las condiciones de los paneles
  outputOptions(output, "datos", suspendWhenHidden = FALSE) 
  
  
  
  
  # Gráfico 1 ---------------------------------------------------------------
  
  
  plotInput1 <- reactive({
    req(input$file1)
    if(input$accion==TRUE){
      withProgress( message = "Ajustando Ciclo mensual",value=0,{
        
        datos<-dataset()
        datos1 <- datos[datos[,input$ycol] != -99,c(input$xcol,input$ycol)]
        datos1$YYYYMM <- ymd(datos1[,input$xcol],truncated = 2)
        
        titulo <- ifelse(input$titulo1=="","Annual cycle",input$titulo1)
        ejex <- ifelse(input$ejex1=="",names(datos1)[1],input$ejex1)
        ejey <- ifelse(input$ejey1=="",names(datos1)[2],input$ejey1)
        
        datos1$Month <- month(datos1[,input$xcol])
        datos1$Year <- year(datos1[,input$xcol])
        datos1$Y<-datos1[,input$ycol]
        datos1 <- datos1[,c("Year","Month","Y")]
        #Means Year and Month####
        MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(Y) )
        MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(Y))
        
        ggplot(data = MM, aes(x=Month, y=medias)) + theme_minimal() +
          geom_boxplot(data = datos1,aes(x=Month,y=Y,group=Month), 
                       fill = "#aee7e8", outlier.color = "#24009c") + 
          scale_y_continuous(name = ejey) + 
          scale_x_continuous(name = ejex, breaks = 1:12 , labels = 
                               c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep",
                                 "Oct","Nov","Dec")) +
          labs(tag = titulo) + 
          theme(plot.tag = element_text(lineheight = 4,face = "bold",size = 20, hjust = 0.5),
                plot.tag.position = "top", axis.text.x = element_text(angle = 90))  +
          geom_point(color = "#c72c41")  + geom_line(color = "#c72c41")
      })
    }
    
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
  
  
  
  
  
  plotInput2 <- reactive({
    req(input$file1)
    
    if(input$accion==TRUE){
      withProgress( message = "Realizando análisis de extremos",value=0,{
        datos<-dataset()
        datos2 <- datos[datos[,input$ycol] != -99,input$ycol]
        datos2 <- as.data.frame(datos2)
        Quant <- data.frame(Probs=seq(0,1,0.0001),
                            Cuantil_P=quantile(datos2$datos2,probs = seq(0,1,0.0001)))
        tipo <- ifelse(min(datos2$datos2)<0,"realAll","realplus")
        Ajuste <- fitDist(y = datos2$datos2, type = tipo,trace=FALSE)
        
        #eval = Evalua expresiones
        #parse = convierte textos en expresiones sin evaluar
        #paste0 = concatena textos sin espacios
        Quant$Cuantil_T <- eval(parse(text = paste0("q",Ajuste$family[1],
                                                    "(p=seq(0,1,0.0001),mu=",
                                                    Ajuste$mu,
                                                    ",sigma=",Ajuste$sigma,")")))
        
        cuantiles <- round(quantile(Quant$Cuantil_P,c(0.01,0.05,0.95,0.99)),2)
        
        titulo2 <- ifelse(input$titulo2=="",paste("Analysis of extremes: ",Ajuste$family[2]),input$titulo2)
        ejex2 <- ifelse(input$ejex2=="","Response",input$ejex2)
        ejey2 <- ifelse(input$ejey2=="","Cumulative probability",input$ejey2)
        
        ggplot(data = Quant) +  theme_minimal() +
          geom_line(aes(x = Cuantil_P, y = Probs), color = "#FF9770", size = 1.3) +
          geom_line(aes(x = Cuantil_T, y = Probs), color = "#70D6FF", size = 1.3) + 
          scale_x_continuous(name = ejex2,breaks = seq(0,ceiling(max(datos2$datos2)),5)) +
          scale_y_continuous(name = ejey2) +
          labs(tag = titulo2) +
          theme(plot.tag = element_text(lineheight = 4,face = "bold",size = 15, hjust = 0.5),
                plot.tag.position = "top", axis.text.x = element_text(angle = 90)) +
          scale_fill_manual(values = c("#FF9770","#70D6FF")) +
          geom_line(aes(x = Cuantil_T, y = 0.01), color = "#99b19c", size = 1) +
          geom_line(aes(x = Cuantil_T, y = 0.05), color = "#99b19c", size = 1) +
          geom_line(aes(x = Cuantil_T, y = 0.95), color = "#99b19c", size = 1) +
          geom_line(aes(x = Cuantil_T, y = 0.99), color = "#99b19c", size = 1) +
          geom_text(aes(x = cuantiles[1], y = 0.01), label = cuantiles[1]) +
          geom_text(aes(x = cuantiles[2], y = 0.05), label = cuantiles[2]) +
          geom_text(aes(x = cuantiles[3], y = 0.95), label = cuantiles[3]) +
          geom_text(aes(x = cuantiles[4], y = 0.99), label = cuantiles[4]) +
          geom_text(aes(x = 0, y = 0.01), label = "1%") +
          geom_text(aes(x = 0, y = 0.05), label = "5%") +
          geom_text(aes(x = 0, y = 0.95), label = "95%") +
          geom_text(aes(x = 0, y = 0.99), label = "99%")
        
      })#with progress
      
    }
    
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
    req(input$file1)
    if(input$accion==TRUE){
      
      withProgress( message = "Ajustando la mejor distribución",value=0,{
        
        
        datos<-dataset()
        datos2 <- datos[datos[,input$ycol] != -99,input$ycol]
        datos2 <- as.data.frame(datos2)
        tipo <- ifelse(min(datos2$datos2)<0,"realAll","realplus")
        Ajuste <- fitDist(y = datos2$datos2, type = tipo,trace=FALSE)
        datos2$densidad <- eval(parse(text = paste0("d",Ajuste$family[1],"(x = datos2$datos2, mu = ",
                                                    Ajuste$mu,", sigma = ",Ajuste$sigma,")")))
        
        
        titulo3 <- ifelse(input$titulo3=="",paste("Best distribution :",Ajuste$family[2]),input$titulo3)
        ejex3 <- ifelse(input$ejex3=="","Response",input$ejex3)
        ejey3 <- ifelse(input$ejey3=="","Density",input$ejey3)
        
        ggplot(data = datos2,aes(x = datos2)) + theme_minimal() +
          geom_histogram(aes(y = ..density..),binwidth=density(datos2$datos)$bw, fill = "#70D6FF",
                         color = "#000000") + 
          geom_line(aes(y = densidad), colour = "#FF9770", size = 1.3) + 
          scale_x_continuous(name = ejex3) + scale_y_continuous(name = ejey3) +
          labs(tag = titulo3) +
          theme(plot.tag = element_text(lineheight = 4,face = "bold",size = 15, hjust = 0.5),
                plot.tag.position = "top", axis.text.x = element_text(angle = 90))
        
      })#wihtprofgres
      
    }
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
    req(input$file1)
    if(input$accion==TRUE){
      
      withProgress( message = "Ajustando ciclo anual",value=0,{
        datos<-dataset()
        datos1 <- datos[datos[,input$ycol] != -99,c(input$xcol,input$ycol)]
        datos1$YYYYMM <- ymd(datos1[,input$xcol],truncated = 2)
        
        titulo4 <- ifelse(input$titulo4=="","Annual averages",input$titulo4)
        ejex4 <- ifelse(input$ejex4=="",names(datos1)[1],input$ejex4)
        ejey4 <- ifelse(input$ejey4=="",names(datos1)[2],input$ejey4)
        
        datos1$Month <- month(datos1[,input$xcol])
        datos1$Year <- year(datos1[,input$xcol])
        datos1$Y<-datos1[,input$ycol]
        datos1 <- datos1[,c("Year","Month","Y")]
        #Means Year and Month####
        MY <- datos1 %>% group_by(Year) %>% summarise(medias = mean(Y) )
        MM <- datos1 %>% group_by(Month) %>% summarise(medias = mean(Y))
        
        
        ggplot(data = MY, aes(x=Year, y=medias)) + theme_minimal() +
          geom_boxplot(data = datos1,aes(x=Year,y=Y,group=Year), 
                       fill = "#aee7e8", outlier.color = "#24009c") +
          scale_y_continuous(name = ejey4) + 
          scale_x_continuous(name = ejex4, breaks = seq(min(MY$Year),max(MY$Year),1)) + 
          labs(tag = titulo4) + 
          theme(plot.tag = element_text(lineheight = 4,face = "bold",size = 20, hjust = 0.5),
                plot.tag.position = "top", axis.text.x = element_text(angle = 90))  +
          geom_point(color = "#c72c41") + geom_line(color = "#c72c41")
        
      })
      
    }
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
