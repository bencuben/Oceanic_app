library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
library(shiny)
library(shinythemes)
library(gamlss)
library(shinyWidgets)
shinyUI(
  
  
  navbarPage(title = tags$b("Oceanic app"),id="navibar",
             tabPanel(title="Pruebas",icon = icon("chart-line","fa-1x"),
                      fluidPage(theme=shinytheme("cerulean"),
                        #shinythemes::themeSelector(),
                        setBackgroundImage(src = "oceano.jpg"),
                        
                        sidebarLayout(
                          sidebarPanel(width=4,
                            fluidRow(
                              column(7,img(src="escudo.png", height="100%", width="100%"))#,
                              #column(4,tags$p(tags$p(""),align="left"))
                            ),
                            
                            
                            fileInput("file1", "Carga tu base de datos",
                                      placeholder = "Datos...",
                                      buttonLabel = "Explorar...",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                      
                            ),
                            #Pregunta si los datos tienen encabezado o no
                            
                            tags$p("¿La base de datos tiene encabezado?"),
                            
                            checkboxInput("header", "Encabezado", TRUE),
                            
                            #Dos opciones en una sola fila
                            
                            fluidRow(
                              
                              #Pregunta el tipo de separador de los datos
                              column(6,selectInput("sep", "Separador",width = "100%",
                                                   choices = c("Coma" = ",",
                                                               "Punto coma" = ";",
                                                               "Tabular" = "\t"),
                                                   selected = ",")
                              ),
                              #Pregunta el tipo de decimal usado en los datos
                              column(5,selectInput("dec","Decimal",width = "100%", 
                                                   choices = c("Punto"=".",
                                                               "Coma"=","),
                                                   selected=",")
                              )
                              
                            )
                            
                          ),
                          
                          # Paneles de salida
                          mainPanel(
                            #Gráfica 1------------------------------------------------
                            dropdownButton(
                              
                              tags$h3("Opciones de la gráfica"),
                              tags$br(),
                              tags$p("Ingrese título del gráfico"),
                              textInput("titulo1",label=NULL),
                              tags$p("Ingrese el nombre del eje Y"),
                              textInput("ejey1",label=NULL),
                              tags$p("Ingrese el nombre del eje X"),
                              textInput("ejex1",label=NULL),
                              downloadButton('downloadPlot1', 'Download Plot'),
                              
                              circle = TRUE, status = "info",
                              icon = icon("gear"), width = "300px",
                              
                              tooltip = tooltipOptions(title = "Click para más!")
                            ),
                            
                            
                            plotOutput("plot1"),
                            
                            #Gráfica 2------------------------------------------------
                            dropdownButton(
                              
                              tags$h3("Opciones de la gráfica"),
                              tags$br(),
                              tags$p("Ingrese título del gráfico"),
                              textInput("titulo2",label=NULL),
                              tags$p("Ingrese el nombre del eje Y"),
                              textInput("ejey2",label=NULL),
                              tags$p("Ingrese el nombre del eje X"),
                              textInput("ejex2",label=NULL),
                              downloadButton('downloadPlot2', 'Download Plot'),
                              
                              circle = TRUE, status = "info",
                              icon = icon("gear"), width = "300px",
                              
                              tooltip = tooltipOptions(title = "Click para más!")
                            ),
                            plotOutput("plot2"),
                            
                            #Gráfica 3------------------------------------------------
                            dropdownButton(
                              
                              tags$h3("Opciones de la gráfica"),
                              tags$br(),
                              tags$p("Ingrese título del gráfico"),
                              textInput("titulo3",label=NULL),
                              tags$p("Ingrese el nombre del eje Y"),
                              textInput("ejey3",label=NULL),
                              tags$p("Ingrese el nombre del eje X"),
                              textInput("ejex3",label=NULL),
                              downloadButton('downloadPlot3', 'Download Plot'),
                              
                              circle = TRUE, status = "info",
                              icon = icon("gear"), width = "300px",
                              
                              tooltip = tooltipOptions(title = "Click para más!")
                            ),
                            plotOutput("plot3"),
                            #Gráfica 4------------------------------------------------
                            dropdownButton(
                              
                              tags$h3("Opciones de la gráfica"),
                              tags$br(),
                              tags$p("Ingrese título del gráfico"),
                              textInput("titulo4",label=NULL),
                              tags$p("Ingrese el nombre del eje Y"),
                              textInput("ejey4",label=NULL),
                              tags$p("Ingrese el nombre del eje X"),
                              textInput("ejex4",label=NULL),
                              downloadButton('downloadPlot4', 'Download Plot'),
                              
                              circle = TRUE, status = "info",
                              icon = icon("gear"), width = "300px",
                              
                              tooltip = tooltipOptions(title = "Click para más!")
                            ),
                            plotOutput("plot4")
                            
                              
                            
                            
                            
                            
                          )
                        )
                      )
                      
             ),#tab 1
             tabPanel(title="Información",icon = icon("info-circle","fa-1x"),
                      fluidRow(
                        column(10,
                               tags$p(tags$h1("Ficha Técnica"), tags$h3("Utilización de R para
gráfico de cajas"), "En las líneas iniciales
                                      se propone instalar los paquetes necesarios para realizar el análisis estadístico,
                                      en este caso, los paquetes son:", tags$li(tags$b(style="color:black",
                                                                                       "lubridate"), ": Para configurar o tratar las fechas."),
                                      tags$li(tags$b(style="color:black", "dplyr"), ": Manejar los datos (agruparlos,
                                              editarlos, análisis, entre otros)."), tags$li(tags$b(style="color:black", "ggplot"),
                                                                                            ": Herramienta utilizada para obtener gráficos más elegantes."),
                                      tags$li(tags$b(style="color:black", "ggplotAssist"), ": Es un asistente en la
                                              realización de las gráficas en ggplot."), "Para instalar los paquetes
                                      anteriormente mencionados se usa la función", tags$i(style="color:black",
                                                                                           "install.packages"),"; se procede a cargar los paquetes  en R (esto se debe hacer
                                      cada vez que se inicia R), la función es", tags$i(style="color:black", "library"),
                                      ", luego se procede a cargar la base de datos con la función",
                                      tags$i(style="color:black", "read.table")," con ciertos argumentos utilizados dentro
                                      de la lectura de los datos, que son:", tags$li(tags$b(style="color:black",
                                                                                            "file"),": Se nombra el archivo de la base de datos (dentro de comillas con su
                                                                                     correspondiente tipo de archivo."), tags$li(tags$b(style="color:black", "skip"),":
                                                                                                                                 Cantidad de líneas que se omiten del archivo para cargar la base de datos (esto sucede
                                                                                                                                 a que la base de datos contiene información de los datos, pero no para su análisis)."),
                                      tags$li(tags$b(style="color:black", "header"),": Es un booleano (FALSE o TRUE) es una
                                              pregunta para saber si las columnas dentro del archivo tienen nombre."),
                                      "Esta información debe ser guardada en una variable para ser utilizada en el futuro
                                      para ser usada para el análisis.", tags$br(), "Después se hace un  filtro a los datos
                                      (dejar solo las columnas que son útiles para el análisis, y eliminar filas que no
                                      contengan la información necesaria).", tags$br(), "Se usa la librería",
                                      tags$b(style="color:black", "lubridate"), "para trabajar la información con los meses
                                      y los años por lo que la función" , tags$i(style="color:black", "ymd"), " (cambia el
                                      formato de la columna en formato fecha),", tags$i(style="color:black", "month"),
                                      "(transforma la columna de formato fecha de días meses y años a solo fechas
                                      mensuales),", tags$i(style="color:black", "year"), " (transforma la columna de formato
                                      fecha de días meses y años a solo fechas anuales); por último, solo se dejan las
                                      columnas necesarias, es decir, la de los meses, años y la que se utiliza para el
                                      análisis.", tags$br(), "Luego con la librería", tags$b(style="color:black", "dplyr"),
                                      "se agrupan los datos ya sea para meses o años según se requiera y se le saca
                                      la media.", tags$br(), "Y para la última sección para generan los",
                                      tags$i(style="color:black", "boxplots")," (gráfico de cajas), tanto para los meses
                                      como para los años, donde la función", tags$b(style="color:black", "ggplot"), "(debe
                                      contener un orden jerárquico), por lo que primero se debe guardar:",tags$br(),
                                      tags$li(tags$b(style="color:black", "data"), ": Contiene los datos para los puntos
                                              donde se encuentre la media."), tags$li(tags$b(style="color:black", "aes"), ": Función
                                                                                      que debe contener la información para graficar los ejes."),
                                      tags$li(tags$b(style="color:black", "x"), ": Los datos que se colocan en el eje x."),
                                      tags$li(tags$b(style="color:black", "y"), ": Los datos que se colocan en el eje y."),
                                      "La función", tags$b(style="color:black", "geom_boxplot")," (graficar las
                                      cajas) de la misma forma que se grafican los puntos solo que dentro de la función",
                                      tags$i(style="color:black","aes"),"se debe indicar como debe estar agrupado los datos
                                      (año o meses), se concluye colocando ", tags$i(style="color:black", "geom_point"),
                                      "(los puntos que ubican las medias) y",tags$i(style="color:black", "geom_lines"),
                                      "(enlazar los puntos de las medias), hay otras funciones como",
                                      tags$i(style="color:black", "scale_y_continuous"),
                                      ",", tags$i(style="color:black", "scale_x_continuous"),",",tags$i(style="color:black",
                                                                                                        "labs"),",",tags$i(style="color:black", "theme")," (son utilizados para decorar las
                                      gráficas).", tags$br(), "Para la segunda parte en la realización de las gráficas
                                      funciona de igual manera que lo anteriormente mencionado, solo que esta vez se usa un
                                      paquete adicional que es",tags$b(style="color:black", "gamlss"),"(identificar o
                                      utilizar funciones utilizadas en el análisis estadístico).", tags$br(), "Se crea un",
                                      tags$i(style="color:black","data.frame"),"(tipo de almacenaje de datos en R), donde
                                      crea dos columnas, una llamada Probs (almacena “probabilidades” de 0 a 1 en una
                                      secuencia de 0.0001 con la función",tags$i(style="color:black", "seq"),") y Cuantil_P
                                      (almacena los cuantiles de los datos del archivo con la función quantile en una
                                      secuencia igual a Probs).", tags$br(), "Con la librería",tags$b(style="color:black",
                                                                                                      "gamlss"),"  con la función", tags$i(style="color:black", "fitDist"),"(se encarga de
                                      encontrar la función de distribución que mejor se ajuste a los datos, como existen
                                      muchas funciones de distribución y los datos son positivos se usa bajo las
                                      distribuciones positivas “realplus”).", tags$br(), "Se procede creando un histograma
                                      con la función",tags$i(style="color:black", "histDist"),"bajo la función de
                                      distribución obtenida con",tags$i(style="color:black", "fitDist"),".", tags$br(),
                                      "Luego de obtener el histograma se evalúa la familia bajo la función de distribución
                                      a la cual se aproxima mejor los datos. Con la función",tags$i(style="color:black",
                                                                                                    "eval"), "(evalúa  funciones de R descritas como expresiones), ",
                                      tags$i(style="color:black", "parse"), "(cambia la información de un texto a una
                                      expresión) y",tags$i(style="color:black", "paste0"),"(crea textos sin espacios).",
tags$li(tags$b(style="color:red", "Nota"),": Esto es útil para que el estudiante no
        tenga la necesidad de hacer el proceso paso a paso, si no que el programa lo haga
        automáticamente."), "Dentro de estas 3 funciones anteriormente mencionadas
el propósito es crear los cuantiles de la función de distribución que mejor se
aproxima a los datos.", tags$br(), "Por último se grafican los cuantiles de la
distribución tanto teóricos como de los datos para ver su comportamiento, todo este
proceso es similar bajo a lo anteriormente mencionado con respecto al gráfico de cajas,
solo que en este caso se crean 4 puntos que van a representar el 1%, 5%, 95% y 99% de
los datos, y las funciones ",tags$i(style="color:black", "geom_text"),"(grafica en
texto los valores donde se encuentran los cuantiles mencionados) y la función",
tags$i(style="color:black", "geom_lines"),"(crea las líneas de la distribuciones).")


                               
                               
                               )
                      )
                      
                      #,uiOutput("pdfview")
                      
             )#tab 2
  )#NAVBAR
  
  
)#UI
