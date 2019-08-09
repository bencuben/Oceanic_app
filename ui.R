library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
library(shiny)
library(shinythemes)
library(gamlss)
# Define UI for application that draws a histogram
shinyUI(
  
  
  navbarPage(title = tags$b("Oceanic app"),id="navibar",
             tabPanel(title="Pruebas",icon = icon("chart-line","fa-1x"),
                      fluidPage(theme=shinytheme("cerulean"),
                        
                        sidebarLayout(
                          sidebarPanel(width=4,
                            fluidRow(
                              column(7,img(src="escudo.png", height="100%", width="100%")),
                              column(4,tags$p(tags$p(""),align="left"))
                            ),
                            
                            
                            fileInput("file1", "Carga tu base de datos",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")#,
                                      #buttonLabel = "Explorar...",
                                      #placeholder = "Ningún archivo"
                            ),
                            #Pregunta si los datos tienen encabezado o no
                            
                            tags$p("¿La base de datos tiene encabezado?"),
                            
                            checkboxInput("header", "Encabezado", FALSE),
                            
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
                          
                          # Show a plot of the generated distribution
                          mainPanel(
                            fluidRow(
                              column(6,
                            plotOutput("plot1",height = "300px"),
                            plotOutput("plot4",height = "300px")
                              ),
                            column(6,
                            plotOutput("plot2",height = "300px"),
                            plotOutput("plot3",height = "300px")
                            )
                              
                            )
                            
                            
                            
                          )
                        )
                      )
                      
             ),#tab 1
             tabPanel(title="Información",icon = icon("info-circle","fa-1x"),
                      tags$p("asdasdasdasdasd"),
                      
                      uiOutput("pdfview")
                      
             )#tab 2
  )#NAVBAR
  
  
)#UI
