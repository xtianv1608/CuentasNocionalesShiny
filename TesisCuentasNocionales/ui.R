library(shiny)
library(lifecontingencies)
library(tidyr)
library(readxl)

shinyUI(fluidPage(
    theme = bslib::bs_theme(bootswatch = "darkly"),
    
    titlePanel("Escuela Politecnica Nacional"),
    h3("Ingenieria Matematica"),
    h3("Tesis Cuentas Nocionales"),
    h3("Por: Cristian Velastegui & Evelin Chile"),
    br(),
    h4("Calculadora de pensiones"),
    
    tabsetPanel(
        tabPanel("Primer caso",
                 h4("Se supone que el individuo gana el mismo salario durante toda su vida laboral"),
                 
                 selectInput("genero_1",
                             label = "Genero",
                             choices = c("Femenino",
                                         "Masculino")
                 ),
                 
                 dateInput("fecha_nacimiento_1",
                           label = "Fecha de nacimiento",
                           value = today() - 360*25,
                           language = "es",
                           format = "dd/mm/yyyy"
                           ),
                 
                 
                 dateInput("fecha_inicio_1",
                           label = "Fecha entrada mercado laboral ",
                           value = today(),
                           min = today(),
                           language = "es",
                           format = "dd/mm/yyyy"
                           
                 ),
                 
                 numericInput("edad_jubilacion_1",
                              label = "Edad de jubilacion",
                              value = 61,
                              min = 50,
                              max = 100
                 ),
                 
                 numericInput("salario",
                              label = "Salario",
                              value = 400,
                              min = 0,
                              max = 100000
                 ),
                 
                 
                 sliderInput("tanto_nocional_1",
                              label = "Tanto nocional",
                              value = 0.01,
                              min = 0,
                              max = 0.1,
                             step = 0.005),
                 
                 submitButton("Calcular"),
                 
                 
                 mainPanel(
                 
                 plotOutput("plot_cuantias"),
                 textOutput("edad_cuantia_minima"),
                 dataTableOutput("table_comparacion_nocional")
                 
                 #textOutput("cuantia_sis_nocional_sis_1")
                 
                 )
        
                 
                 )
)
)
)