library(shiny)
library(lifecontingencies)
library(tidyr)
library(readxl)

shinyUI(fluidPage(
    
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
                 
                 numericInput("imposiciones",
                              label = "Numero de imposiciones",
                              value = 480,
                              min = 0,
                              max = 600
                 ),
                 
                 numericInput("edad_inicio_1",
                              label = "Edad entrada mercado laboral ",
                              value = 23,
                              min = 14,
                              max = 100
                 ),
                 
                 numericInput("edad_jubilacion_1",
                              label = "Edad de jubilacion",
                              value = 61,
                              min = 50,
                              max = 100
                 ),
                 
                 numericInput("salario",
                              label = "Salario",
                              value = 0,
                              min = 0,
                              max = 100000
                 ),
                 
                 numericInput("tipo_cotizacion_1",
                              label = "Tipo de cotizacion",
                              value = 0.1046,
                              min = 0,
                              max = 1),
                 
                 numericInput("tanto_nocional_1",
                              label = "Tanto nocional",
                              value = 0.02,
                              min = 0,
                              max = 5),
                 
                 submitButton("Calcular"),
                 
                 
                 mainPanel(
                 
                 textOutput("cuantia_1_sis_act"),
                 
                 textOutput("cuantia_sis_nocional_sis_1")
                 
                 )
                 
                 ),
        
        tabPanel("Segundo caso",
                 
                 h4("Los datos de los salarios se extraen de una base de datos"),
                 
                 selectInput("genero",
                             label = "Genero",
                             choices = c("Femenino",
                                         "Masculino")
                 ),
                 
                 numericInput("edad_inicio",
                              label = "Edad entrada mercado laboral ",
                              value = 23,
                              min = 14,
                              max = 100
                 ),
                 
                 numericInput("edad_jubilacion",
                              label = "Edad de jubilacion",
                              value = 61,
                              min = 50,
                              max = 100
                 ),
                 
                 numericInput("tipo_cotizacion",
                              label = "Tipo de cotizacion",
                              value = 0.1046,
                              min = 0,
                              max = 1),
                 
                 numericInput("tanto_nocional",
                              label = "Tanto nocional",
                              value = 0.02,
                              min = 0,
                              max = 5),
                 
                 submitButton("Calcular"),
                 
                 mainPanel(
                 
                 textOutput("cuantia_2_sis_act"),
                 
                 textOutput("cuantia_sis_nocional_sis_2")
                    )
        )
))
)