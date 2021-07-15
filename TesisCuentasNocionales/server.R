library(shiny)
library(readxl)
library(tidyverse)
library(lifecontingencies)
library(eeptools)
library(DT)

shinyServer(function(input, output) {

    output$plot_cuantias <- renderPlot({
        
        a <- salarios_2(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
                        input$edad_jubilacion_1)
        
       # a <- salarios_2(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 30)
        
        imposiciones <- as.numeric(a[,.N])
        
        if ((imposiciones >= 480)|
            (input$edad_jubilacion_1 >= 60 & imposiciones >= 360) |
            (input$edad_jubilacion_1 >= 65 & imposiciones >= 180) |
            (input$edad_jubilacion_1 >= 70 & imposiciones >= 120))
        {
        pension_calculada <- 0
        anios_jubilacion <- 0
        pension_calculada <- cuantia_sistema_actual(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
                                                    input$edad_jubilacion_1)
        #ahorro_acumulado <- input$salario*input$imposiciones*input$tipo_cotizacion_1
        #anios_jubilacion <- inf_actuarial - input$edad_jubilacion_1
        
        #pension_calculada <- as.numeric(pension_calculada)
        
        coef_minimo <- minimo(floor(imposiciones/12))
        coef_maximo <- maximo(floor(imposiciones/12))
        
        ### Seccion cuentas nocionales #######
        edad_inicio <- floor(age_calc(input$fecha_nacimiento_1, input$fecha_inicio_1, units = "years"))
        sueldos <- salarios(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
                            input$edad_jubilacion_1)
        fondo_nocional <- capital_nocional(input$genero_1, input$fecha_nacimiento_1, 
                                           input$fecha_inicio_1, input$edad_jubilacion_1, 
                                           input$salario, input$tanto_nocional_1)
        ######################################
        
        if(pension_calculada < SBU*coef_minimo){
            actual <- SBU*coef_minimo
            nocional <- fondo_nocional
            y <- c(actual, nocional)
            grafico_actual_nocional(y)
        }
        else{
            
        if(pension_calculada > SBU*coef_maximo){
            actual <- SBU*coef_maximo
            nocional <- fondo_nocional
            y <- c(actual, nocional)
            grafico_actual_nocional(y)
        }
        
        else{
            actual <- pension_calculada
            nocional <- fondo_nocional
            y <- c(actual, nocional)
            grafico_actual_nocional(y)
        }

            }
        }
        else
        {
            mensaje <- "Usted no cumple con los requisitos minimos para jubilacion"
        }
            
    })
    
    
   # output$cuantia_1_nocionales <- renderText({
    #    edad_inicio <- floor(age_calc(input$fecha_nacimiento_1, input$fecha_inicio_1, units = "years"))
     #   sueldos <- salarios(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
      #                       input$edad_jubilacion_1)
       # fondo_nocional <- capital_nocional(input$genero_1, edad_inicio, input$edad_jubilacion_1, 
        #                                   sueldos, input$tanto_nocional_1)
        #paste(fondo_nocional)
    #})
    
    output$table_comparacion_nocional <- renderDataTable({
        #tabla_cuantias <- tabla_nocional(input$genero_1, input$fecha_nacimiento_1, 
         #                                input$fecha_inicio_1, input$edad_jubilacion_1, 
          #                               input$salario, input$edad_jubilacion_1)
        #tabla_cuantias%>% DT::datatable(
         #   selection = "single")
        
        aux1 <- input$edad_jubilacion_1 - 4
        aux2 <- input$edad_jubilacion_1 + 4
        
        tabla <- data.table()
        for (i in aux1:aux2) {
            fondo_nocional <- 0
            fondo_nocional <- capital_nocional(input$genero_1, input$fecha_nacimiento_1, 
                                               input$fecha_inicio_1, i,
                                               input$salario, input$tanto_nocional_1)
            
            tabla <- rbind(tabla,
                           data.table(
                               Anio = i,
                               Cuantia_Nocional = fondo_nocional
                           ))
        }
        
        tabla
    })
})
    
#tabla_nocional("Femenino", as.Date("1995-07-01"), as.Date("2021-09-09"), 65, 400, 0.02)   
