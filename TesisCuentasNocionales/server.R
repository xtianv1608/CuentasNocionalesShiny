library(shiny)
library(readxl)
library(tidyverse)
library(lifecontingencies)
library(eeptools)
library(DT)

#source("TesisCuentasNocionales/global.R")

shinyServer(function(input, output) {
  
  output$edad_cuantia_minima <- renderText({
    parametros_minimos <- pension_edad_minima(input$genero_1, input$fecha_nacimiento_1,
                                              input$fecha_inicio_1, input$salario, 
                                              input$tanto_nocional_1)
    paste("Mediante el Sistema de Cuentas Nocionales, la edad minima de jubilacion es", 
          parametros_minimos[1], "para obtener un salario de", parametros_minimos[2])
  })

    output$plot_cuantias <- renderPlot({
        
        a <- salarios_2(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
                        input$edad_jubilacion_1)
        
       # a <- salarios_2(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 30)
        
        imposiciones <- as.numeric(a[,.N])
        
        SBU <- salario_basico(input$fecha_nacimiento_1, input$fecha_inicio_1, input$edad_jubilacion_1)
        
        if ((imposiciones >= 480)|
            (input$edad_jubilacion_1 >= 60 & imposiciones >= 360) |
            (input$edad_jubilacion_1 >= 65 & imposiciones >= 180) |
            (input$edad_jubilacion_1 >= 70 & imposiciones >= 120))
        {
        pension_calculada <- 0
        anios_jubilacion <- 0
        pension_calculada <- cuantia_sistema_actual(input$salario, input$fecha_nacimiento_1, 
                                                    input$fecha_inicio_1, input$edad_jubilacion_1)
        #ahorro_acumulado <- input$salario*input$imposiciones*input$tipo_cotizacion_1
        #anios_jubilacion <- inf_actuarial - input$edad_jubilacion_1
        
        #pension_calculada <- as.numeric(pension_calculada)
        
        coef_minimo <- minimo(floor(imposiciones/12))
        coef_maximo <- maximo(floor(imposiciones/12))
        
        ### Seccion cuentas nocionales #######
        #edad_inicio <- floor(age_calc(input$fecha_nacimiento_1, input$fecha_inicio_1, units = "years"))
        #sueldos <- salarios(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
         #                   input$edad_jubilacion_1)
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
    
    
    
#tabla_nocional("Femenino", as.Date("1995-07-01"), as.Date("2021-09-09"), 65, 400, 0.02)   


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
        a <- salarios_2(input$salario, input$fecha_nacimiento_1, input$fecha_inicio_1, 
                        i)
        
        # a <- salarios_2(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 30)
        
        salario_antes_jubilarse <- salario_al_jubilarse(a, input$fecha_nacimiento_1, i)
        
        imposiciones <- as.numeric(a[,.N])
        
        pension_calculada <- cuantia_sistema_actual(input$salario, input$fecha_nacimiento_1, 
                                                    input$fecha_inicio_1, i)
        coef_minimo <- minimo(floor(imposiciones/12))
        coef_maximo <- maximo(floor(imposiciones/12))
        
        fondo_nocional <- 0
        fondo_nocional <- capital_nocional(input$genero_1, input$fecha_nacimiento_1, 
                                           input$fecha_inicio_1, i,
                                           input$salario, input$tanto_nocional_1)
        
        SBU <- salario_basico(input$fecha_nacimiento_1, input$fecha_inicio_1, i)
        
        if(pension_calculada < SBU*coef_minimo){
          tabla <- rbind(tabla,
                         data.table(
                           Edad = i,
                           SalarioAntesDeJubilarse = salario_antes_jubilarse,
                           CuantiaSis.Actual = SBU*coef_minimo,
                           TasaSustitucionSis.Actual = round(SBU*coef_minimo/salario_antes_jubilarse, 2),
                           Cuantia.Sis.Cta.Nocional = fondo_nocional,
                           TasaSustitucionSis.Cta.Nocional = round(fondo_nocional/salario_antes_jubilarse, 
                                                                   2)
                         ))

        }
        
        else{
          
          if(pension_calculada > SBU*coef_maximo){
            tabla <- rbind(tabla,
                           data.table(
                             Edad = i,
                             SalarioAntesDeJubilarse = salario_antes_jubilarse,
                             CuantiaSis.Actual = SBU*coef_maximo,
                             TasaSustitucionSis.Actual = round(SBU*coef_maximo/salario_antes_jubilarse, 2),
                             Cuantia.Sis.Cta.Nocional = fondo_nocional,
                             TasaSustitucionSis.Cta.Nocional = round(fondo_nocional/
                                                                       salario_antes_jubilarse, 2)
                           ))

          }
          
          else{
            tabla <- rbind(tabla,
                           data.table(
                             Edad = i,
                             SalarioAntesDeJubilarse = salario_antes_jubilarse,
                             CuantiaSis.Actual = pension_calculada,
                             TasaSustitucionSis.Actual = round(pension_calculada/salario_antes_jubilarse, 
                                                               2),
                             Cuantia.Sis.Cta.Nocional = fondo_nocional,
                             TasaSustitucionSis.Cta.Nocional = round(fondo_nocional/
                                                                       salario_antes_jubilarse, 2)
                           ))
          }
          
        }
        
        
      }
      t(tabla)
    })
    
})