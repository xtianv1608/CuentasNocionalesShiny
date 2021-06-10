library(shiny)
library(readxl)
library(tidyverse)
library(lifecontingencies)

shinyServer(function(input, output) {

    output$cuantia_1_sis_act <- renderText({
        
        if ((input$imposiciones >= 480)|
            (input$edad_jubilacion >= 60 & input$imposiciones >= 360) |
            (input$edad_jubilacion >= 65 & input$imposiciones >= 180) |
            (input$edad_jubilacion >= 70 & input$imposiciones >= 10))
        {
        
        coeficiente <- coeficiente(input$imposiciones/12)
        ahorro_acumulado <- 0
        pension_calculada <- 0
        anios_jubilacion <- 0
        pension_calculada <- input$salario*coeficiente
        ahorro_acumulado <- input$salario*input$imposiciones*input$tipo_cotizacion_1
        anios_jubilacion <- inf_actuarial - input$edad_jubilacion_1
        
        coef_minimo <- minimo(input$imposiciones/12)
        coef_maximo <- maximo(input$imposiciones/12)
        
        
        if(pension_calculada < SBU*coef_minimo)
            paste0("Usted aporto al Seguro IVM ", ahorro_acumulado, " dolares, durante su permanecia
                   en el mundo laboral.", " \n Su pension es de ", SBU*coef_minimo, " dolares.",
                   "\n Y usted recibira aproximadamente", anios_jubilacion*(SBU*coef_minimo*13 + SBU), 
                   "\n\n dolares durante su periodo de jubilacion.")
        else{
        if(pension_calculada > SBU*coef_maximo)
            paste0("Usted aporto al Seguro IVM ", ahorro_acumulado, " dolares, durante su permanecia
                   en el mundo laboral.", " \n Su pension es de ", SBU*coef_maximo, " dolares.",
                   "\n Y usted recibira aproximadamente", anios_jubilacion*(SBU*coef_maximo*13 + SBU), 
                   "\n\n dolares durante su periodo de jubilacion.")
        else
            paste0("Usted aporto al Seguro IVM ", ahorro_acumulado, " dolares, durante su permanecia
                   en el mundo laboral.", " \n Su pension es de ", pension_calculada, " dolares.",
                   "\n Y usted recibira aproximadamente", anios_jubilacion*(pension_calculada*13 + SBU), 
                   "\n\n dolares durante su periodo de jubilacion")
            }
        }
        else
        {
            mensaje <- "Usted no cumple con los requisitos minimos para jubilacion"
        }
            
    })
    
    
    output$cuantia_sis_nocional_sis_1 <- renderText({
        valor_pension <- 0
        cuantia <- 0
        cuantia <-  cuantia_cuentas_nocionales_sis_1 (input$edad_inicio_1, input$edad_jubilacion_1, 
                                                      input$salario, input$tipo_cotizacion_1, 
                                                      input$tanto_nocional_1)
        q_x <- 0
        if(input$genero_1 == "Femenino"){
            valor_pension <- cuantia/axn(tabla_femenino, x = input$edad_jubilacion_1, 
                                         i = 0.04, k = 13, payment = "due", m = 0, n = 1)
            paste0("La cuantia bajo Cuentas Nocionales es de ", round(valor_pension, 2), " dolares")
        }
        else{
            valor_pension <- cuantia/axn(tabla_masculino, x = input$edad_jubilacion_1, 
                                         i = 0.04, k = 13, payment = "due", m = 0, n = 1)
            paste0("La cuantia bajo Cuentas Nocionales es de ", round(valor_pension, 2), " dolares")
        }
    })
    
    output$cuantia_2_sis_act <- renderText({
        anios <- input$edad_jubilacion - input$edad_inicio
        if ( (anios >= 40) | 
             (anios >= 30 & input$edad_jubilacion >= 60) | 
             (anios >= 15 & input$edad_jubilacion >= 65) | 
             (anios >= 10 & input$edad_jubilacion >= 70))
        {
            ahorro_acumulado <- 0
            ahorro_acumulado <- ahorro_acumulado_con_data(input$edad_inicio, input$edad_jubilacion,
                                                          input$tipo_cotizacion)
            
            coeficiente <- coeficiente(anios)
            salario_base <- salario_base_con_data(input$edad_inicio, input$edad_jubilacion)
            
            pension_calculada <- 0
            pension_calculada <- round(salario_base*coeficiente, 2)
            
            anios_jubilacion <- inf_actuarial - input$edad_jubilacion
            
            coef_minimo <- minimo(anios)
            coef_maximo <- maximo(anios)
            
            if(pension_calculada < SBU*coef_minimo)
                paste0("Usted aporto al Seguro IVM ", round(ahorro_acumulado, 2), " dolares, 
                    durante su pertenencia en el mundo laboral.", "\n Su pension es de ", SBU*coef_minimo,
                    " dolares. ", "\n Y usted recibira ", anios_jubilacion*(SBU*coef_minimo*13 + SBU), 
                    " dolares, durante su periodo de jubilacion.")
            else{
                if(pension_calculada > SBU*coef_maximo)
                    paste0("Usted aporto al Seguro IVM ", round(ahorro_acumulado, 2), " dolares,
                           durante su pertenencia en el mundo laboral.", "\n Su pension es de ", 
                           SBU*coef_maximo, " dolares. ", "\n Y usted recibira ", 
                           anios_jubilacion*(SBU*coef_maximo*13 + SBU), " dolares, durante su periodo 
                           de jubilacion.")
                else
                    paste0("Usted aporto al Seguro IVM ", round(ahorro_acumulado, 2), " dolares, 
                    durante su pertenencia en el mundo laboral.", "\n Su pension es de ", 
                    pension_calculada, " dolares. ", "\n Y usted recibira ", 
                    anios_jubilacion*(pension_calculada*13 + SBU), " dolares, durante su periodo de 
                    jubilacion.")
            }
        }
        
        else
            mensaje <- "Para cuantia 2 usted no cumple con los requisitos minimos para jubilacion"
        
    })
    
    output$cuantia_sis_nocional_sis_2 <- renderText({
        valor_pension <- 0
        cuantia <- 0
        cuantia <-  cuantia_cuentas_nocionales(input$edad_inicio, input$edad_jubilacion, 
                                               input$tipo_cotizacion, input$tanto_nocional)
        q_x <- 0
        if(input$genero == "Femenino"){
            valor_pension <- cuantia/axn(tabla_femenino, x = input$edad_jubilacion, 
                                         i = 0.04, k = 12, payment = "due", m = 0, n = 1)
            paste0("La cuantia bajo Cuentas Nocionales es de ", round(valor_pension, 2), " dolares")
        }
        else{
            valor_pension <- cuantia/axn(tabla_masculino, x = input$edad_jubilacion, 
                                         i = 0.04, k = 12, payment = "due", m = 0, n = 1)
            paste0("La cuantia bajo Cuentas Nocionales es de ", round(valor_pension, 2), " dolares")
        }
        })
    
})
