library(readxl)
library(lifecontingencies)
library(data.table)

#data_coeficiente <- read_xlsx("Datos/Coeficientes_Sis_Actual.xlsx")
#data_coeficiente_min_max <- read_xlsx("Datos/Coeficientes_PensionMinMax.xlsx")
#data_afiliados <- read_xlsx("Datos/Distribucion_Afiliados.xlsx",
 #                          sheet = "2018")

#pib_nominal <- read_xlsx("Datos/PIB_NOMINAL.xlsx")

# Data para las probabilidades de muerte segun el genero
#femenino <- read_xlsx("Datos/Tabla_Mortalidad_Ecuador.xlsx",
 #                     sheet = "Probs_Mujeres")
#masculino <- read_xlsx("Datos/Tabla_Mortalidad_Ecuador.xlsx",
 #                      sheet = "Probs_Hombres")

# se transforman las probabilidades q_x a formato lifecontingencies
#tabla_femenino <- probs2lifetable(femenino$q_2021, radix = 100000, type = "qx",
 #                                 name = "Probabilidad Femenino")
#tabla_masculino <- probs2lifetable(masculino$q_2021, radix = 100000, type = "qx",
 #                                  name = "Probabilidad Masculino")


SBU <- 400
tipo_cotizacion <- 0.1046
crecimiento_salario <- 0.0215
crecimiento_pib <- 0.01675
inf_actuarial <- 95

#### FUNCIONES ####

#### Cuantia Sistema Actual ####
## Para extraer el valor del coeficiente
coeficiente <- function(imposiciones){
  coefi <- data_coeficiente %>% 
    dplyr::filter(Numero_Impo == imposiciones) %>% 
    select(Coeficiente)
  as.numeric(coefi)
}

## Para extraer el valor minimo, segun los años de aportacion
minimo <- function(imposiciones){
  coefi <- data_coeficiente_min_max %>% 
    dplyr::filter(Tiempo_Aportacion == imposiciones) %>% 
    select(Coef_Minimo)
  as.numeric(coefi)
}

## Para extraer el coeficiente para el valor maximo de la pension
maximo <- function(imposiciones){
  coefi <- data_coeficiente_min_max %>% 
    dplyr::filter(Tiempo_Aportacion == imposiciones) %>% 
    select(Coef_Maximo)
  as.numeric(coefi)
}


salarios <- function(salario, fecha_nacimiento, fecha_inicio, edad_jubilacion){
  meses_faltantes_anio_1 <- 12 - month(fecha_inicio)
  salarios <- data.table()
  salarios <- data.table(salarios_anuales = salario*meses_faltantes_anio_1)
  
  anio_inicio <- year(fecha_inicio)
  anio_nacimiento <- year(fecha_nacimiento)
  anio_jubilacion <- anio_nacimiento + edad_jubilacion
  dif_anios <- anio_jubilacion - anio_inicio
  
  for (i in 1:(dif_anios - 1)) {
    salarios <- rbind(salarios, data.table(salarios_anuales = 12*salario*(1 + crecimiento_salario)^i))
  }
  
  meses_antes_de_la_jubilacion <- month(fecha_nacimiento) - 1
  salarios <- 
    rbind(salarios, data.table(salarios_anuales = 
                                 meses_antes_de_la_jubilacion*salario*(1 + crecimiento_salario)^dif_anios))
  
  return(salarios)
}

salarios(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 30)

salarios_2 <- function(salario, fecha_nacimiento, fecha_inicio, edad_jubilacion){
  meses_faltantes_anio1 <- 12 - month(fecha_inicio)
  anio_inicio <- year(fecha_inicio)
  salarios <- data.table()
  salarios <- data.table(anio = rep(anio_inicio, meses_faltantes_anio1),
                         salario_mensual = salario)
  
  anio_nacimiento <- year(fecha_nacimiento)
  anio_jubilacion <- anio_nacimiento + edad_jubilacion
  dif_anios <- anio_jubilacion - anio_inicio
  
  for (i in 1:(dif_anios - 1)) {
    salarios <- rbind(salarios, data.table(anio = rep(anio_inicio + i, 12),
                           salario_mensual = salario*(1 + crecimiento_salario)^i))
  }
  
  meses_faltantes_final <- month(fecha_nacimiento) - 1
  salarios <- rbind(salarios, data.table(anio = rep(anio_jubilacion, meses_faltantes_final),
                         salario_mensual = salario*(1 + crecimiento_salario)^dif_anios))
  
  return(salarios)
}

#a <- salarios_2(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 30)

cuantia_sistema_actual <- function(salario, fecha_nacimiento, fecha_inicio, edad_jubilacion){
  salarios_sis_act <- salarios_2(salario, fecha_nacimiento, fecha_inicio, edad_jubilacion)
  mejores_salarios <- data.table()
  aux <- floor(salarios_sis_act[ , .N]/12)
  for (i in 1 : aux) {
    mejores_salarios <- rbind(mejores_salarios, 
                              data.table(
                                salario = prod(salarios_sis_act[c((12*(i-1)+1):(12*i)), 
                                                               salario_mensual])))
  }
  
  aux_2 <- salarios_sis_act[ , .N]%%12
  
  mejores_salarios <- rbind(mejores_salarios,
                            data.table(
                              salario = prod(salarios_sis_act[
                                c((salarios_sis_act[,.N]-aux_2+1):salarios_sis_act[,.N]), 
                                salario_mensual])))
  
  mejores_salarios <- mejores_salarios[with(mejores_salarios, order(mejores_salarios$salario, 
                                                                    decreasing = T)), ]
  
  mejores_salarios <-  mejores_salarios[1:5]
  
  salario_afiliado <- prod(mejores_salarios$salario)^(1/60)
  
  imposiciones <- floor(salarios_sis_act[,.N]/12)
  indice <- which(data_coeficiente$Numero_Impo == imposiciones)
  
  coeficiente <- data_coeficiente$Coeficiente[indice]
  
  salario_afiliado <- salario_afiliado*coeficiente
  
  return(round(as.numeric(salario_afiliado), 2))
}

#cuantia_sistema_actual(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 70)

a <- salarios(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 39)

capital_nocional <- function(genero, edad_inicio, edad_jubilacion, sueldos, tanto_nocional){
  sueldos[ , (c("incr_anual", "incr_historico")) := 0]
  aux <- edad_jubilacion - edad_inicio + 1
  
  for (i in 1:aux) {
    sueldos[i, "incr_anual"] = 1 + tanto_nocional*(1 + crecimiento_pib)^(i - 1)
  }
  
  for (i in 1:aux) {
    sueldos[i, "incr_historico"] = prod(sueldos[c(i:sueldos[,.N]), "incr_anual"])
  }
  
  sueldos <- sueldos %>% 
    mutate(
      sueldo_historico = salarios_anuales*tipo_cotizacion*incr_historico)
  
  K <- 0
  K <- sueldos[, (k = sum(sueldo_historico))]
  
  cuantia <- 0
  
    if(genero == "Femenino"){
     renta_vitalicia <- axn(tabla_femenino, x = edad_jubilacion, i = 0.04, m = 0, 
                           k = 13, payment = "due")
   cuantia <- K/renta_vitalicia
  }
  else{
   renta_vitalicia <- axn(tabla_masculino, x = edad_jubilacion, i = 0.04, m = 0, 
                         k = 13, payment = "due")
  cuantia <- K/renta_vitalicia
  }
  
  #return(K)
  return(round(as.numeric(cuantia), 2))
}

a <- salarios(100, as.Date("1995-07-01"), as.Date("2021-09-07"), 39)
capital_nocional("Femenino", 26, 39, a, 0.02)


### Funcion para el grafico sis. actual vs nocional

grafico_actual_nocional <- function(y){
  grafico <- ggplot() +
        aes(x = c("Cuantia Sistema Actual", "Cuantia Cuentas Nocionales"), 
            y, 
            fill = c("Cuantia Sistema Actual", "Cuantia Cuentas Nocionales")) + 
        geom_bar(position = "dodge", stat = "identity", show.legend = F) +
        geom_text(aes(label = y) , vjust = -0.3, color = "black", size = 5,
                  position = position_dodge(0.9)) +
        labs(title = "Comparacion del valor de las cuantias",
              subtitle = "Sistema Actual vs Cuentas Nocionlale",
              x = "", y = "Cuantia ($)") +
        scale_fill_manual("Legend", values = c("springgreen4", "steelblue4"))
  
  return(grafico)
}


## Extraer los datos de la base para el calculo de la cuantia con distribucion de afiliados
salario_base_con_data <- function(edad_inicio, edad_jubilacion){
  datos <- data_afiliados %>% 
    dplyr::filter(Edad >= edad_inicio, Edad < edad_jubilacion) %>% 
    select(Promedio_Sueldo) %>% 
    arrange(desc(Promedio_Sueldo)) %>% 
    head(n = 5)
  
  aux <- 1
  
  for (i in 1:5) {
    aux <- aux*datos[i, 1]
  }
  
  base_calculo <- aux^(1/5)
  
  round(as.numeric(base_calculo), 2)
}

## Funcion para saber cuanto dinero acumuló el individuo durante su vida laboral (con data)
ahorro_acumulado_con_data <- function(edad_inicio, edad_jubilacion, tipo_cotizacion){
  ahorro_acumulado <- data_afiliados %>% 
    dplyr::filter(Edad >= edad_inicio, Edad < edad_jubilacion) %>% 
    select(Promedio_Sueldo) %>% 
    mutate(promedio_sueldo_anual = Promedio_Sueldo*12*tipo_cotizacion) %>% 
    summarise(sum(promedio_sueldo_anual))
  
  return(as.numeric(ahorro_acumulado))
}


#### CUENTAS NOCIONALES ####

cuantia_cuentas_nocionales_sis_1 <- function(edad_inicio, edad_jubilacion, salario, 
                                             tipo_cotizacion, tanto_nocional){
  cuantia <- 0
  for (i in edad_inicio:(edad_jubilacion - 1)) {
    aux <- 1
    for (j in i:(edad_jubilacion - 1)) {
      aux <- aux*(1 + tanto_nocional)
    }
    cuantia <- cuantia + tipo_cotizacion*salario*12*aux
  }
  return(cuantia)
}

cuantia_cuentas_nocionales <- function(edad_inicio, edad_jubilacion, tipo_cotizacion, tanto_nocional){
  datos <- data_afiliados %>% 
    dplyr::filter(Edad >= edad_inicio, Edad < edad_jubilacion) %>% 
    select(Número, Promedio_Sueldo) %>% 
    mutate(base_salario = Promedio_Sueldo*12*tipo_cotizacion)
  
  datos$nocional <- 0
  diferencia <- edad_jubilacion - edad_inicio
  
  for (i in 1:diferencia) {
    datos[i, "nocional"] = (1 + tanto_nocional)^(diferencia - i + 1)
  }
  
  K <- datos %>% 
    mutate(K = base_salario*nocional) %>% 
    summarise(sum(K))
  
  return(as.numeric(K))
}

## Funcion para obtener la cuantia de cuentas nocionales utilizando el pib como tanto nocional
cuantia_pib <- function(edad_inicio, edad_jubilacion, tipo_cotizacion){
  datos <- data_afiliados %>% 
    dplyr::filter(Edad >= edad_inicio, Edad < edad_jubilacion) %>% 
    select(Número, Promedio_Sueldo) %>% 
    mutate(base_salario = Promedio_Sueldo*12*tipo_cotizacion)
  
  anio_actual <-  as.numeric(year(today()))
  anio_inicio_trabajo <- anio_actual - (edad_jubilacion - edad_inicio)
  
  datos_pib <- pib_nominal %>% 
    dplyr::filter(año >= anio_inicio_trabajo) %>% 
    select(pib)
  
  datos$tanto_nocional <- 0
  diferencia <- edad_jubilacion - edad_inicio
  for (i in 1:diferencia) {
    aux <- 1
    for (j in 1:diferencia) {
      aux <- aux*(1 + datos_pib[j, "pib"]*0.01)
    }
    datos[i, "tanto_nocional"] <- aux
  }
  
  K <- datos %>% 
    mutate(K = base_salario*tanto_nocional) %>% 
    summarise(sum(K))
  
  return(as.numeric(K))
}
