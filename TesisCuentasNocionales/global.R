library(readxl)
library(lifecontingencies)

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
