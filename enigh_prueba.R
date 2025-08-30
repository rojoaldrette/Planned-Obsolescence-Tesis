

# ______________________________________________________________________________
#
# Proyecto:       Enigh para estimar cosas
#                
#
# Autor:          Rodrigo Aldrette
# Email:          raaldrettes@colmex.mx
#
# Fecha:          F
#
# ______________________________________________________________________________




# OPCIONES _________________________________________________________________________________________________


  # Para abrir el template
#file.edit("C:\\Users\\rorya\\AppData\\Roaming\\RStudio\\templates\\default.R")


options(scipen = 999)


# PREAMBULO _________________________________________________________________________________________________


  # Setwd()

path <- readClipboard()
path <- gsub("\\\\", "/", path)
setwd(path)


  # Eliminar objetos

rm(list=ls())


  # Paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, readxl, openxlsx, lubridate, httr, jsonlite,
               tidyenigh, stringr)


font_add_google("Montserrat")
showtext_auto()

# Data _____________________________________________________________________________________________________

#install.packages("devtools")

#devtools::install_github("estebandegetau/tidyenigh")

#library(tidyenigh)



# Cargando las bases

# Gastoshogar
gh24 <- read.csv("gastoshogar24.csv",
                 col_select = c()) %>%
  filter(clave == "053121")
gh22 <- read.csv("gastoshogar22.csv") %>%
  filter(clave == "K015")
gh20 <- read.csv("gastoshogar20.csv") %>%
  filter(clave == "K015")
gh18 <- read.csv("gastoshogar18.csv") %>%
  filter(clave == "K015")
gh16 <- read.csv("gastoshogar16.csv") %>%
  filter(clave == "K015")
gh14 <- read.csv("gastoshogar14.csv") %>%
  filter(clave == "K015")
gh12 <- read.csv("gastoshogar12.csv") %>%
  filter(clave == "K015")
gh10 <- read.csv("gastoshogar10.csv") %>%
  filter(clave == "K015")

# Hogares
hog24 <- read.csv("hogares24.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor)
hog22 <- read.csv("hogares22.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor)
hog20 <- read.csv("hogares20.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad)
hog18 <- read.csv("hogares18.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad)
hog16 <- read.csv("hogares16.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad)
hog14 <- read.csv("hogares14.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor_hog)
hog12 <- read.csv("hogares12.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor_hog)
hog10 <- read.csv("hogares10.csv") %>%
  select(folioviv, foliohog, eqh12_n, eqh12_a, factor)


# Vivienda
viv24 <- read.csv("viviendas24.csv")
viv22 <- read.csv("viviendas22.csv")
viv20 <- read.csv("viviendas20.csv")
viv18 <- read.csv("viviendas18.csv")
viv16 <- read.csv("viviendas16.csv")
viv14 <- read.csv("viviendas14.csv")
viv12 <- read.csv("viviendas12.csv")


# Añadir factor a las que les falta (20, 18, 16)


hog20 <- hog20 %>%
  left_join(
    viv20 %>% select(folioviv, factor),
    by = "folioviv"
  )
hog18 <- hog18 %>%
  left_join(
    viv18 %>% select(folioviv, factor),
    by = "folioviv"
  )
hog16 <- hog16 %>%
  left_join(
    viv16 %>% select(folioviv, factor),
    by = "folioviv"
  )


# Script _________________________________________________________________________________________________




# Función para obtener cantidad de ventas, vectores de precios,
# cantidad de hogares y así...

get_all <- function(gas, hog, profile=0){

  gas <- gas
  hog <- hog %>%
    filter(num_lavad >= 1)
  
  if (!"factor" %in% names(gas)){
    gas <- gas %>%
      left_join(
        hog %>% select(folioviv, factor),
        by = "folioviv"
        )
  }
  
  
  # Cantidades y precio
  precios <- c()
  cant_vendida <- 0
  precios1 <- c()
  folios_1 <- c()
  print("Precios...")
  for (i in 1:nrow(gas)){
    if (!is.na(gas$costo[i]) && gas$costo[i] > 0) {
      precios1 <- append(precios1, gas$costo[i])
    } else if (!is.na(gas$gasto[i]) && gas$gasto[i] > 0) {
      precios1 <- append(precios1, gas$gasto[i])
    }
  }
  
  print("Casos...")
  # Caso no discriminación de precios
  if (profile == 0){
    for (i in 1:nrow(gas)){
      cant_vendida <- cant_vendida + gas$factor[i]
    }
    precios <- precios1
    folios_1 <- gas$folioviv
    
  # Caso discriminación de ricos (2 sd)
  } else if(profile == 1){
    
    for (i in 1:nrow(gas)){
      if (!is.na(gas$costo[i]) && gas$costo[i] > 0 && gas$costo[i] < (mean(precios1)+ sd(precios1))) {
        precios <- append(precios, gas$costo[i])
        cant_vendida <- cant_vendida + gas$factor[i]
        folios_1 <- append(folios_1, gas$folioviv)
      } else if (!is.na(gas$gasto[i]) && gas$gasto[i] > 0 && gas$gasto[i] < (mean(precios1)+ sd(precios1))) {
        precios <- append(precios, gas$gasto[i])
        cant_vendida <- cant_vendida + gas$factor[i]
        folios_1 <- append(folios_1, gas$folioviv)
      }
    }
  # Caso discrimnación de pobres (2 sd)
  } else if (profile == 2){
    
    for (i in 1:nrow(gas)){
      if (!is.na(gas$costo[i]) && gas$costo[i] > 0 && gas$costo[i] > (mean(precios1)+ sd(precios1))) {
        precios <- append(precios, gas$costo[i])
        cant_vendida <- cant_vendida + gas$factor[i]
        folios_1 <- append(folios_1, gas$folioviv)
      } else if (!is.na(gas$gasto[i]) && gas$gasto[i] > 0 && gas$gasto[i] > (mean(precios1)+ sd(precios1))) {
        precios <- append(precios, gas$gasto[i])
        cant_vendida <- cant_vendida + gas$factor[i]
        folios_1 <- append(folios_1, gas$folioviv)
      }
    }
  }
  
  # Cantidad hogar, creo que el %in% es O(n) o poco mejor, tarda mucho
  # Mejor debería hacerlo algo binario (folioviv que cumple y que no)
  print("Cantidad hogar...")
  # Sumar directamente la cantidad expandida
  cant_hog <- sum(hog$num_lavad * hog$factor, na.rm = TRUE)
  
  print("Listo!!")
  results <- list(
    hogares = cant_hog,
    ventas = cant_vendida,
    precios = precios
  )
  
  
  
}



coso20 <- get_all(gh20, hog20, viv20, profile = 0)
coso22 <- get_all(gh22, hog22, viv22, profile = 0)
coso24 <- get_all(gh24, hog24, viv24, profile = 0)
coso18 <- get_all(gh18, hog18, viv18, profile = 0)
coso16 <- get_all(gh16, hog16, viv16, profile = 0)



sales <- c(coso16$ventas, coso18$ventas, coso20$ventas, coso22$ventas, coso24$ventas) / 100000
lavadoras <- c(coso16$hogares, coso18$hogares, coso20$hogares, coso22$hogares, coso24$hogares) / 100000
media_p <- c(mean(coso20$precios), mean(coso22$precios), mean(coso24$precios))
med_p <- c(median(coso20$precios), median(coso22$precios), median(coso24$precios))
t <- c(2016, 2018, 2020, 2022, 2024)

twoyear_sales_1 <- 8 * (sales[1] + ((sales[2] - sales[1]) / 8))
twoyear_sales_2 <- 8 * (sales[2] + ((sales[3] - sales[2]) / 8))
twoyear_sales_3 <- 8 * (sales[3] + ((sales[4] - sales[3]) / 8))
twoyear_sales_4 <- 8 * (sales[4] + ((sales[5] - sales[4]) / 8))


delta_1 <- ((twoyear_sales_1 - lavadoras[2]) / lavadoras[1]) + 1
delta_2 <- ((twoyear_sales_2 - lavadoras[3]) / lavadoras[2]) + 1
delta_3 <- ((twoyear_sales_3 - lavadoras[4]) / lavadoras[3]) + 1
delta_4 <- ((twoyear_sales_4 - lavadoras[5]) / lavadoras[4]) + 1


deltas <- c(delta_1, delta_2, delta_3, delta_4)

plot(deltas)
plot(sales)




coso1 <- get_all(gh20, hog20, viv20, profile = 1)

coso2 <- get_all(gh20, hog20, viv20, profile = 2)
