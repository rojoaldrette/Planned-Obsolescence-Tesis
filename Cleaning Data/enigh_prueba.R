

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


# Eliminar objetos

rm(list=ls())

  # Setwd()

path <- readClipboard()
path <- gsub("\\\\", "/", path)
setwd(path)


  # Paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, readxl, openxlsx, lubridate, httr, jsonlite,
               tidyenigh, stringr, haven, foreign)

library(dataAPIs)

set_api_tokens(
  banxico_token = "cdd1fb5cef5f5c4302cd2fac0b9bb1518866008fc6c8d09d297548d56d00e2dd",
  inegi_token = "88cf3fd3-4f88-4448-98dd-d4c505b9c6f4",
  fred_token = "cd961dd4a15107e7dc6bdf663faf1f0f"
)

font_add_google("Montserrat")
showtext_auto()

# Data _____________________________________________________________________________________________________

#install.packages("devtools")

#devtools::install_github("estebandegetau/tidyenigh")

#library(tidyenigh)

# Inflación
inpc <- api_one.banxico("SP1", from = "2010-01-01", to = "2024-12-01")

# Cargando las bases

# Gastoshogar
gh24 <- read.csv("gastoshogar24.csv") %>%
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
  select(folioviv, foliohog, num_lavad, anio_lavad) %>%
  filter(!is.na(num_lavad))
hog16 <- read.csv("hogares16.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad)
hog14 <- read.csv("hogares14.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor_hog)
hog12 <- read.csv("hogares12.csv") %>%
  select(folioviv, foliohog, num_lavad, anio_lavad, factor_hog)
hog10 <- read.csv("hogares10.csv") %>%
  select(folioviv, foliohog, eqh12_n, eqh12_a, factor)
hog08 <- read.csv("hogares08.csv") %>%
  select(folioviv, foliohog, eqh4_12, factor)
hog06 <- read_dta("hogares06.dta") %>%
  select(folio, eqh07_23, eqh08_23, factor)
hog04 <- read.dbf("hogares04.dbf") %>%
  select(FOLIO, EQH10_23, FACTOR)
hog02 <- read.dbf("hogares02.dbf") %>%
  select(FOLIO, APAR59_28, APAR60_28, FACTOR)



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
  ) %>%
  group_by(folioviv) %>%
  mutate(
    n_hog = n(),
    factor = factor / n_hog
  )


hog18 <- hog18 %>%
  left_join(
    viv18 %>% select(folioviv, factor),
    by = "folioviv"
  ) %>%
  group_by(folioviv) %>%
  mutate(
    n_hog = n(),
    factor = factor / n_hog
  )
hog16 <- hog16 %>%
  left_join(
    viv16 %>% select(folioviv, factor),
    by = "folioviv"
  ) %>%
  group_by(folioviv) %>%
  mutate(
    n_hog = n(),
    factor = factor / n_hog
  )

colnames(hog14)[colnames(hog14) == "factor_hog"] <- "factor"
colnames(hog12)[colnames(hog12) == "factor_hog"] <- "factor"
colnames(hog04)[colnames(hog04) == "FACTOR"] <- "factor"
colnames(hog02)[colnames(hog02) == "FACTOR"] <- "factor"

colnames(hog10)[colnames(hog10) == "eqh12_n"] <- "num_lavad"
colnames(hog08)[colnames(hog08) == "eqh4_12"] <- "num_lavad"
colnames(hog06)[colnames(hog06) == "eqh07_23"] <- "num_lavad"
colnames(hog04)[colnames(hog04) == "EQH10_23"] <- "num_lavad"
colnames(hog02)[colnames(hog02) == "APAR59_28"] <- "num_lavad"

colnames(hog10)[colnames(hog10) == "eqh12_a"] <- "anio_lavad"


# Hog 08 hacerlo con binario bien

hog08 <- hog08 %>%
  mutate(num_lavad = ifelse(num_lavad == 2, 0, 1))


# Script _________________________________________________________________________________________________




# Función para obtener cantidad de ventas, vectores de precios,
# cantidad de hogares y así...

get_all <- function(gas, hog){
  
  # if (!is.null(gas)){
  # 
  #   gas <- gas
  #   hog <- hog %>%
  #     filter(num_lavad >= 1)
  # 
  #   if (!"factor" %in% names(gas)){
  #     gas <- gas %>%
  #       left_join(
  #         hog %>% select(folioviv, foliohog, factor),
  #         by = c("folioviv", "foliohog")
  #       )
  #   }
  # 
  # 
  #   # Cantidades de venta y precio
  #   precios <- c()
  #   cant_vendida <- 0
  #   print("Precios...")
  #   for (i in 1:nrow(gas)){
  #     cant_vendida <- cant_vendida + gas$factor[i]
  #     if ((!is.na(gas$costo[i]) && gas$costo[i] > 0) | !("costo" %in% colnames(gas))) {
  #       precios <- append(precios, gas$costo[i])
  #     } else if (!is.na(gas$gasto[i]) && gas$gasto[i] > 0) {
  #       precios <- append(precios, gas$gasto[i])
  #     } else if(is.na(gas$costo[i]) && is.na(gas$gasto[i])){
  #       precios <- append(precios, NA)
  #     }
  #   }
  # 
  # } else{
  #   precios <- NA
  # }
  
  
  # Cantidad hogar
  print("Cantidad hogar...")
  hog$lavad_n <- hog$num_lavad * hog$factor
  # Sumar directamente la cantidad expandida
  cant_hog <- sum(hog$lavad_n, na.rm = TRUE)
  serie_numlavad <- cant_hog
  
  # Años
  anios <- unique(hog$anio_lavad)
  lavad_anios <- data.frame(cant_total = cant_hog ) #, ventas = cant_vendida)
  hog <- hog %>%
    mutate(num_lavad=if_else((num_lavad >= 1 & !is.na(num_lavad)), 1, num_lavad))
  for (anio in anios) {
    lavad_anios[[as.character(anio)]] <- NA
  }
  for (anio in anios){
    hog_temp <- hog %>%
      filter(anio_lavad == anio) %>%
      filter(num_lavad == 1)
    lavad_anios[[as.character(anio)]] <- sum(hog_temp$lavad_n, na.rm = TRUE)
    
  }
  
  
  
  print("Listo!!")
  results <- list(
    df_final = lavad_anios,
    # precios = precios,
    serie_numlavad = serie_numlavad
  )
  
}

coso08 <- get_all(gas=NULL, hog08)
coso10 <- get_all(gh10, hog10)

coso12 <- get_all(gh12, hog12)
coso14 <- get_all(gh14, hog14)
coso16 <- get_all(gh16, hog16)
coso18 <- get_all(gh18, hog18)
coso20 <- get_all(gh20, hog20)
coso22 <- get_all(gh22, hog22)
coso24 <- get_all(gh24, hog24)

colnames(coso24$df_final) <- ifelse(
  grepl("^[0-9]$", colnames(coso24$df_final)),  # If it's a single digit
  paste0("0", colnames(coso24$df_final)),       # Add leading zero
  colnames(coso24$df_final)                     
)
colnames(coso12$df_final) <- ifelse(
  grepl("^[0-9]$", colnames(coso12$df_final)),  # If it's a single digit
  paste0("0", colnames(coso12$df_final)),       # Add leading zero
  colnames(coso12$df_final)                     
)
colnames(coso10$df_final) <- ifelse(
  grepl("^[0-9]$", colnames(coso10$df_final)),  # If it's a single digit
  paste0("0", colnames(coso10$df_final)),       # Add leading zero
  colnames(coso10$df_final)                     
)


coso_list <- list(coso08, coso10, coso12, coso14, coso16, coso18, coso20, coso22, coso24)


# Mejora de gastoshogar -----------

# Qué queremos de gastoshogar:
# - Estimar número de ventas nuevas (lavadoras entrado en la economía)
# - Obtener cuántas lavadoras están cambiando de mano (intraventas)
# - Obtener los precios a los que se están transando







# Cohortes ------------------

anios_df <- data.frame(id = c(2010, 2012, 2014, 2016, 2018, 2020, 2022, 2024),
                       cantidad = NA)

valores1 <- c("00", "01", "02", "03", "04", "05")
valores2 <- c("06", "07", "08", "09", "10")

valores <- c("90", "91", "92", "93", "94", 
             "95", "96", "97", "98", "99",
             "00", "01", "02", "03", "04", "05",
             "06", "07", "08", "09", "10",
             "11", "12", "13", "14", "15",
             "16", "17", "18", "19", "20",
             "21", "22", "23", "24")

for (valor in valores){
  anios_df[[as.character(valor)]] <- NA
}

for (i in 1:8){
  # Seleccionar un coso y seleccionar las columnas de interés
  coso_0 <- coso_list[[i]]
  coso_1 <- coso_0$df_final %>%
    select(-ventas)
  
  ## Obtener cantidad total primero y guardarla
  data_ini <- coso_1[1, 1] / 1000
  anios_df[i, "cantidad"] <- data_ini
  
  
  for (valor in valores){
    if (!(valor %in% colnames(coso_1))){
      anios_df[i, valor] <-  NA
    } else{
      data_00 <- coso_1[1, valor] / 1000
      anios_df[i, valor] <-  data_00 
    }
  }
  
}


anios_df <- anios_df %>%
  mutate(c1 = `90` + `91` + `92` + `93` + `94`,
         c2 = `95` + `96` + `97` + `98` + `99`,
         c3 = `00` + `01` + `02` + `03` + `04` + `05`,
         c4 = `06` + `07` + `08` + `09` + `10`)
anios_df$t <- seq(1, 8, by=1)


anios_df <- anios_df %>%
  mutate(t_c1 = 100 * c(0, diff(log(c1))),
         t_c2 = 100 * c(0, diff(log(c2))),
         t_c3 = 100 * c(0, diff(log(c3))),
         t_c4 = 100 * c(0, diff(log(c4))) )

anios_df <- anios_df %>%
  mutate(s_c1 = 100 * c1 / c1[1],
         s_c2 = 100 * c2 / c2[1],
         s_c3 = 100 * c3 / c3[1],
         s_c4 = 100 * c4 / c4[1] )


anios_df <- anios_df %>%
  mutate(p_c1 = -100 * c(0, diff(c1)) / c1[1],
         p_c2 = -100 * c(0, diff(c2)) / c2[1],
         p_c3 = -100 * c(0, diff(c3)) / c3[1],
         p_c4 = -100 * c(0, diff(c4)) / c4[1] )


# Se usa la función de rutils.R
# Gráficas

ts_graph(anios_df$id, list(anios_df$c1, anios_df$c2, anios_df$c3, anios_df$c4),
         jump=1000, lim1=0, lim2=10000, titulo = "Número de lavadoras por cohorte")


ts_graph(anios_df$id, list(anios_df$t_c1, anios_df$t_c2, anios_df$t_c3, anios_df$t_c4),
         jump=10, lim1=-100, lim2=20, titulo = "Tasa de cambio")

ts_graph(anios_df$id, list(anios_df$s_c1, anios_df$s_c2, anios_df$s_c3, anios_df$s_c4),
         jump=10, lim1=0, lim2=100, titulo = "% de unidades sobrevivientes")

ts_graph(anios_df$id, list(anios_df$p_99, anios_df$p_00, anios_df$p_06),
         jump=10, lim1=-10, lim2=100, titulo = "% de unidades perdidas c/r a inicial", from=2)











## Prueba cohortes de 10-15 y 16-20

# Grupos de años
g1 <- c("06", "07", "08", "09", "10")
g2 <- c("11", "12", "13", "14", "15")
g3 <- c("16", "17", "18", "19", "20")

cohorts <- data.frame(year = c(10, 12, 14, 16, 18, 20, 22, 24),
                      g1=NA, g2=NA, g3=NA)








# Calcular número de lavadoras en el tiempo ----------------------

# Aquí hay varios conflictos
# - La del 2008 es categórica -> 1:sí, 2:np, esto causa el problema de sub o 
# sobre estimación. Podríamos calcular la probabilidad de tener más de 1 lava-
# dora y sacar su distribución entre 2006 y 2010 para obtener una estimación
# Otra forma es sacar punto medio xd que genuinamente creo que no debería tener
# tanto error como otros métodos


# Lista de datos hogares

hogares <- list(hog02, hog04, hog06, hog08,
                hog10, hog12, hog14, hog16,
                hog18, hog20, hog22, hog24)


# Vamos a hacer otra función solo pa N de lavadoras

get_num_lavad <- function(hogares){
  niu <- hogares$num_lavad * hogares$factor
  total <- as.numeric(sum(niu))
  return(total)
}


num_df <- data.frame(fecha = seq(2002, 2024, by=2), num=NA)
for (i in 1:length(hogares)){
  cantidad <- get_num_lavad(hogares[[i]]) / 1000000
  num_df[i, 2] <- cantidad
  
}


table(hog18$num_lavad)

# Vamos a estimar acá la cantidad de lavad pa 2008
# Para esto vamos a obtener los porcentajes de numeros de lavad con una función


get_pct_nl <- function(hogares){

  hogares$num_factor <- as.factor(hogares$num_lavad)
  hogares$totales <- hogares$num_lavad * hogares$factor
  
  hogares_pct <- hogares %>%
    group_by(num_factor) %>%
    summarise(
      pct = 100 * sum(totales) / get_num_lavad(hogares)
    )
}


hog10_pct <- get_pct_nl(hog10)
hog06_pct <- get_pct_nl(hog06)

# Graficar de rutils

ts_graph(num_df$fecha, list(num_df$num), jump = 2, lim1=10, lim2=30, 
         byx=2, titulo = "Número de lavadoras en los hogares", ylab = "Millones de unidades")



