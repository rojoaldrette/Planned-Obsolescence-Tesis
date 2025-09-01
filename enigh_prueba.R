

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
               tidyenigh, stringr)

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

colnames(hog14)[colnames(hog14) == "factor_hog"] <- "factor"
colnames(hog12)[colnames(hog12) == "factor_hog"] <- "factor"

colnames(hog10)[colnames(hog10) == "eqh12_n"] <- "num_lavad"
colnames(hog10)[colnames(hog10) == "eqh12_a"] <- "anio_lavad"

# Script _________________________________________________________________________________________________




# Función para obtener cantidad de ventas, vectores de precios,
# cantidad de hogares y así...

get_all <- function(gas, hog){

  gas <- gas
  hog <- hog %>%
    filter(num_lavad >= 1)
  
  if (!"factor" %in% names(gas)){
    gas <- gas %>%
      left_join(
        hog %>% select(folioviv, foliohog, factor),
        by = c("folioviv", "foliohog")
        )
  }
  
  
  # Cantidades de venta y precio
  precios <- c()
  cant_vendida <- 0
  print("Precios...")
  for (i in 1:nrow(gas)){
    cant_vendida <- cant_vendida + gas$factor[i]
    if (!is.na(gas$costo[i]) && gas$costo[i] > 0) {
      precios <- append(precios, gas$costo[i])
    } else if (!is.na(gas$gasto[i]) && gas$gasto[i] > 0) {
      precios <- append(precios, gas$gasto[i])
    } else if(is.na(gas$costo[i]) && is.na(gas$gasto[i])){
      precios <- append(precios, NA)
    }
  }
  
  
  # Cantidad hogar
  print("Cantidad hogar...")
  hog$lavad_n <- hog$num_lavad * hog$factor
  # Sumar directamente la cantidad expandida
  cant_hog <- sum(hog$lavad_n, na.rm = TRUE)
  
  # Años
  anios <- unique(hog$anio_lavad)
  lavad_anios <- data.frame(cant_total = cant_hog,
                            ventas = cant_vendida)
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
    precios = precios
  )
  
}


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


coso_list <- list(coso12, coso14, coso16, coso18, coso20, coso22, coso24)

View(table(hog10$eqh12_a))
View(table(hog12$anio_lavad))
View(table(hog14$anio_lavad))
View(table(hog16$anio_lavad))
View(table(hog18$anio_lavad))


anios_df <- data.frame(id = c(2012, 2014, 2016, 2018, 2020, 2022, 2024),
                       cantidad = NA)

valores1 <- c("00", "01", "02", "03", "04", "05")
valores2 <- c("06", "07", "08", "09", "10")

valores <- c("95", "96", "97", "98", "99", "00", "01", "02", "03", "04", "05",
             "06", "07", "08", "09", "10")
for (valor in valores){
  anios_df[[as.character(valor)]] <- NA
}

for (i in 1:7){
  coso_0 <- coso_list[[i]]
  coso_1 <- coso_0$df_final %>%
    select(-ventas)
  
  data_ini <- coso_1[1, 1] / 1000
  anios_df[i, "cantidad"] <- data_ini
  
  for (valor in valores){
    data_00 <- coso_1[1, valor] / 1000
    anios_df[i, valor] <-  data_00
  }
  
}


anios_df <- anios_df %>%
  mutate(`95-99` = `95` + `96` + `97` + `98` + `99`,
         `00-05`=`00` + `01` + `02` + `03` + `04` + `05`,
         `06-10`=`06` + `07` + `08` + `09` + `10`)
anios_df$t <- seq(1, 7, by=1)


anios_df <- anios_df %>%
  mutate(t_99 = 100 * c(0, diff(log(`95-99`))),
         t_00 = 100 * c(0, diff(log(`00-05`))),
         t_06 = 100 * c(0, diff(log(`06-10`))) )

anios_df <- anios_df %>%
  mutate(s_99 = 100 * `95-99` / `95-99`[1],
         s_00 = 100 * `00-05` / `00-05`[1],
         s_06 = 100 * `06-10` / `06-10`[1] )


anios_df <- anios_df %>%
  mutate(p_99 = -100 * c(0, diff(`95-99`)) / `95-99`[1],
         p_00 = -100 * c(0, diff(`00-05`)) / `00-05`[1],
         p_06 = -100 * c(0, diff(`06-10`)) / `06-10`[1] )


# Se usa la función de rutils.R
# Gráficas
ts_graph(anios_df$id, list(anios_df$`95-99`, anios_df$`00-05`, anios_df$`06-10`),
         jump=1000, lim1=0, lim2=10000, titulo = "Número de lavadoras por cohorte")

ts_graph(anios_df$id, list(anios_df$t_99, anios_df$t_00, anios_df$t_06),
         jump=10, lim1=-100, lim2=20, titulo = "Tasa de cambio")

ts_graph(anios_df$id, list(anios_df$s_99, anios_df$s_00, anios_df$s_06),
         jump=10, lim1=0, lim2=100, titulo = "% de unidades sobrevivientes", from=2)

ts_graph(anios_df$id, list(anios_df$p_99, anios_df$p_00, anios_df$p_06),
         jump=10, lim1=-10, lim2=100, titulo = "% de unidades perdidas c/r a inicial", from=2)







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
