

# ______________________________________________________________________________
#
# Proyecto:       Censo de población
#                
#
# Autor:          Rodrigo Aldrette
# Email:          raaldrettes@colmex.mx
#
# Fecha:          4 de octubre de 2025
#
# ______________________________________________________________________________




# OPCIONES _____________________________________________________________________


  # Para abrir el template
#file.edit("C:\\Users\\rorya\\AppData\\Roaming\\RStudio\\templates\\default.R")


options(scipen = 999)


# PREAMBULO ____________________________________________________________________


  # Setwd()

# path <- readClipboard()
# path <- gsub("\\\\", "/", path)
# setwd(path)


  # Eliminar objetos

rm(list=ls())


  # Paquetes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr)


# set_api_tokens(
#   banxico_token = "cdd1fb5cef5f5c4302cd2fac0b9bb1518866008fc6c8d09d297548d56d00e2dd",
#   inegi_token = "88cf3fd3-4f88-4448-98dd-d4c505b9c6f4",
#   fred_token = "cd961dd4a15107e7dc6bdf663faf1f0f"
# )



font_add_google("Montserrat")
showtext_auto()

# Data _____________________________________________________________________________________________________


# El censo abierto no es representativo

censo20 <- read.csv("C:\\Users\\rorya\\Desktop\\COLMEX\\TESIS\\po\\Datos\\censo pob\\cpv2020_cb_viviendas_ej.csv")

censo20 <- censo20 %>%
  dplyr::select(LAVADORA) %>%
  filter(!is.na(LAVADORA)) %>%
  mutate(lavadora = ifelse(LAVADORA == 3, 1, 0))

tot_lavad20 <- sum(censo20$lavadora)


# Script _________________________________________________________________________________________________



















