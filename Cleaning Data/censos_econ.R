

# ______________________________________________________________________________
#
# Proyecto:       Censo Económico
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

# rm(list=ls())


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


censo24 <- read.csv("C:\\Users\\rorya\\Desktop\\COLMEX\\TESIS\\po\\Datos\\censo economico\\conjunto_de_datos\\tr_ce_nac_2024.csv")



# Script _________________________________________________________________________________________________



















