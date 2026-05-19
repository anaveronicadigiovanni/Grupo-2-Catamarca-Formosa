#CARGAR LIBRERIAS#


# library(dplyr) #transformación de datos
# library(lubridate) #trabajo con fechas
# library(dlookr) #exploración de datos
# library(readr) #lectura de archivos
# library(readxl) #lectura de archivos. xlsx
# library(writexl) #exportar archivos .xlsx
# library(gt) # libreria para tablas 
# library(gtable) #libreria para tablaslibrary(gt)
# library(gtExtras)
# 
# library(tidyr)
# library(ggplot2)#tablas
# library(knitr)#tablas lindas
# library(highcharter) #graficos interactivos
# library(stringr)
# library(here)
# library(stringi)
# library (purrr)

#Librerias cargadas##

# Función para verificar, instalar y cargar librerías
verificar_cargar_librerias <- function(librerias) {
  librerias_faltantes <- librerias[!(librerias %in% installed.packages()[,"Package"])]
  if(length(librerias_faltantes) > 0) install.packages(librerias_faltantes, dependencies = TRUE)
  for(libreria in librerias) library(libreria, character.only = TRUE)
}

# Lista de librerías necesarias
librerias_necesarias <- c(
  "dplyr",      # transformación de datos
  "lubridate",  # trabajo con fechas
  "dlookr",     # exploración de datos
  "readr",      # lectura de archivos
  "readxl",     # lectura de archivos .xlsx
  "writexl",    # exportar archivos .xlsx
  "gt",         # libreria para tablas
  "gtable",     # libreria para tablas
  "gtExtras",   
  "tidyr",
  "ggplot2",    # gráficos
  "knitr",      # tablas lindas
  "highcharter", # gráficos interactivos
  "stringr",
  "here",
  "stringi",
  "purrr"
)

# Ejecutar la función
verificar_cargar_librerias(librerias_necesarias)
 

