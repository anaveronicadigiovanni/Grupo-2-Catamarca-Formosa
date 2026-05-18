##LLAMAR BASE CON CODIGO Q LEA ACENTOS

data <- read.csv("bases/UC_IRAG_CATAMARCA.csv", sep=";")

#Modifico encoding a UTF 8 para evitar incovenientes con el source
data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")

data$EDAD_UC_IRAG <- iconv(data$EDAD_UC_IRAG, from = "latin1", to = "UTF-8")

AGRUPADOS <-read_excel("Bases/UC IRAG-Carga_agrupada-Catamarca.xlsx")

