##LLAMAR BASE CON CODIGO Q LEA ACENTOS

data <- read.csv("bases/UC_IRAG_CATAMARCA.csv", sep=";")

#Modifico encoding a UTF 8 para evitar incovenientes con el source
data$CLASIFICACION_MANUAL <- iconv(data$CLASIFICACION_MANUAL, from = "latin1", to = "UTF-8")

