###APLICO CRITERIOS DE EXCLUSIÓN#####

####SACO LOS QUE SON INVALIDADOS POR EPIDEMIOLOGIA###

data<- data %>% filter(CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)"|
                                CLASIFICACION_MANUAL=="IRAG extendida")


unique(data$CLASIFICACION_MANUAL)

####SACO LOS DE 15 O MÄS AÑOS, los de 2024 y 2026 y los de SE53###

data<- data %>% filter(EDAD_DIAGNOSTICO<15 & 
                                       SEPI_FECHA_INTER<53 &
                                       ANIO_FECHA_INTER==2025)
data <-data |>filter(ID !=10477)

####CREO COLUMNA NUEVA: RESULTADO###

data <- data %>% mutate(RESULTADO="NO")  

####COLOCO LOS VALORES DE COLUMNA RESULTADO SEGUN LAS VARIABLES COVID_19_FINAL, VRS_FINAL,INFLUENZA_FINAL
# PARA QUE SI LOS 3 ESTÁN SIN RESULTADO SE CATALOGUE COMO "INVALIDO"#####

data <-data %>%  
  mutate(RESULTADO = if_else( COVID_19_FINAL== "Sin resultado" &
                                INFLUENZA_FINAL== "Sin resultado" &  
                                VSR_FINAL== "Sin resultado", "invalido", "valido"))

unique(data$RESULTADO)

###FILTRO PARA ELIMINAR LOS RESULTADOS INVALIDOS""

data <- data %>% filter(RESULTADO=="valido")

unique(data$RESULTADO)

