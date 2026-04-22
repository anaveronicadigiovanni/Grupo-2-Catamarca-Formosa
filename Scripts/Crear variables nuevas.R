####asigno nombre a PRESENCIA COMORBILIDADES"

 data <- data %>%
  mutate(PRESENCIA_COMORBILIDADES = case_match(PRESENCIA_COMORBILIDADES,
                                               1 ~ "Con comorbilidades",
                                               2 ~ "Sin comorbilidades",
                                               9 ~ "Sin dato",  
                                               .default = NA_character_ ))


# 2. Verificar el cambio
table(data$PRESENCIA_COMORBILIDADES)

###########CREO COLUMNA "SIN_OXIGENO"#########

data <-data%>% mutate(SIN_OXIGENO=1)

############## ASIGNO VALORES A COLUMNA SIN_OXIGENO ########

 
data <- data %>%
  mutate(CON_OXIGENO = if_else(OXIGENOTERAPIA_BAJO_FLUJO == 1 | 
                                 OXIGENOTERAPIA_ALTO_FLUJO == 1 | 
                                 VM == 1, 
                               "1",   # Valor si se cumple (TRUE)
                               "")) # Valor si NO se cumple (FALSE)

# 2. Verificar el cambio
table(data$CON_OXIGENO)

##################EN IRAG##################


DATA_IRAG_CAFOUTI_SE <- DATA_IRAG %>% 
  group_by(OXIGENOTERAPIA_BAJO_FLUJO, OXIGENOTERAPIA_ALTO_FLUJO, SEPI_FECHA_INTER,CUIDADO_INTENSIVO)  %>%  #agrupa las filas por cada valor distinto en las variables grupo etario y sexo 
  summarise(CASOS = n())  #cuenta cuántas filas hay en cada grupo  y guarda el conteo en una columna llamada casos
ungroup() # desagrupar los datos para evitar afectaciones posteriores