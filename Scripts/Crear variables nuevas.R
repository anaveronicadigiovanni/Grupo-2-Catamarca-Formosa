####asigno nombres a valores de PRESENCIA COMORBILIDADES"

 data <- data %>%
  mutate(PRESENCIA_COMORBILIDADES = case_match(PRESENCIA_COMORBILIDADES,
                                               1 ~ "Con comorbilidades",
                                               2 ~ "Sin comorbilidades",
                                               9 ~ "Sin dato",  
                                               .default = NA_character_ ))


# 2. Verificar el cambio
table(data$PRESENCIA_COMORBILIDADES)


data <- data %>%
  mutate(CON_OXIGENO = case_when(
    OXIGENOTERAPIA_BAJO_FLUJO == 1 | 
      OXIGENOTERAPIA_ALTO_FLUJO == 1 | 
      VM == 1 ~ "SI",
    .default = "NO"))

###########CREO COLUMNA "CON cafo vm"#########

data <-data%>% mutate(CON_CAFO_VM="no")

############## ASIGNO VALORES A COLUMNA con cafo vm########

 

data <- data %>%
  mutate(CON_CAFO_VM = case_when(
    OXIGENOTERAPIA_ALTO_FLUJO == 1 | VM == 1 ~ "SI",
    .default = "NO"
  ))

# 2. Verificar el cambio
table(data$CON_CAFO_VM)

#########SEVERIDAD #########

#########AGREGO COLUMNA #######

data<- data %>%
  mutate(SEVERIDAD = case_when(CON_CAFO_VM == "SI" ~ "CAFO y/o VM",
                                     CON_CAFO_VM == "NO"& CON_OXIGENO=="SI" ~ "Oxigeno Bajo Flujo",
                                     CON_OXIGENO == "NO" ~ "Sin Oxígeno"))

###AGREGO COLUMNA DE OTRO_DX#######


data <- data %>%
  mutate(OTRO_DX = if_else( DIAG_NEUMONIA== 1 | 
                              DIAG_BRONQUIOLITIS== 1 |
                              DIAG_SEPSIS==1|
                              DIAG_SHOCK_SEPTICO == 1, 
                               "",   # Valor si se cumple (TRUE)
                               "1")) # Valor si NO se cumple (FALSE)

# 2. Verificar el cambio
table(data$OTRO_DX)

########AGREGO VARIABLE VACUNA VRS############

unique(data$VAC_VSR)

data <- data %>%
  mutate(VACUNA_VRS = case_match(VAC_VSR,
                                                                 
                                    "SE 36"~ "madre vacunada",
                                     "SE 35"~ "madre vacunada",
                                     "SE 34"~ "madre vacunada",  
                                     "SE 33"~ "madre vacunada",                                                
                                     "SE 32"~ "madre vacunada",
                                     "SE DESCONOCIDA"~ "madre vacunada",
                                      "SIN DATO" ~ "Sin dato",
                                      "MADRE NO VACUNADA"~ "madre NO vacunada",
                                      .default = NA_character_ ))


table(data$VACUNA_VRS)
