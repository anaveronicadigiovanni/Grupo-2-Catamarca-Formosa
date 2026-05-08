

####asigno nombres a valores de PRESENCIA COMORBILIDADES"



 data <- data %>%
  mutate(PRESENCIA_COMORBILIDADES = case_when(PRESENCIA_COMORBILIDADES,
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

####################################################

#CREO VARIABLES PARA GRAFICOS DE VIRUS######


#########UNIFICO VSR###

data <- data %>%
  mutate(VRS = case_match(VSR_FINAL,
                          
                                 "VSR"~ "POSITIVO",
                                 "VSR A"~ "POSITIVO",
                                 "VSR B"~ "POSITIVO",  
                                 "Negativo"~ "Negativo",                                                
                                 "Sin resultado"~ "Negativo",
                                   .default ="Negativo"))

table(data$VRS)


####UNIFICO FLU####

data <- data %>%
  mutate(FLU = case_match(INFLUENZA_FINAL,
                          
                          "Influenza A (sin subtipificar)"~ "POSITIVO",
                          "	Influenza A H1N1"~ "POSITIVO",
                          "Influenza B (sin linaje)"~ "POSITIVO",
                          "Influenza B Victoria"~ "POSITIVO",
                          "Negativo"~ "Negativo",                                                
                          "Sin resultado"~ "Negativo",
                          .default = "Negativo" ))

####unifico SARSCov 2#####

data <- data %>%
  mutate(SARS_COV_2 = case_match(COVID_19_FINAL,
                                 "Positivo" ~ "POSITIVO",
                                 "Negativo" ~ "Negativo",
                                 "Sin resultado" ~ "Negativo",
                                  .default = "Negativo" ))
####################################################################################

data <- data %>%
  mutate(Virus_sr = case_match(VSR_FINAL,
                               
                               "VSR"~ 1,
                               "VSR A"~ 1,
                               "VSR B"~ 1,  
                               
                               .default = 0 ))


table(data$Virus_sr)


########

data <- data %>%
  mutate(Virus_influenza = case_match(INFLUENZA_FINAL,
                                      
                                      "Influenza A (sin subtipificar)"~ 1,
                                      "	Influenza A H1N1"~ 1,
                                      "Influenza B (sin linaje)"~ 1,
                                      "Influenza B Victoria"~ 1,
                                      
                                      .default = 0))

table(data$Virus_influenza)

#########

data <- data %>%
  mutate(covid = case_match(COVID_19_FINAL,
                            "Positivo" ~ 1,
                            .default =  ))

table(data$covid)

#######################

data <- data %>%
  mutate(NEGATIVOS = case_when(COVID_19_FINAL=="Negativo" & 
                                 INFLUENZA_FINAL== "Negativo"& 
                                 VSR_FINAL=="Negativo"
                               ~ 1,
                               .default = 0 ))

table(data$NEGATIVOS)

###CAMBIO LOS NOMBRES DE LAS ULTIMAS 4 VARIABLES

data<-data |>
  rename(
 ## NUEVO_NOMBRE = nombre_viejo,
    COVID = covid,
    Influenza= Virus_influenza,
    VSR = Virus_sr
  )
