#####ANALISIS UC IRAG CATAMARCA#####

##(Dios nos ampare!!)####
source("Scripts/Library.R")
source("Scripts/Importar base.R")

### VER DATAFRAME

glimpse(data)

###VER VALORES NA

find_na(data, rate=TRUE)


###VER COLUMNAS

colnames(data)

####EMPIEZO A FILTRAR LOS CRITERIOS DE EXCLUSION


unique(data$CLASIFICACION_MANUAL)

####SACO LOS QUE SON INVALIDADOS POR EPIDEMIOLOGIA###

data_EVENTO<- data %>% filter(CLASIFICACION_MANUAL=="Infeccion respiratoria aguda grave (IRAG)"|
                                  CLASIFICACION_MANUAL=="IRAG extendida")


unique(data_EVENTO$CLASIFICACION_MANUAL)

####SACO LOS DE 15 O MÄS AÑOS, los de 2024 y 2026 y los de SE53###

data_EVENTO<- data_EVENTO %>% filter(EDAD_DIAGNOSTICO<15 & 
                                     SEPI_FECHA_INTER<53 &
                                     ANIO_FECHA_INTER==2025)

####CREO COLUMNA NUEVA: RESULTADO###
                      
 data_resultado <- data_EVENTO %>% mutate(RESULTADO="NO")  
 
 ####COLOCO LOS VALORES DE COLUMNA RESULTADO SEGUN LAS VARIABLES COVID_19_FINAL, VRS_FINAL,INFLUENZA_FINAL
# PARA QUE SI LOS 3 ESTÁN SIN RESULTADO SE CATALOGUE COMO "INVALIDO"#####

 data_resultado <-data_resultado %>%  
       mutate(RESULTADO = if_else( COVID_19_FINAL== "Sin resultado" &
                                  INFLUENZA_FINAL== "Sin resultado" &  
                                  VSR_FINAL== "Sin resultado", "invalido", "valido"))
  
unique(data_resultado$RESULTADO)

###FILTRO PARA ELIMINAR LOS RESULTADOS INVALIDOS""

data_filtrada <- data_resultado %>% filter(RESULTADO=="valido")

unique(data_filtrada$RESULTADO)

###CREO UN DATAFRAME PARA CADA CLASIFICACION###


unique(data_filtrada$CLASIFICACION_MANUAL)

DATA_IRAG<-data_filtrada %>%filter(CLASIFICACION_MANUAL=="Infeccion respiratoria aguda grave (IRAG)")

DATA_IRAGE<-data_filtrada%>%filter(CLASIFICACION_MANUAL=="IRAG extendida")

###VEO IRAG POR GRUPO ETARIO Y SEXO###

DATA_IRAG_GRUPEDAD <- DATA_IRAG %>% 
  group_by(EDAD_UC_IRAG, SEXO)  %>%  #agrupa las filas por cada valor distinto en las variables grupo etario y sexo 
  summarise(CASOS = n())  #cuenta cuántas filas hay en cada grupo  y guarda el conteo en una columna llamada casos
   ungroup() # desagrupar los datos para evitar afectaciones posteriores
  
####ORDENO POR ORDEN DE GRUPO ETARIO###
   
   ###VER!!!
   
   
   ###VEO IRAGE POR GRUPO ETARIO Y SEXO###
   
   DATA_IRAGE_GRUPEDAD <- DATA_IRAGE %>% 
     group_by(EDAD_UC_IRAG, SEXO)  %>%  #agrupa las filas por cada valor distinto en las variables grupo etario y sexo 
     summarise(CASOS = n())  #cuenta cuántas filas hay en cada grupo  y guarda el conteo en una columna llamada casos
   ungroup() # desagrupar los datos para evitar afectaciones posteriores
   
   ####ORDENO POR ORDEN DE GRUPO ETARIO###   
   
   ###VER!!!!
   
   ####AGRUPO IRAG POR SE###
   DATA_IRAG_SE <- DATA_IRAG %>% 
     group_by(SEPI_FECHA_INTER)  %>%  
     summarise(CASOS = n())%>%
   ungroup() 

   ####AGRUPO IRAGE POR SE###
   DATA_IRAGE_SE <- DATA_IRAGE %>% 
     group_by(SEPI_FECHA_INTER)  %>%  
     summarise(CASOS = n())%>%
     ungroup() 
   
   ####BORRO LOS DATAFRAMES QUE NO NECESITO EN EL ENVIRONMENT####
   
   rm(data,data_EVENTO,data_resultado)
   
   #############COMORBILIDADES###################
   
   table(DATA_IRAG$PRESENCIA_COMORBILIDADES)
   
   
   #################################
   ###hago tabla de comorbilidades en las IRAG####
   
   tabla_comorbilidades_IRAG <- DATA_IRAG  |>
     summarise(across(c(PREMATURIDAD,BRONQUIOLITIS_PREVIA, ASMA,
                        CARDIOPATIA_CONGENITA,S_DOWN,DBP,ENF_RESPIRATORIA, 
                        ENF_NEUROLOGICA_CRONICA, ENF_NEUROMUSCULAR, ENF_RENAL,
                        ENF_CARDIACA, BAJO_PESO_NACIMIENTO), 
                      ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
    pivot_longer(cols = everything(), 
                  names_to = "Comorbilidades", 
                  values_to = "Prevalencia")|>
   arrange(desc(Prevalencia))
   
   # Mostrar el resultado final
   print(tabla_comorbilidades_IRAG)
   
   ###hago tabla de comorbilidades en las IRAGE####
   
   tabla_comorbilidades_IRAGE <- DATA_IRAGE  |>
     summarise(across(c(PREMATURIDAD,BRONQUIOLITIS_PREVIA, ASMA,
                        CARDIOPATIA_CONGENITA,S_DOWN,DBP,ENF_RESPIRATORIA, 
                        ENF_NEUROLOGICA_CRONICA, ENF_NEUROMUSCULAR, ENF_RENAL,
                        ENF_CARDIACA, BAJO_PESO_NACIMIENTO), 
                      ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
     pivot_longer(cols = everything(), 
                  names_to = "Comorbilidades", 
                  values_to = "Prevalencia")|>
     arrange(desc(Prevalencia))
   
   # Mostrar el resultado final
   print(tabla_comorbilidades_IRAGE)
   
   ###################################################################
   ######VER SI LOS SIGUIENTES dos PASOS NO TIENEN QUE IR ANTES!!!!!!!###################
######################################################################
   
   #1.Coloco nombres en lugar de los  1, 2 y 9 en variable PRESENCIA_COMORBILIDADEs en IRAG####   
  PRESENCIA_IRAG <- DATA_IRAG %>%
     mutate(PRESENCIA_COMORBILIDADES = case_match(PRESENCIA_COMORBILIDADES,
                                                  1 ~ "Con comorbilidades",
                                                  2 ~ "Sin comorbilidades",
                                                  9 ~ "Sin dato",  
                                                  .default = NA_character_ ))
  
   
   # 2. Verificar el cambio
   table(PRESENCIA_IRAG$PRESENCIA_COMORBILIDADES)
   
   
  #1.Coloco nombres en lugar de los  1, 2 y 9 en variable PRESENCIA_COMORBILIDADEs en IRAGE####
   PRESENCIA_IRAGE <- DATA_IRAGE %>%
     mutate(PRESENCIA_COMORBILIDADES = case_match(PRESENCIA_COMORBILIDADES,
                                                  1 ~ "Con comorbilidades",
                                                  2 ~ "Sin comorbilidades",
                                                  9 ~ "Sin dato",  
                                                  .default = NA_character_ ))
   
   
   # 2. Verificar el cambio
   table(PRESENCIA_IRAGE$PRESENCIA_COMORBILIDADES)
   
   
   ###########VER SEVERIDAD SEGUN REQUERIMIENTO DE CAFO O VM ##############
   
   ###########CREO COLUMNA "SIN_OXIGENO"#########
   
   data_filtrada <-data_filtrada %>% mutate(SIN_OXIGENO=1)
   
   ############## ASIGNO VALORES A COLUMNA SIN_OXIGENO ########
   
   #1.Coloco nombres en lugar de los  1, 2 y 9 en variable PRESENCIA_COMORBILIDADEs en IRAG####   
   SEVERIDAD <- data_filtrada %>%
     mutate(SIN_OXIGENO <- if_else(OXIGENOTERAPIA_BAJO_FLUJO==1 |
                                    OXIGENOTERAPIA_ALTO_FLUJO== 1|
                                    VM== 1)~ SIN_OXIGENO== "",
            ELSE="1")
          
   
   # 2. Verificar el cambio
   table(PRESENCIA_IRAG$PRESENCIA_COMORBILIDADES)
   
   ##################EN IRAG##################
  
 
   DATA_IRAG_CAFOUTI_SE <- DATA_IRAG %>% 
     group_by(OXIGENOTERAPIA_BAJO_FLUJO, OXIGENOTERAPIA_ALTO_FLUJO, SEPI_FECHA_INTER,CUIDADO_INTENSIVO)  %>%  #agrupa las filas por cada valor distinto en las variables grupo etario y sexo 
     summarise(CASOS = n())  #cuenta cuántas filas hay en cada grupo  y guarda el conteo en una columna llamada casos
   ungroup() # desagrupar los datos para evitar afectaciones posteriores
   
  ####################EN IRAGE###################
   
   DATA_IRAGE_O2_SE <- DATA_IRAGE %>% 
     group_by(SEPI_FECHA_INTER, OXIGENOTERAPIA_BAJO_FLUJO, OXIGENOTERAPIA_ALTO_FLUJO, VM)  %>%  #agrupa las filas por cada valor distinto en las variables grupo etario y sexo 
     summarise(CASOS = n())  #cuenta cuántas filas hay en cada grupo  y guarda el conteo en una columna llamada casos
   ungroup() # desagrupar los datos para evitar afectaciones posteriores
   
   ############QUIERO EXPORTAR MIS DATAFRAMES A EXCEL PARA VER GRAFICOS POSIBLES##########
   
   write_xlsx(DATA_IRAG_CAFOUTI_SE, "C:/USers/Tobias/Desktop/ejercicio1/salidas/severidad.xlsx")
   write_xlsx(DATA_IRAG, "C:/USers/Tobias/Desktop/ejercicio1/salidas/DATA_IRAG.xlsx")
   write_xlsx(data_filtrada, "C:/USers/Tobias/Desktop/ejercicio1/salidas/DATA_FILTRADA.xlsx")
   