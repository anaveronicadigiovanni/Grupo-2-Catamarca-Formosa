#####CÓDIGO PARA NO TENER PROBLEMAS CON Ñ o ACENTOS

data_irag_limpio <- data_irag %>%
mutate(across(where(is.character), ~ {
  # Convierte a UTF-8 nativo y escapa los elementos no válidos
  x_enc <- iconv(.x, to = "UTF-8", sub = " ")
  # Codificación estricta para asegurar compatibilidad HTML/JSON
  utf8::utf8_encode(x_enc)
}))


######Agrupo por grupo de edad y sexo####



  GRUPEDAD_IRAG<- data_irag_limpio %>%
  group_by(EDAD_UC_IRAG, SEXO)%>% 
  summarise(CASOS = n())%>%
ungroup()
  
  ###ordeno de mnor a mayor edad"
 GRUPEDAD_IRAG <- GRUPEDAD_IRAG %>%
 arrange(
   # Extraemos el número inicial, lo pasamos a numérico y multiplicamos por 12 si es año
   as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")) * 
     ifelse(str_detect(EDAD_UC_IRAG, "Años"), 12, 1)
 )%>%
   #tabla a lo ancho####
 pivot_wider(names_from = SEXO, values_from = CASOS)
 
 ####grafico barras horizontales agrupadas
 
 curva_irag_sex <- highchart() %>%
   hc_chart(type = "bar") %>% # Cambiado de "column" a "bar"
  # hc_title(text = "Distribución de Casos Notificados de IRAG por Sexo y Grupo Etario") %>% 
   hc_plotOptions(bar = list(stacking = NULL, # Cambiado de column a bar
                             pointPadding = 0.05,   
                             groupPadding = 0.1,  
                             borderWidth = 0)) %>%
   hc_xAxis(
     categories = GRUPEDAD_IRAG$EDAD_UC_IRAG, 
     title = list(text = "Grupo etario")) %>%  
   hc_yAxis(title = list(text = "Casos notificados")) %>%
   hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
              enabled = TRUE) %>% 
   hc_add_series(
     data = GRUPEDAD_IRAG$`F`,
     name = "Femenino",
     color = "#f9a58c") %>%
   hc_add_series(
     data = GRUPEDAD_IRAG$`M`,
     name = "Masculino",
     color = "#9bc4e2")
 
 ###LLAMO AL GRAFICO###
 curva_irag_sex
 
 ###Grafico columnas agrupadas
 
# curva_irag_sex <-highchart() %>%
#   hc_chart(type= "column") %>%
#   hc_title(text = "Distribución de Casos Notificados de IRAG por Sexo y Grupo Etario") %>% 
#   hc_plotOptions(column = list(stacking = NULL,
 #                               pointPadding = 0.05,   
 #                               groupPadding = 0.1,  
  #                              borderWidth = 0)) %>%
 #  hc_xAxis(
#     categories =GRUPEDAD_IRAG$EDAD_UC_IRAG, #categorías en eje X
 #    title = list(text = "Grupo etario")) %>%  #título del eje X) 
#   hc_yAxis(title= list(text="Casos notificados")) %>%
#   hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
#              enabled = TRUE) %>% 
#   hc_add_series(
 #    data = GRUPEDAD_IRAG$`F`,
 ##    name = "Femenino",
 #    color = "#f9a58c") %>%
 #  hc_add_series(
 #    data = GRUPEDAD_IRAG$`M`,
 #    name = "Masculino",
#     color = "#9bc4e2") 

 # LIMPIEZA RIGUROSA DE CODIFICACIÓN (Soluciona el error de gsub/payload)
 data_irag_limpio <- data_irag %>%
   mutate(across(where(is.character), ~ {
     # 1. Convierte a caracteres explícitos por si hay factores ocultos
     x <- as.character(.x)
     # 2. Transforma forzosamente a Latin-1/Bytes para reparar la Ñ rota
     x <- iconv(x, from = "UTF-8", to = "Latin1", sub = "")
     # 3. Lo devuelve a UTF-8 nativo limpio y válido para Highcharts
     x <- iconv(x, from = "Latin1", to = "UTF-8", sub = "")
     # 4. Asegura que R declare internamente la codificación como UTF-8
     enc2utf8(x)
   }))
 
 
 # 2. Agrupación de datos
 GRUPEDAD_IRAG <- data_irag_limpio %>%
   group_by(EDAD_UC_IRAG, SEXO) %>% 
   summarise(CASOS = n(), .groups = "drop")
 
 # 3. ORDENAR Y CONVERTIR EN FACTOR (Clave para Highcharts)
 GRUPEDAD_IRAG <- GRUPEDAD_IRAG %>%
   # Primero calculamos el peso numérico para ordenar de MENOR a MAYOR meses/años
   mutate(
     num = as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")),
     es_ano = str_detect(EDAD_UC_IRAG, "(?i)a\u00f1o|ano"), # Detecta año u año codificado
     peso = num * ifelse(es_ano, 12, 1)
   ) %>%
   arrange(peso) %>%
   # PASO CRUCIAL: Transformar el texto en factor respetando este orden exacto
   mutate(EDAD_UC_IRAG = factor(EDAD_UC_IRAG, levels = unique(EDAD_UC_IRAG))) %>%
   select(-num, -es_ano, -peso)
 
 # 4. Pasar a formato ancho (Mantiene los niveles del factor creados)
 GRUPEDAD_IRAG_WIDE <- GRUPEDAD_IRAG %>%
   pivot_wider(names_from = SEXO, values_from = CASOS, values_fill = 0)
 
 # 5. Configuración del gráfico Highcharts
 curva_irag_sex <- highchart() %>%
   hc_chart(type = "bar") %>% 
   hc_plotOptions(bar = list(stacking = NULL, 
                             pointPadding = 0.05,   
                             groupPadding = 0.1,  
                             borderWidth = 0)) %>%
   hc_xAxis(
     categories = levels(GRUPEDAD_IRAG_WIDE$EDAD_UC_IRAG), # Extrae las categorías en orden
     title = list(text = "Grupo etario")) %>%  
   hc_yAxis(title = list(text = "Casos notificados")) %>%
   hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
              enabled = TRUE) %>% 
   hc_add_series(
     data = GRUPEDAD_IRAG_WIDE$F,
     name = "Femenino",
     color = "#f9a58c") %>%
   hc_add_series(
     data = GRUPEDAD_IRAG_WIDE$M,
     name = "Masculino",
     color = "#9bc4e2")
 
 # Llamar al gráfico
 curva_irag_sex
 