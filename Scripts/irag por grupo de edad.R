data_irag<- data %>% 
  filter(CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)")




  GRUPEDAD_IRAG<- data_irag %>%
  group_by(EDAD_UC_IRAG, SEXO)%>% 
  summarise(CASOS = n())%>%
ungroup()
  
  ###ordeno de mnor a mayot edad"
 GRUPEDAD_IRAG <- GRUPEDAD_IRAG %>%
 arrange(
   # Extraemos el número inicial, lo pasamos a numérico y multiplicamos por 12 si es año
   as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")) * 
     ifelse(str_detect(EDAD_UC_IRAG, "Años"), 12, 1)
 )%>%
   #tabla a lo ancho####
 pivot_wider(names_from = SEXO, values_from = CASOS)
 
 ###Grafico columnas agrupadas
 
 curva_irag_sex <-highchart() %>%
   hc_chart(type= "column") %>%
   hc_title(text = "Distribución de Casos Notificados de IRAG por Sexo y Grupo Etario") %>% 
   hc_plotOptions(column = list(stacking = NULL,
                                pointPadding = 0.05,   
                                groupPadding = 0.1,  
                                borderWidth = 0)) %>%
   hc_xAxis(
     categories =GRUPEDAD_IRAG$EDAD_UC_IRAG, #categorías en eje X
     title = list(text = "Grupo etario")) %>%  #título del eje X) 
   hc_yAxis(title= list(text="Casos notificados")) %>%
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
 
 curva_irag_sex
 
 

  