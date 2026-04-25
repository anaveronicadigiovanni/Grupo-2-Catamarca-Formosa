###Creo dataframe para grafico de IRAG e IRAGe por SE###


curva_casos <- data %>% group_by(CLASIFICACION_MANUAL,SEPI_MIN_INTERNACION) %>%
  summarise(n = n()) %>%
  ungroup()

#Completo tabla con las SE donde no hubo casos notificados. El número de casos se completa con 0

curva_casos <- curva_epidemiologica_casos %>% 
  complete(SEPI_MIN_INTERNACION = 1:53,
           fill = list (n= 0))

# Creo la variable SE para utilizar como etiqueta del eje x.Se 
# normaliza la escritura para que todas las SE estén compuestas por 2 dígitos

curva_casos <- curva_epidemiologica_casos %>% mutate(SE = str_pad(SEPI_MIN_INTERNACION, #variable a normalizar
                                                                                 width = 2, #cantidad de dígitos
                                                                                 side = "left", #posición del número que se utilizará para "completar"
                                                                                 pad = "0")) #número que se utilizará para "completar"


curva_widers <- curva_casos %>% pivot_wider(names_from = CLASIFICACION_MANUAL,
                                                          values_from = n)

curva_casos_interactiva <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(text = "Casos de IRAG e IRAGe por SE en HINEP.Año 2025")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = curva_wider$SE, #categorías en eje X
    title = list(text = "Semana epidemiológica")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = curva_wider$`Infección respiratoria aguda grave (IRAG)`,
    name = "IRAG",
    color = "#112151") %>%
  hc_add_series(
    data = curva_wider$`IRAG extendida`,
    name = "IRAGe",
    color = "#006854") 

curva_casos_interactiva
