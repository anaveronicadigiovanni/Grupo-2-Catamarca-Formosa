
#########SEVERIDAD #########

Severidad <- data %>% group_by(CLASIFICACION_MANUAL, SEVERIDAD) %>%
  summarise(n = n()) %>%
  ungroup()

tabla_severidad <- Severidad %>% pivot_wider(names_from = SEVERIDAD,
                                            values_from = n)

tabla_severidad <-highchart() %>%
  hc_chart(type= "column") %>%
  hc_title(text = "Casos de IRAG e IRAGe por SE en HINEP.Año 2025")%>%
  hc_plotOptions(column = list(stacking = "normal",
                               pointPadding = 0.1,   
                               groupPadding = 0.05,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = tabla_severidad$CLASIFICACION_MANUAL, #categorías en eje X
    title = list(text = "Categoria")) %>%  #título del eje X) 
  hc_yAxis(title= list(text="Casos notificados")) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = tabla_severidad$`Sin Oxígeno`,
    name = "Sin Oxígeno",
    color = "#006854") %>%
  hc_add_series(
    data = tabla_severidad$`Oxígeno Bajo Flujo`,
    name = "Oxigeno Bajo Flujo",
    color = "#f7e859") %>%
   hc_add_series(
    data = tabla_severidad$`CAFO y/o VM`,
    name = "CAFO y/o VM",
    color = "#cc0c00") 
  
tabla_severidad

# 1. Asegurate de que los NA sean 0 para que las columnas se vean bien
tabla_severidad <- Severidad %>% 
  pivot_wider(names_from = SEVERIDAD, values_from = n, values_fill = 0)

# 2. Construcción del gráfico
severidad_grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Severidad según Requerimiento de O2 de Casos IRAG/IRAGe. Año 2025") %>%
  hc_plotOptions(column = list(
    stacking = "percent", # <--- Cambia "normal" por "percent"
    pointPadding = 0.1,   
    groupPadding = 0.05,  
    borderWidth = 0)) %>%
  hc_xAxis(categories = tabla_severidad$CLASIFICACION_MANUAL) %>%
  hc_yAxis(title = list(text = "Porcentaje (%)"), max = 100) %>%
  # Configuración para ver el % al pasar el mouse
  hc_tooltip(pointFormat = "{series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>",
             shared = TRUE) %>% 
  hc_credits(text = "Fuente: SNVS 2.0", enabled = TRUE) %>% 
  hc_add_series(data = tabla_severidad$`Sin Oxígeno`, name = "Sin Oxígeno", color = "#93c6e0") %>%
  hc_add_series(data = tabla_severidad$`Oxigeno Bajo Flujo`, name = "Oxigeno Bajo Flujo", color = "#ffb777") %>%
  hc_add_series(data = tabla_severidad$`CAFO y/o VM`, name = "CAFO y/o VM", color = "#fc4f59")


# Mostrar gráfico
severidad_grafico
