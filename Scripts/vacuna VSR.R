# FILTRO NIÑOS DE 0 A 5 MESES


VACUNA_VSR <- data %>% filter(EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses"))

# 1. Preparamos los datos 
data_vac_vsr <- VACUNA_VSR %>% 
  group_by(VACUNA_VRS, CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n())%>%
  ungroup() 

data_vac_vsr<-data_vac_vsr%>%
  pivot_wider(names_from = VACUNA_VRS , values_from = CASOS, values_fill = 0)

####GRAFICO DE BARRAS APILADAS COMPARATIVO
# 2. Construcción del gráf
vsr_grafico <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Vacuna para VSR materna en Casos IRAG/IRAGe. Año 2025") %>%
  hc_plotOptions(column = list(
    stacking = "percent", # <--- Cambia "normal" por "percent"
    pointPadding = 0.1,   
    groupPadding = 0.05,  
    borderWidth = 0)) %>%
  hc_xAxis(categories = data_vac_vsr$CLASIFICACION_MANUAL) %>%
  hc_yAxis(title = list(text = "Porcentaje (%)"), max = 100) %>%
  # Configuración para ver el % al pasar el mouse
  hc_tooltip(pointFormat = "{series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>",
             shared = TRUE) %>% 
  hc_credits(text = "Fuente: SNVS 2.0", enabled = TRUE) %>% 
  hc_add_series(data = data_vac_vsr$`Sin dato`, name = "Sin dato", color = "#d3d1aa") %>%
  hc_add_series(data = data_vac_vsr$`madre vacunada`, name = "Madre vacunada", color = "#60c659") %>%
  hc_add_series(data = data_vac_vsr$`madre NO vacunada`, name = "Madre NO vacunada", color = "#fc4f59")


# Mostrar gráfico
vsr_grafico

