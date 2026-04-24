# 1. Preparamos los datos 
data_grafico <- data %>% 
  group_by(CLASIFICACION_MANUAL) %>%
  summarise(CASOS = n())%>%
  ungroup() 


# 2. CreO el gráfico de TORTA###
torta<-highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(
    data = data_grafico,
    type = "pie",
    hcaes(name = CLASIFICACION_MANUAL, y = CASOS),
    name = "Proporción",
    colorByPoint = TRUE
  ) %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        # Formato: Nombre (Porcentaje sin decimales)
        format = '{point.name}: {point.percentage:.0f}%' 
      )
    )
  ) %>%
  hc_title(text = "Casos de UC IRAG según clasificación. HINEP. Año 2025") %>%
  hc_tooltip(pointFormat = "<b>{point.y}</b> casos<br/>{point.percentage:.1f}%")

torta

