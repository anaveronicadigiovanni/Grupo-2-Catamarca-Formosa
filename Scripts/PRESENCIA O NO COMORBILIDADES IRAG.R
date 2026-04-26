##########presencia comorbilidades IRAG3#

##1- Creo la tabla#
PRES_COMORB<-data_irag |>
  group_by(PRESENCIA_COMORBILIDADES)%>% 
  summarise(CASOS = n())%>%
  ungroup()

# 2. CreO el gráfico de TORTA###
torta_comorbilidades_IRAG<-highchart() %>%
  hc_chart(type = "pie") %>%
  hc_add_series(
    data = PRES_COMORB,
    type = "pie",
    hcaes(name = PRESENCIA_COMORBILIDADES, y = CASOS),
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
  hc_title(text = "Presencia de comorbilidades en los casos de IRAG") %>%
  hc_tooltip(pointFormat = "<b>{point.y}</b> casos<br/>{point.percentage:.1f}%")

torta_comorbilidades_IRAG
