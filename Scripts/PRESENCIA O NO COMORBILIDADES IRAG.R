##########Presencia comorbilidades IRAG#

##1- Creo la tabla#
PRES_COMORB<-data_irag |>
  group_by(PRESENCIA_COMORBILIDADES_texto)%>% 
  summarise(CASOS = n())%>%
  ungroup()

comorb_irag<-PRES_COMORB %>%
  filter(PRESENCIA_COMORBILIDADES_texto == "Con comorbilidades") %>%
  pull(CASOS)

# Asignar los colores específicos a cada categoría en tus datos

PRES_COMORB_COLORES <- PRES_COMORB %>%
  mutate(color = case_when(
    PRESENCIA_COMORBILIDADES_texto == "Con comorbilidades" ~ "#ffb777",
    PRESENCIA_COMORBILIDADES_texto == "Sin comorbilidades" ~ "#93c6e0",
    PRESENCIA_COMORBILIDADES_texto == "Sin datos"           ~ "#a6a6a6", # Gris medio
    TRUE ~ "#d3d3d3" # Gris claro por si hay otra categoría inesperada
  ))

# 2. Creo el gráfico de TORTA con el mapeo de color
torta_comorbilidades_IRAG <- highchart() %>%
  hc_chart(type = "pie") %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = PRES_COMORB_COLORES, # Usamos el dataset con la nueva columna 'color'
    type = "pie",
    hcaes(name = PRESENCIA_COMORBILIDADES_texto, y = CASOS, color = color), # Mapeamos 'color = color'
    name = "Proporción",
    colorByPoint = TRUE
  ) %>%
  hc_plotOptions(
    pie = list(
      dataLabels = list(
        enabled = TRUE,
        format = '{point.name}: {point.percentage:.0f}%' 
      )
    )
  ) %>%
#  hc_title(text = "Presencia de comorbilidades en los casos de IRAG") %>%
  hc_tooltip(pointFormat = "<b>{point.y}</b> casos<br/>{point.percentage:.1f}%")


# LLAMO GRAFICO
torta_comorbilidades_IRAG


