# GRAFICO DE TORTA PARA VACUNA ANTIGRIPAL EN NIÑOS DE 6 A 23 MESES
# FILTRO NIÑOS DE 6 A 23 MESES
VACUNA_ANTIGRIPAL<- data %>% filter(EDAD_UC_IRAG %in% c("6 a 11 Meses", "12 a 23 Meses"))



# 1. Preparamos los datos 
data_grafico <- VACUNA_ANTIGRIPAL %>% 
  group_by(VAC_ANTIGRIPAL) %>%
  summarise(CASOS = n())%>%
  ungroup() 

# 2. CreO el gráfico de TORTA2###
torta2<-highchart() %>%
  hc_chart(type = "pie") %>%
  hc_colors(c( "#ffb777", "#fc4f59", "#93c6e0"
  )) %>%
  hc_credits(text = "Fuente: Elaboración propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = data_grafico,
    type = "pie",
    hcaes(name = VAC_ANTIGRIPAL, y = CASOS),
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
  #hc_title(text = "Niños de 6 a 23 meses con vacunación antigripal. HINEP. Año 2025") %>%
  hc_tooltip(pointFormat = "<b>{point.y}</b> casos<br/>{point.percentage:.1f}%")
torta2
