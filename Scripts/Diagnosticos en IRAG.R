#####Diagnosticos en IRAG, HAgo tabla###

tabla_DX_IRAG <- data_irag |>
  summarise(across(c(DIAG_NEUMONIA,DIAG_BRONQUIOLITIS,DIAG_SHOCK_SEPTICO,DIAG_SEPSIS,OTRO_DX), 
                   ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
  pivot_longer(cols = everything(), 
               names_to = "Diagnóstico", 
               values_to = "Prevalencia")|>
  arrange(desc(Prevalencia))

# 1. Limpieza y filtrado
tabla_grafico_DX <- tabla_DX_IRAG %>%
  mutate(Diagnóstico = str_replace_all(Diagnóstico, "_", " "),
         Diagnóstico = str_to_sentence(Diagnóstico)) %>%
  filter(Prevalencia > 0) # Filtramos las que no tienen casos

# 2. Generación del gráfico
highchart() %>%
  hc_chart(type = "bar") %>% 
  hc_title(text = "Diagnósticos en Casos de IRAG") %>%
  hc_subtitle(text = "") %>%
  hc_xAxis(categories = tabla_grafico_DX$Diagnóstico,
           title = list(text = NULL)) %>% 
  hc_yAxis(title = list(text = "Prevalencia (%)"),
           labels = list(format = "{value}%"),
           max = 100) %>%
  hc_add_series(
    data = tabla_grafico_DX$Prevalencia,
    name = "Porcentaje",
    color = "#2c3e50", # Un azul oscuro elegante
    showInLegend = FALSE
  ) %>%
  hc_plotOptions(bar = list(
    dataLabels = list(enabled = TRUE, format = "{point.y}%"),
    pointPadding = 0.1,
    groupPadding = 0.1,
    borderWidth = 0
  )) %>%
  hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia en base a datos del SNVS 2.0")
