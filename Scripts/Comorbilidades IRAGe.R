###hago tabla de comorbilidades en las IRAGE####

tabla_comorbilidades_IRAGE <- data_irage  |>
  summarise(across(c(PREMATURIDAD,BRONQUIOLITIS_PREVIA, ASMA,
                     CARDIOPATIA_CONGENITA,S_DOWN,DBP,ENF_RESPIRATORIA, 
                     ENF_NEUROLOGICA_CRONICA, ENF_NEUROMUSCULAR, ENF_RENAL,
                     ENF_CARDIACA, BAJO_PESO_NACIMIENTO), 
                   ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
  pivot_longer(cols = everything(), 
               names_to = "Comorbilidades", 
               values_to = "Prevalencia")|>
  arrange(desc(Prevalencia))

# 1. Limpieza y filtrado
tabla_grafico <- tabla_comorbilidades_IRAGE %>%
  mutate(Comorbilidades = str_replace_all(Comorbilidades, "_", " "),
         Comorbilidades = str_to_sentence(Comorbilidades)) %>%
  filter(Prevalencia > 0) # Filtramos las que no tienen casos

# 2. Generación del gráfico
highchart() %>%
  hc_chart(type = "bar") %>% 
  hc_title(text = "Prevalencia de Comorbilidades en Casos de IRAGe") %>%
  hc_subtitle(text = "Factores de riesgo presentes en pacientes notificados") %>%
  hc_xAxis(categories = tabla_grafico$Comorbilidades,
           title = list(text = NULL)) %>% 
  hc_yAxis(title = list(text = "Prevalencia (%)"),
           labels = list(format = "{value}%"),
           max = 100) %>%
  hc_add_series(
    data = tabla_grafico$Prevalencia,
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
