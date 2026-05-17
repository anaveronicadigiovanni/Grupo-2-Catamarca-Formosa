###################HAGO TABLA Q INCLUYA VALORES ABSOLUTOS TAMBIEN############

# 1. Cálculo de valores absolutos y relativos
tabla_comorbilidades_IRAGE <- data_irage |>
  summarise(across(
    c(DIABETES, BAJO_PESO_NACIMIENTO, ASMA, TUBERCULOSIS, PREMATURIDAD,
      ENF_RESPIRATORIA, CARDIOPATIA_CONGENITA, VIH, ASPLENIA, DESNUTRICION, 
      CANCER, TRASPLANTADO, BRONQUIOLITIS_PREVIA, ENF_NEUROLOGICA_CRONICA, 
      ENF_HEPATICA, HIPERTENSION, ENF_CEREBROVASCULAR, ENF_NEUROMUSCULAR, 
      DISCAPACIDAD_INTELECTUAL, ENF_CARDIACA, ENF_REUMATOLOGICA, DBP, 
      ENF_RENAL, OBESIDAD, S_DOWN, INMUNOCOMPROMETIDO_OTRAS_CAUSAS, OTRAS_COMORBILIDADES),
    .fns = list(
      Abs = ~ sum(.x == 1, na.rm = TRUE),
      Rel = ~ round(mean(.x == 1, na.rm = TRUE) * 100, 1) # Un decimal para más precisión
    ),
    .names = "{.col}_{.fn}"
  )) |>
  # Pivotamos usando un patrón para separar la comorbilidad de su métrica (Abs o Rel)
  pivot_longer(
    cols = everything(), 
    names_to = c("Comorbilidades", ".value"), 
    names_pattern = "(.*)_(Abs|Rel)"
  ) |>
  arrange(desc(Abs))

# 2. Limpieza, filtrado y formato de texto
tabla_comorb_irage <- tabla_comorbilidades_IRAGE %>%
  mutate(
    Comorbilidades = str_replace_all(Comorbilidades, "_", " "),
    Comorbilidades = str_to_sentence(Comorbilidades)
  ) %>%
  filter(Abs > 0) # Filtramos las que no tienen casos absolutos

# 3. Generación de la tabla gt con ambas columnas
tabla_comorb_IRAGE <- tabla_comorb_irage %>%
  gt() %>%
  
  # Títulos y subtítulos 
  tab_header(
    title = "Frecuencia de las comorbilidades en IRAGe"
  ) %>%
  
  # Formato numérico para absolutos (agrega separador de miles si es necesario)
  fmt_integer(
    columns = Abs
  ) %>%
  
  # Formato para agregar el símbolo de porcentaje (%) a la columna Relativos
  fmt_number(
    columns = Rel,
    decimals = 1,
    pattern = "{x}%"
  ) %>%
  
  # Renombrar encabezados de columnas para presentación limpia
  cols_label(
    Comorbilidades = "Comorbilidades",
    Abs = "Casos (N)",
    Rel = "Porcentaje (%)"
  ) %>%
  
  # Alinear números a la derecha y texto a la izquierda
  cols_align(
    align = "right",
    columns = c(Abs, Rel)
  ) %>%
  
  # Resaltar con colores los valores más altos basados en el porcentaje
  gt_color_rows(
    columns = Rel, 
    palette = c("#ffffff", "#2c3e90") # Va de blanco a tu azul según el porcentaje
  ) %>%
  
  # Nota al pie de página con la fuente
  tab_source_note(
    source_note = "Fuente: Elaboración propia en base a datos del SNVS 2.0"
  )

# LLAMO TABLA
tabla_comorb_IRAGE



# ###hago tabla de comorbilidades en las IRAGE####
# 
# tabla_comorbilidades_IRAGE <- data_irage  |>
#   summarise(across(c(PREMATURIDAD,BRONQUIOLITIS_PREVIA, ASMA,
#                      CARDIOPATIA_CONGENITA,S_DOWN,DBP,ENF_RESPIRATORIA, 
#                      ENF_NEUROLOGICA_CRONICA, ENF_NEUROMUSCULAR, ENF_RENAL,
#                      ENF_CARDIACA, BAJO_PESO_NACIMIENTO), 
#                    ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
#   pivot_longer(cols = everything(), 
#                names_to = "Comorbilidades", 
#                values_to = "Prevalencia")|>
#   arrange(desc(Prevalencia))
# 
# # 1. Limpieza y filtrado
# tabla_grafico <- tabla_comorbilidades_IRAGE %>%
#   mutate(Comorbilidades = str_replace_all(Comorbilidades, "_", " "),
#          Comorbilidades = str_to_sentence(Comorbilidades)) %>%
#   filter(Prevalencia > 0) # Filtramos las que no tienen casos
# 
# # 2. Generación del gráfico
# highchart() %>%
#   hc_chart(type = "bar") %>% 
#   hc_title(text = "Prevalencia de Comorbilidades en Casos de IRAGe") %>%
#   hc_subtitle(text = "Factores de riesgo presentes en pacientes notificados") %>%
#   hc_xAxis(categories = tabla_grafico$Comorbilidades,
#            title = list(text = NULL)) %>% 
#   hc_yAxis(title = list(text = "Prevalencia (%)"),
#            labels = list(format = "{value}%"),
#            max = 100) %>%
#   hc_add_series(
#     data = tabla_grafico$Prevalencia,
#     name = "Porcentaje",
#     color = "#2c3e50", # Un azul oscuro elegante
#     showInLegend = FALSE
#   ) %>%
#   hc_plotOptions(bar = list(
#     dataLabels = list(enabled = TRUE, format = "{point.y}%"),
#     pointPadding = 0.1,
#     groupPadding = 0.1,
#     borderWidth = 0
#   )) %>%
#   hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia en base a datos del SNVS 2.0")
