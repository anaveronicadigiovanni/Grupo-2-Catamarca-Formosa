#####Diagnosticos en IRAG, HAgo tabla###


tabla_DX_IRAG <- data_irag |>
  summarise(across(c(DIAG_NEUMONIA,DIAG_BRONQUIOLITIS,DIAG_SHOCK_SEPTICO,DIAG_SEPSIS,OTRO_DX), 
                   ~ round (mean(.x == 1, na.rm = TRUE) * 100))) |>
  pivot_longer(cols = everything(), 
               names_to = "Diagnóstico", 
               values_to = "Prevalencia")|>
  arrange(desc(Prevalencia))


################################################

###Hago tabla linda##


# 1. Limpieza y filtrado 
tabla_grafico_DX <- tabla_DX_IRAG %>%
  mutate(Diagnóstico = str_replace_all(Diagnóstico, "_", " "),
         Diagnóstico = str_to_sentence(Diagnóstico)) %>%
  # CAMBIO DE NOMBRES AQUÍ: "Nombre nuevo" = "Nombre viejo exacto"
  mutate(Diagnóstico = forcats::fct_recode(Diagnóstico,
                                           "Neumonía"         = "Diag neumonia",
                                           "Bronquiolitis"    = "Diag bronquiolitis",
                                           "Shock Séptico"    = "Diag shock septico",
                                           "Sepsis"           = "Diag sepsis",
                                           "Otros diagnósticos" = "Otro dx"
  )) %>%
  filter(Prevalencia > 0)
# 2. Generación de la tabla con colores en lo más frecuente
tabla_dx_IRAG <- tabla_grafico_DX %>%
  gt() %>%
  
  # Títulos y subtítulos 
  tab_header(
    title = "Diagnósticos al ingreso en IRAG"
  ) %>%
  
  # Formato para agregar el símbolo de porcentaje (%) a la columna Prevalencia
  fmt_number(
    columns = Prevalencia,
    decimals = 0,
    pattern = "{x}%"
  ) %>%
  
  # Renombrar encabezados de columnas para presentación limpia
  cols_label(
    Diagnóstico = "Diagnóstico",
    Prevalencia = "Prevalencia"
  ) %>%
  
  # Resaltar con colores los valores más altos (Usa el azul #2c3e90 de tu gráfico)
  gt_color_rows(
    columns = Prevalencia, 
    palette = c("#ffffff", "#2c3e90") # Va de blanco a tu azul según la frecuencia
  ) %>%
  
  # Nota al pie de página con la fuente
  tab_source_note(
    source_note = "Fuente: Elaboración propia en base a datos del SNVS 2.0"
  )

# LLAMO TABLA
tabla_dx_IRAG

#############################
##GRAFICO DE BARRAS#

# # 1. Limpieza y filtrado
# tabla_grafico_DX <- tabla_DX_IRAG %>%
#   mutate(Diagnóstico = str_replace_all(Diagnóstico, "_", " "),
#          Diagnóstico = str_to_sentence(Diagnóstico)) %>%
#   filter(Prevalencia > 0) # Filtramos las que no tienen casos
# 
# # 2. Generación del gráfico
# grafico_dx_IRAG <- highchart() %>%
#   hc_chart(type = "bar") %>% 
#   hc_title(text = "Diagnósticos al ingreso en IRAG") %>%
#   hc_subtitle(text = "") %>%
#   hc_xAxis(categories = tabla_grafico_DX$Diagnóstico,
#            title = list(text = NULL)) %>% 
#   hc_yAxis(title = list(text = "Prevalencia (%)"),
#            labels = list(format = "{value}%"),
#            max = 100) %>%
#   hc_add_series(
#     data = tabla_grafico_DX$Prevalencia,
#     name = "Porcentaje",
#     color = "#2c3e90", # azul
#     showInLegend = FALSE
#   ) %>%
#   hc_plotOptions(bar = list(
#     dataLabels = list(enabled = TRUE, format = "{point.y}%"),
#     pointPadding = 0.1,
#     groupPadding = 0.1,
#     borderWidth = 0
#   )) %>%
#   hc_credits(enabled = TRUE, text = "Fuente: Elaboración propia en base a datos del SNVS 2.0")
# 
# ##LLAMO GRAFICO
# 
#grafico_dx_IRAG
