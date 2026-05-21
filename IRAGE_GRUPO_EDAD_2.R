#####CÓDIGO PARA NO TENER PROBLEMAS CON Ñ o ACENTOS

data_irage_limpio <- data_irage %>%
  mutate(across(where(is.character), ~ {
    # Convierte a UTF-8 nativo y escapa los elementos no válidos
    x_enc <- iconv(.x, to = "UTF-8", sub = " ")
    # Codificación estricta para asegurar compatibilidad HTML/JSON
    utf8::utf8_encode(x_enc)
  }))



# 1. Agrupar por edad y sexo
GRUPEDAD_IRAGE <- data_irage_limpio %>%
  group_by(EDAD_UC_IRAG, SEXO) %>% 
  summarise(CASOS = n(), .groups = "drop")

# 2. Ordenar usando encodificacio'n segura y pivotar
GRUPEDAD_IRAGE <- GRUPEDAD_IRAGE %>%
  arrange(
    as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")) * 
      ifelse(str_detect(iconv(EDAD_UC_IRAG, to = "UTF-8"), "A\u00f1os"), 12, 1)
  ) %>%
  pivot_wider(names_from = SEXO, values_from = CASOS, values_fill = 0)

# 3. Generar el gra'fico con strings limpios
curva_irage_sex <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Distribucion de Casos Notificados de IRAGe por Sexo y Grupo Etario") %>% 
  hc_plotOptions(column = list(stacking = NULL,
                               pointPadding = 0.05,   
                               groupPadding = 0.1,  
                               borderWidth = 0)) %>%
  hc_xAxis(
    categories = GRUPEDAD_IRAGE$EDAD_UC_IRAG,
    title = list(text = "Grupo etario")) %>%
  hc_yAxis(title = list(text = "Casos notificados")) %>%
  hc_credits(text = "Fuente: Elaboracion propia en base a datos del SNVS 2.0", 
             enabled = TRUE) %>% 
  hc_add_series(
    data = GRUPEDAD_IRAGE$F,
    name = "Femenino",
    color = "#f9a58c") %>%
  hc_add_series(
    data = GRUPEDAD_IRAGE$M,
    name = "Masculino",
    color = "#9bc4e2") 

# 4. Mostrar gra'fico
curva_irage_sex
