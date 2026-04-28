
 VIRUS_SE_TABLA<-data |>
   group_by(FLU,VRS, SARS_COV_2,SEPI_FECHA_INTER)%>%
  summarise(CASOS = n())%>%
   ungroup()
 
 #Completo tabla con las SE donde no hubo casos notificados. El número de casos se completa con 0
 
  VIRUS_SE_TABLA <-  VIRUS_SE_TABLA %>% 
   complete(SEPI_FECHA_INTER= 1:53,
            fill = list (n= 0))
 
 # Creo la variable SE para utilizar como etiqueta del eje x.Se 
 # normaliza la escritura para que todas las SE estén compuestas por 2 dígitos
 
  VIRUS_SE_TABLA <-  VIRUS_SE_TABLA %>% mutate(SE = str_pad(SEPI_FECHA_INTER, #variable a normalizar
                                                    width = 2, #cantidad de dígitos
                                                    side = "left", #posición del número que se utilizará para "completar"
                                                    pad = "0")) #número que se utilizará para "completar"
 

  # 1. Procesar los datos para calcular % de positividad
  VIRUS_SE <- VIRUS_SE_TABLA %>%
    # Pasamos a formato largo para manejar los 3 virus a la vez
    pivot_longer(
      cols = c(FLU, VRS, SARS_COV_2), 
      names_to = "Virus", 
      values_to = "Resultado"
    ) %>%
    group_by(SE, Virus) %>%
    summarise(
      # Sumamos las muestras totales de la semana
      Total_Muestras = sum(CASOS, na.rm = TRUE),
      # Sumamos solo donde el resultado sea POSITIVO (ignorando mayúsculas/minúsculas)
      Positivos = sum(CASOS[toupper(Resultado) == "POSITIVO" | Resultado == "SARSCov 2"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Calculamos el porcentaje
    mutate(Porcentaje = round((Positivos / Total_Muestras) * 100, 2))
  
  
  # 2. Crear el gráfico de líneas
  Grafico_virus<- hchart(  VIRUS_SE , "line", hcaes(x = SE, y = Porcentaje, group = Virus)) %>%
    hc_title(text = "Porcentaje de Positividad de Virus Respiratorios por Semana Epidemiológica en UCIRAG. HINEP 2025.") %>%
    hc_yAxis(title = list(text = "Positividad (%)"), min = 0, max = 100) %>%
    hc_xAxis(title = list(text = "Semana Epidemiológica (SE)")) %>%
    hc_tooltip(shared = TRUE, valueSuffix = "%") %>%
    hc_add_theme(hc_theme_smpl())

  ###LLAMO GRAFICO
  Grafico_virus
  