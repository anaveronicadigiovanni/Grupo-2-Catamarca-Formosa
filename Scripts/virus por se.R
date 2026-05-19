#####AGRUPAMOS por virus y Semana############

 VIRUS_SE_TABLA<-data |>
   group_by(FLU,VRS, SARS_COV_2,SEPI_FECHA_INTER)%>%
  summarise(CASOS = n())%>%
   ungroup()
 
 #Completo tabla con las SE donde no hubo casos notificados. El número de casos se completa con 0
 
  VIRUS_SE_TABLA <-  VIRUS_SE_TABLA %>% 
   complete(SEPI_FECHA_INTER= 1:53,
            fill = list (n= 0))
 
 # Creo la variable SE para utilizar como etiqueta del eje x (SE)
 # normaliza la escritura para que todas las SE estén compuestas por 2 dígitos
 
  VIRUS_SE_TABLA <-  VIRUS_SE_TABLA %>% mutate(SE = str_pad(SEPI_FECHA_INTER, #variable a normalizar
                                                    width = 2, #cantidad de dígitos
                                                    side = "left", #posición del número que se utilizará para "completar"
                                                    pad = "0")) #número que se utilizará para "completar"
 

  # # 1. Procesar los datos para calcular % de positividad
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


  #2. Crear el gráfico de líneas
  Grafico_virus<- hchart(  VIRUS_SE , "line", hcaes(x = SE, y = Porcentaje, group = Virus)) %>%
    hc_title(text = "Porcentaje de Positividad de Virus Respiratorios por Semana Epidemiológica en UCIRAG. HINEP 2025.") %>%
    hc_yAxis(title = list(text = "Positividad (%)"), min = 0, max = 100) %>%
    hc_xAxis(title = list(text = "Semana Epidemiológica (SE)")) %>%
    hc_tooltip(shared = TRUE, valueSuffix = "%") %>%
    hc_add_theme(hc_theme_smpl())

  ###LLAMO GRAFICO
  Grafico_virus
  
  
  ############################################################################  
  
  
  ###Grafico de columnas apiladas


 
  
  # #Agrupamos y sumamos por Semana Epidemiológica (SEPI_FECHA_INT)
  # df_columnas_virus <- data %>%
  #   mutate(SE = str_pad(SEPI_FECHA_INTER, #variable a normalizar
  #                       width = 2, #cantidad de dígitos
  #                       side = "left", #posición del número que se utilizará para "completar"
  #                       pad = "0"))|> #número que se utilizará para "completar" |>
  #  
  #   group_by(SE)%>%
  #   summarise(
  #     Negativos = sum(NEGATIVOS, na.rm = TRUE),
  #    SARS_CoV_2 = sum(COVID, na.rm = TRUE),
  #     Influenza = sum(Influenza, na.rm = TRUE),
  #     VSR= sum(VSR, na.rm = TRUE))|>
      #mutate(SE = str_pad(SEPI_FECHA_INTER, #variable a normalizar
   #                       width = 2, #cantidad de dígitos
   #                       side = "left", #posición del número que se utilizará para "completar"
   #                       pad = "0")) #número que se utilizará para "completar"
   # ) %>%
    # # Pasamos a formato largo para Highcharts
    # pivot_longer(
    #   cols = -SE, 
    #   names_to = "Virus", 
    #   values_to = "Casos"
    # )
    # 
    # 
  # # 2. Creamos el gráfico de columnas apiladas
  # hchart(
  #   df_columnas_virus, 
  #   "column", 
  #   hcaes(x = SE, y = Casos, group = Virus)) %>%
  #   hc_plotOptions(column = list(stacking = "normal")) %>%
  #   hc_title(text = "Resultados de muestras para PCR por Semana Epidemiológica. HINEP. 2025") %>%
  #   hc_xAxis(title = list(text = "Semana Epidemiológica")) %>%
  #   hc_yAxis(title = list(text = "Total de Muestras / Casos"))|>
  # hc_colors(c("#E74C3C", "#3498DB", "#95A5A0", "#2ECC71"))
   
 #####################################
  
  #En otro orden de apilado############
  
  # 1. Agrupamos y sumamos
  df_columnas_virus <- data %>%
    mutate(SE = str_pad(SEPI_FECHA_INTER, width = 2, side = "left", pad = "0")) %>%
    group_by(SE) %>%
    summarise(
      Negativos = sum(NEGATIVOS, na.rm = TRUE),
      Influenza = sum(Influenza, na.rm = TRUE),
     SARS_CoV_2 = sum(SARS_CoV_2, na.rm = TRUE),
      VSR = sum(VSR, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = -SE, 
      names_to = "Virus", 
      values_to = "Casos"
    ) %>%
    # CLAVE: Definir el orden. El primero (Negativos) va abajo.
    mutate(Virus = factor(Virus, levels = c("Negativos", "Influenza", "SARS_CoV_2", "VSR")))|>
  # Reemplaza los guiones bajos por espacios en los nombres que verá Highcharts
     mutate(Virus = factor(gsub("_", " ", Virus), levels = c("Negativos", "Influenza", "SARS CoV 2", "VSR")))
  
  # 2. Creamos el gráfico con los colores corregidos
  grafico_virus<-hchart(
    df_columnas_virus, 
    "column", 
    hcaes(x = SE, y = Casos, group = Virus)
  ) %>%
    hc_plotOptions(column = list(stacking = "normal")) %>%
    hc_colors(c("#D3D3D3", "#2ECC71", "#3498DB", "#FFB347")) %>% # Gris, Verde, Celeste, Naranja
 #   hc_title(text = "Resultados de muestras para PCR por Semana Epidemiológica. HINEP. 2025") %>%
    hc_xAxis(title = list(text = "Semana Epidemiológica")) %>%
    hc_yAxis(title = list(text = "Total de determinaciones"))
  
 ## LLAMO GRAFICO
  
  grafico_virus
  