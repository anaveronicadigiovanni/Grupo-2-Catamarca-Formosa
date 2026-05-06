
# 1. Asumiendo que las columnas de edad empiezan en la 4ta columna CREO UNA COLUMNA PARA LA SUMA DE LOS TOTALES DE CADA FILA
   dfagrupados <- AGRUPADOS %>%
        mutate(Total_Fila = rowSums(select(., 4:ncol(.)), na.rm = TRUE)) %>%
     ###FILTRO POR LAS FILAS Q ME INTERESAN
     filter(NOMBREEVENTOAGRP %in% c("Pacientes internados por todas las causas", 
                                                                      "Casos de IRAG entre los internados", 
                                                                       "Casos de IRAG extendida entre los internados")) %>%
     ###AGRUPO POR SE Y FILAS QUE ELEGÍ
     group_by(SEMANA, NOMBREEVENTOAGRP) %>%
     summarise(Total = sum(Total_Fila, na.rm = TRUE)) %>%
     ungroup()

 # 2. Crear el gráfico de columnas al 100%
  Grafico_virus_columnas<- highchart() %>%
     hc_chart(type = "column") %>%
     hc_plotOptions(column = list(stacking = "percent")) %>% # <--- Esto hace que sumen 100%
    hc_add_series(
        data = dfagrupados, 
        type = "column", 
        hcaes(x = SEMANA, y = Total, group = NOMBREEVENTOAGRP)
      ) %>%
    hc_title(text = "Proporción de IRAG e IRAG Extendida sobre el Total de Internados") %>%
    hc_xAxis(title = list(text = "Semana Epidemiológica")) %>%
   hc_yAxis(title = list(text = "Porcentaje (%)"), max = 100) %>%
    hc_credits(text = enc2utf8("Fuente: Elaboración propia en base a datos del SNVS 2.0"), 
               enabled = TRUE) %>% 
   hc_tooltip(
       shared = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>'
       ) %>%
    hc_add_theme(hc_theme_gridlight())  
  
  # 3. LLAMO AL GRÁFICO
  
  Grafico_virus_columnas
  
  Grafico_virus_columnas <- highchart() %>%
    hc_chart(type = "column") %>%
    hc_plotOptions(column = list(stacking = "percent")) %>% 
    hc_add_series(
      data = dfagrupados, 
      type = "column", 
      hcaes(x = SEMANA, y = Total, group = NOMBREEVENTOAGRP)
    ) %>%
    # Define aquí tus colores (puedes usar nombres o códigos Hex)
    # Cambia el orden según cuál serie quieres que sea gris
    hc_colors(c("#7cb5ec", "#434399","#D3D3D3" )) %>% 
    hc_title(text = "Proporción de IRAG e IRAG Extendida sobre el Total de Internados") %>%
    hc_xAxis(title = list(text = "Semana Epidemiológica")) %>%
    hc_yAxis(title = list(text = "Porcentaje (%)"), max = 100) %>%
    hc_credits(text = enc2utf8("Fuente: Elaboración propia en base a datos del SNVS 2.0"), 
               enabled = TRUE) %>% 
    hc_tooltip(
      shared = TRUE,
      pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>'
    ) %>%
    hc_add_theme(hc_theme_gridlight())
   