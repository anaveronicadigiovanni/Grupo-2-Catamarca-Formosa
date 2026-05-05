
#  # Asumiendo que las columnas de edad empiezan en la 4ta columna
   dfagrupados <- AGRUPADOS %>%
        mutate(Total_Fila = rowSums(select(., 4:ncol(.)), na.rm = TRUE)) %>%
     filter(NOMBREEVENTOAGRP %in% c("Pacientes internados por todas las causas", 
                                                                      "Casos de IRAG entre los internados", 
                                                                       "Casos de IRAG extendida entre los internados")) %>%
     group_by(SEMANA, NOMBREEVENTOAGRP) %>%
     summarise(Total = sum(Total_Fila, na.rm = TRUE)) %>%
     ungroup()

 # 2. Crear el gráfico de columnas al 100%
   highchart() %>%
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
   hc_tooltip(
       shared = TRUE,
        pointFormat = '<span style="color:{point.color}">\u25CF</span> {series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>'
       ) %>%
     hc_add_theme(hc_theme_elementary())
   