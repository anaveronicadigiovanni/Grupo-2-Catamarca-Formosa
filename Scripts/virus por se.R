
 VIRUS_SE_TABLA<-data |>
   group_by(FLU,VRS, SARS_COV_2,SEPI_FECHA_INTER)%>%
  summarise(CASOS = n())%>%
   ungroup()

 # 1. Preparar los datos: pasar de formato ancho a largo
 VIRUS_SE <- data %>%
   pivot_longer(
     cols = c(FLU, VRS, SARS_COV_2), 
     names_to = "Virus", 
     values_to = "Resultado"
   ) %>%
   # Filtramos para no graficar los "Negativos" o vacíos si solo querés ver positivos
   filter(!is.na(Resultado), Resultado != "Negativo", Resultado != "") %>%
   group_by(SEPI_FECHA_INTER, Virus) %>%
   summarise(Total_Casos = sum(CASOS, na.rm = TRUE), .groups = "drop")
 
 # 2. Crear el gráfico de columnas apiladas
GRAFICO_VIRUS_SE<- hchart(data_grafico, "column", hcaes(x = SEPI_FECHA_INTER, y = Total_Casos, group = Virus)) %>%
   hc_plotOptions(column = list(stacking = "normal")) %>%
   hc_title(text = "Casos por Semana Epidemiológica") %>%
   hc_xAxis(title = list(text = "Semana")) %>%
   hc_yAxis(title = list(text = "Número de Casos")) %>%
   hc_add_theme(hc_theme_smpl())