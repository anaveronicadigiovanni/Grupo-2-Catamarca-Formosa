# Grafico  vacunas maternas
#1 filtro niños menores de 6 meses
NIÑOS_MENORES_6_MESES<- data %>% filter(EDAD_UC_IRAG %in% c("0 a 2 Meses", "3 a 5 Meses"))


data <- data %>%
  mutate(VAC_ANTIG_MATERNA = case_match(VAC_ANTIGRIPAL_MATERNA,
                                        
                                        "CONSTATADA"~ "madre vacunada",
                                        "REFERIDA"~ "madre vacunada",
                                        "SIN DATO" ~ "Sin dato",
                                        "MADRE NO VACUNADA"~ "madre no vacunada",
                                        .default = NA_character_ ))
library (purrr)


#2 Creo data frame para grafico de madres vacunadas en menores de 6 meses###


tabla_grafico <- NIÑOS_MENORES_6_MESES %>% group_by(VACUNA_VRS,VAC_ANTIG_MATERNA) %>%
  summarise(n = n()) %>%
  ungroup()

# 1. Preparamos los datos con limpieza de texto
tabla_grafico <- NIÑOS_MENORES_6_MESES %>%
  select(VACUNA_VRS, VAC_ANTIG_MATERNA) %>% 
  pivot_longer(cols = everything(), names_to = "Vacuna", values_to = "Estado") %>%
  # Limpieza: quitamos espacios extras y pasamos a minúsculas para comparar fácil
  mutate(Estado = trimws(tolower(Estado))) %>% 
  # Si hay NAs o está vacío, le ponemos "sin dato"
  mutate(Estado = ifelse(is.na(Estado) | Estado == "", "sin dato", Estado)) %>% 
  group_by(Vacuna, Estado) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(Vacuna = case_when(
    Vacuna == "VACUNA_VRS" ~ "Vacuna VRS",
    Vacuna == "VAC_ANTIG_MATERNA" ~ "Vacuna Antigripal",
    TRUE ~ Vacuna
  ))

# 2. Armamos el gráfico
vacunas_madres_grafico<- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Vacunación Materna en niños menores de 6 meses") %>%
  hc_plotOptions(column = list(
    stacking = "percent", 
    borderWidth = 0,
    dataLabels = list(enabled = TRUE, format = "{point.percentage:.1f}%")
  )) %>%
  hc_xAxis(categories = unique(tabla_grafico$Vacuna)) %>%
  hc_yAxis(title = list(text = "Porcentaje (%)"), max = 100) %>%
  hc_tooltip(shared = TRUE, pointFormat = "{series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>") %>%
  # Configuración para ver el % al pasar el mouse
  hc_tooltip(pointFormat = "{series.name}: <b>{point.percentage:.1f}%</b> ({point.y} casos)<br/>",
             shared = TRUE) %>% 
  # Serie: Madre Vacunada
  hc_add_series(
    data = (tabla_grafico %>% filter(Estado == "madre vacunada"))$n, 
    name = "Madre Vacunada", color = "#93c6e0"
  ) %>%
  
  # Serie: Madre NO Vacunada (Asegúrate que el filtro coincida con el tolower de arriba)
  hc_add_series(
    data = (tabla_grafico %>% filter(Estado == "madre no vacunada"))$n, 
    name = "Madre No Vacunada", color = "#ffb777"
  ) %>%
  
  # Serie: Sin dato
  hc_add_series(
    data = (tabla_grafico %>% filter(Estado == "sin dato"))$n, 
    name = "Sin Dato", color = "#fc4f59"
  )
#ver grafic

vacunas_madres_grafico
