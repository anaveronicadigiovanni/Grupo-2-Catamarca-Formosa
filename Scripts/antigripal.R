###crear columna de analisis de Vac Antigripal

data <- data %>% mutate(VACANTIGRIPAL="NO")

data <-data %>%  
     mutate(VACANTIGRIPAL = if_else( EDAD_UC_IRAG == "6 a 11 Meses"| EDAD_UC_IRAG == "12 a 23 Meses", case_when(
                                    VAC_ANTIGRIPAL == "CONSTATADA" ~ " CONSTATADA",
                                    VAC_ANTIGRIPAL == "NO VACUNADO" ~ " NO VACUNADO",
                                    VAC_ANTIGRIPAL == "SIN DATO" ~ " SIN DATO",
                                    VAC_ANTIGRIPAL == "REFERIDA" ~ " REFERIDA",
                                    TRUE ~ "etc" ), "NO CORRESPONDE"))

data_antigripal <- data %>% 
  group_by(VACANTIGRIPAL) %>%
  summarise(CASOS = n())%>%
  ungroup()

data_edad<- data %>% 
  group_by(EDAD_UC_IRAG) %>%
  summarise(CASOS = n())%>%
  ungroup()
