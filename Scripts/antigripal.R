###crear columna de analisis de Vac Antigripal

data <- data %>% mutate(VACANTIGRIPAL="NO")

data <-data %>%  
     mutate(VACANTIGRIPAL = if_else( EDAD_UC_IRAG == "6 a 11 Meses"| EDAD_UC_IRAG == "12 a 23 Meses", case_when(
                                    VAC_ANTIGRIPAL == "CONSTATADA" ~ " CONSTATADA1",
                                    VAC_ANTIGRIPAL == "NO VACUNADO" ~ " NO VACUNADO",
                                    VAC_ANTIGRIPAL == "SIN DATO" ~ " SIN DATO",
                                    VAC_ANTIGRIPAL == "REFERIDA" ~ " REFERIDA",
                                    TRUE ~ "etc" ), "NO CORRESPONDE"))
