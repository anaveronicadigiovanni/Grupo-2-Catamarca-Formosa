data_irage<- data %>% 
  filter(CLASIFICACION_MANUAL=="IRAG extendida")


library(stringr)

GRUPEDAD_IRAGE<- data_irage %>%
  group_by(EDAD_UC_IRAG)%>% 
  summarise(CASOS = n())%>%
  ungroup()

###ordeno de mnor a mayot edad"
GRUPEDAD_IRAGE <- GRUPEDAD_IRAGE %>%
  arrange(
    # Extraemos el número inicial, lo pasamos a numérico y multiplicamos por 12 si es año
    as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")) * 
      ifelse(str_detect(EDAD_UC_IRAG, "Años"), 12, 1)
  )  
