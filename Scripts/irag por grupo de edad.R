data_irag<- data %>% 
  filter(CLASIFICACION_MANUAL=="Infección respiratoria aguda grave (IRAG)")


library(stringr)

  GRUPEDAD_IRAG<- data_irag %>%
  group_by(EDAD_UC_IRAG)%>% 
  summarise(CASOS = n())%>%
ungroup()
  
  ###ordeno de mnor a mayot edad"
 GRUPEDAD_IRAG <- GRUPEDAD_IRAG %>%
 arrange(
   # Extraemos el número inicial, lo pasamos a numérico y multiplicamos por 12 si es año
   as.numeric(str_extract(EDAD_UC_IRAG, "\\d+")) * 
     ifelse(str_detect(EDAD_UC_IRAG, "Años"), 12, 1)
 )  

  