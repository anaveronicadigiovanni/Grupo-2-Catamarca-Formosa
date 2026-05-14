# --- SCRIPT PRINCIPAL (Main / Index) --- (YA FUNCIONA!)

# 1. Configuración inicial y carga de datos
source("Scripts/Library.R")
source("Scripts/Importar base.R", encoding = "UTF-8")

# 2. Procesamiento de datos y filtros
source("Scripts/Crear variables nuevas.R")    
source("Scripts/Criterios de exclusión.R", encoding = "UTF-8")

# 3. Análisis de Casos y Semanas (SE)
source("Scripts/Casos IRAG e IRAGe po SE.R")
source("Scripts/total casos.R")
source("Scripts/total IRAG.R")
source("Scripts/total IRAGe.R")
source("Scripts/Torta IRAG-IRAGe.R")
source("Scripts/virus por se.R")

# 4. Comorbilidades y Severidad
source("Scripts/PRESENCIA O NO COMORBILIDADES IRAG.R")
source("Scripts/PRESENCIA O NO COMORBILIDADES IRAGe.R")
source("Scripts/Comorbilidades IRAG.R")
source("Scripts/Comorbilidades IRAGe.R")   
source("Scripts/severidad.R")

