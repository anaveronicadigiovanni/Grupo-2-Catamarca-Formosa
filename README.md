# Grupo-2-Catamarca-Formosa
Automatización de Reportes Epidemiológicos: UC-IRAG Catamarca Hospital de Niños Eva Perón

Este repositorio contiene el proyecto de análisis automatizado de datos derivados de la Estrategia de Vigilancia en Unidades Centinela de Infección Respiratoria Aguda Grave (UC-IRAG)  implementada en la población pediátrica de la provincia de Catamarca.
El objetivo principal es la organización, procesamiento y análisis reproducible de la información nominal y agrupada disponible, aportando datos de calidad y robustez para la toma de decisiones sanitarias por parte de los equipos de salud 

Alcance e Implementación
Establecimiento: Hospital Interzonal de Niños "Eva Perón" (HINEP), situado en la ciudad capital de San Fernando del Valle de Catamarca (único nosocomio con UC-IRAG en la provincia).

Dirección del Proyecto: Área de Vigilancia Epidemiológica de la Dirección Provincial de Epidemiología de Catamarca.

Población de Estudio: Pacientes pediátricos internados de 1 mes a 14 años de edad .

Periodo de Análisis: Semana Epidemiológica (SE) 1 a SE 52 del año 2025 (correspondiente a la etapa consolidada tras la prueba piloto de 2024) 

Objetivos del Análisis
Perfil Clínico y Epidemiológico:
Describir la distribución de los casos de IRAG e IRAGe según grupo etario, sexo y diagnóstico de ingreso (Bronquiolitis, Neumonía, Sepsis, Shock o Otros).
Evolución Temporal: Analizar la curva epidémica de las internaciones mediante semanas epidemiológicas.
Etiología Viral: Identificar el patrón de ocurrencia, estacionalidad y carga absoluta de virus respiratorios (VSR, Influenza y SARS-CoV-2) identificados por PCR.
Severidad y Comorbilidades: Estimar la frecuencia relativa de patologías de base y evaluar indirectamente la gravedad mediante la letalidad y el requerimiento de soporte ventilatorio (Oxígeno de bajo flujo, CAFO o ARM) 
Vacunación: Evaluar el estado de vacunación materna (Antigripal y VSR) en menores de 6 meses y la inmunización propia en niños de 6 a 24 meses.
Carga de Enfermedad: Determinar la proporción de ingresos por IRAG/IRAGe sobre el total de internaciones por todas las causas en el HINEP.

📁 Contenido del Repositorio
DOCUMENTACION/: Plan de análisis metodológico y mapa del circuito de datos de la Dirección de Epidemiología.
SCRIPTS/: Archivos de código en R para la importación, limpieza activa, codificación de nuevas variables y procesamiento estadístico.
TEMPLATES/: Estilos CSS, logos institucionales del Ministerio de Salud de Catamarca y encabezados para los reportes.
Reporte_UC_IRAG_Catamarca.qmd: Archivo Quarto maestro que integra el encabezado YAML, texto enriquecido y los chunks de R para la automatización.

🔒 Política de Confidencialidad de Datos⚠️ IMPORTANTE: Con el fin de garantizar el secreto profesional y cumplir con las normativas vigentes sobre la protección de datos personales y médicos, las bases de datos nominales y agrupadas utilizadas para este análisis no se comparten de manera pública en este repositorio.Para reproducir localmente los elementos de visualización (gráficos de Highcharter/ggplot2, tablas de resumen y mapas del circuito), el usuario institucional deberá incorporar de forma local las siguientes fuentes :
Base Nominal Uniregistro: Exportación de Vigilancia Nación de UC-IRAG del sistema SNVS 2.0.
Drive de Agrupados: Datos consolidados de ingresos totales a Clínica Médica y UTIP por semana epidemiológica 

🛠️ Requisitos e Instalación
Para ejecutar las herramientas de automatización y renderizar el informe final interactivo en formato HTML, se requiere instalar los siguientes paquetes de R en RStudio:
Rinstall.packages(c("tidyverse", "dplyr", "ggplot2", "lubridate", "stringr", 
                   "readxl", "readr", "writexl", "highcharter", "tidyr", 
                   "leaflet", "gt", "htmltools", "here"))
Instrucciones de uso:
Clonar o descargar este repositorio localmente.
Abrir el proyecto .Rproj e incluir las bases de datos protegidas en la ruta local correspondiente.
Renderizar el archivo .qmd para compilar el informe epidemiológico automatizado 
