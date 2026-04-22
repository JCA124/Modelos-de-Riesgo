# Análisis clientes solicitantes de crédito

#install.packages("bigrquery")
  library(bigrquery)
  library(dplyr)
  library(ggplot2)

# Consulta realizada sobre una base proporcionada para fines académicos
# El acceso real fue omitido por confidencialidad

# =================================================
#                  PROCEDIMIENTO INICIAL
# =================================================

  str(datos)
  nrow(datos) # 261656
  
  # LIMPIEZA GASTOS y CORRECCIÓN DE NEGATIVOS
  datos_clean <- datos %>%  mutate( 
                 GASTOS = na_if(GASTOS, "NA"),
                 GASTOS = as.numeric(GASTOS),
                 INGRESOS = ifelse(INGRESOS < 0, 0, INGRESOS),
                 GASTOS = ifelse(GASTOS < 0, 0, GASTOS),
                 INSTRUCCION = ifelse(INSTRUCCION == "", "Sin registro", INSTRUCCION),
                 INSTRUCCION = ifelse(INSTRUCCION == "Formación intermedia (técnica", 
                                      "Formación intermedia (técnica)", INSTRUCCION),
                 PROVINCIA = ifelse(PROVINCIA == "", "Sin registro", PROVINCIA),
                 ANTIG_LABORAL = ifelse(ANTIG_LABORAL < 0, NA, ANTIG_LABORAL),
                 REFINANCIAMIENTO_REESTRUCTURA = na_if(REFINANCIAMIENTO_REESTRUCTURA, "")
                 ) %>%
                 filter(!(INGRESOS == 0 & GASTOS == 0)) %>% 
                 arrange(IDENTIFICACION, desc(FECHA_CORTE)) %>%
                 distinct(IDENTIFICACION, .keep_all = TRUE) 
  
  nrow(datos_clean) #123631


# ========= SEPARACIÓN POR PERFIL =========

  microempresa <- datos_clean %>% filter(PERFIL_CLIENTE == "MICROEMPRESARIO")
  dependiente <- datos_clean %>% filter(PERFIL_CLIENTE == "DEPENDIENTE")
  nrow(microempresa); mean(microempresa$INGRESOS); sd(microempresa$INGRESOS) # 76874, 2553.707, 4367.168
  nrow(dependiente) ; mean(dependiente$INGRESOS) ; sd(dependiente$INGRESOS) #  46757, 1305.341, 7698.564
  boxplot( microempresa$INGRESOS, ylab = "Ingresos")
  boxplot( dependiente$INGRESOS, ylab = "Ingresos")
  
  

# ========= TRATAMIENTO DATOS ATÍPICOS =========

  microempresa %>% arrange(desc(INGRESOS)) %>% select("INSTRUCCION" ,"CARGAS","INGRESOS","GASTOS") %>% head(10) 
  dependiente %>% arrange(desc(INGRESOS)) %>% select("INSTRUCCION","CARGAS","INGRESOS","GASTOS") %>% head(10) 
  
  microempresa <- microempresa %>% filter( !(INGRESOS > quantile(INGRESOS, 0.99, na.rm = TRUE) & GASTOS == 0))
  
  dependiente <- dependiente %>% filter(!( INGRESOS > quantile(INGRESOS, 0.99, na.rm = TRUE) &
                  GASTOS == 0)) %>% filter(!(INGRESOS > 10000 &GASTOS <= INGRESOS * 0.3))
  
  nrow(microempresa); mean(microempresa$INGRESOS); sd(microempresa$INGRESOS) # 76822 , 2531.608 , 3078.552
  nrow(dependiente) ; mean(dependiente$INGRESOS) ; sd(dependiente$INGRESOS) # 46448 , 1072.059 , 1229.085
  boxplot( microempresa$INGRESOS, ylab = "Ingresos")
  boxplot( dependiente$INGRESOS, ylab = "Ingresos")
  

# ========= MARGEN FINANCIERO =========
  
  MF_microempresa <- microempresa%>% mutate(MF = INGRESOS - GASTOS) 
  MF_dependiente <- dependiente%>% mutate(MF = INGRESOS - GASTOS) 
  
  
# =================================================
#        ANÁLISIS ESTADISTICO Y VISUALIZACIÓN
# =================================================

# ========= ANÁLISIS DE DENSIDAD =========
  
  ggplot(microempresa %>% filter(INGRESOS > 0), aes(x = INGRESOS, fill = INSTRUCCION)) +
  geom_density(alpha = 0.35)  + facet_wrap(~ INSTRUCCION, scales = "free_x")+ coord_cartesian(xlim = c(0, 10000)) +
  labs( x = "Ingresos", y = "Densidad") + theme_minimal()
  
  ggplot(dependiente %>% filter(INGRESOS > 0),aes(x = INGRESOS, fill = INSTRUCCION)) +
  geom_density(alpha = 0.35)  + facet_wrap(~ INSTRUCCION) + coord_cartesian(xlim = c(0, 2000)) +
  labs( x = "Ingresos",y = "Densidad") + theme_minimal()

  
# ========= SEGMENTACIÓN DE CAPACIDAD =========
 
  #------- MICROEMPRESA
  MF_microempresa %>%filter(is.finite(MF), PROVINCIA %in% names(sort(table(PROVINCIA), decreasing = TRUE)[1:3])) %>%
                     ggplot(aes(PROVINCIA, MF, fill = PROVINCIA)) +geom_boxplot() +coord_flip() +
                     labs(x = "Provincia",y = "Margen financiero") +theme_minimal() +theme(legend.position = "none")
  
  
  MF_microempresa %>%filter(is.finite(MF),ACTIVIDAD_ECONOMICA %in% names(sort(table(ACTIVIDAD_ECONOMICA),decreasing = TRUE)[1:3])) %>%
        ggplot(aes(ACTIVIDAD_ECONOMICA, MF, fill = ACTIVIDAD_ECONOMICA)) +geom_boxplot() +coord_flip() +
        labs(x = "Actividad económica",y = "Margen financiero") +theme_minimal() +theme(legend.position = "none")
  
  #------- DEPENDIENTE
  MF_dependiente %>%filter(is.finite(MF), PROVINCIA %in% names(sort(table(PROVINCIA), decreasing = TRUE)[1:3])) %>%
        ggplot(aes(PROVINCIA, MF, fill = PROVINCIA)) +geom_boxplot() +coord_flip() +
        labs(x = "Provincia",y = "Margen financiero") +theme_minimal() +theme(legend.position = "none")
  
  
  MF_dependiente %>%filter(is.finite(MF),ACTIVIDAD_ECONOMICA %in% names(sort(table(ACTIVIDAD_ECONOMICA),decreasing = TRUE)[1:3])) %>%
        ggplot(aes(ACTIVIDAD_ECONOMICA, MF, fill = ACTIVIDAD_ECONOMICA)) +geom_boxplot() +coord_flip() +
        labs(x = "Actividad económica",y = "Margen financiero") +theme_minimal() +theme(legend.position = "none")
  
# ========= EVALUACIÓN DE ESTABILIDAD =========
  MF_microempresa <- MF_microempresa %>% mutate(rango_antig = cut(ANTIG_LABORAL,
                             breaks = c(0, 30, 90, 180, 365, Inf),
                             labels = c("0-30", "31-90", "91-180", "181-365", "366+"),
                             include.lowest = TRUE))
  
  MF_dependiente <- MF_dependiente %>% mutate(rango_antig = cut(ANTIG_LABORAL,
                             breaks = c(0, 30, 90, 180, 365, Inf),
                             labels = c("0-30", "31-90", "91-180", "181-365", "366+"),
                             include.lowest = TRUE))

  ggplot(MF_dependiente,aes(x = rango_antig,y = SALDO_PROMEDIO_AHORRO,fill = rango_antig)) +
  geom_boxplot(outlier.alpha = 0.25) +coord_cartesian(ylim = c(0, 5000)) +
  labs(x = "Rango de antigüedad laboral",y = "Saldo promedio de ahorro") +
  theme_minimal() +theme(legend.position = "none")
  
  cor(MF_dependiente$ANTIG_LABORAL,MF_dependiente$SALDO_PROMEDIO_AHORRO,use = "complete.obs")
  
# ========= IDENTIFICACIÓN DE ALERTAS =========
 
  #------ Proporción de clientes con refinanciamiento o reestructuración
  sum(MF_microempresa$REFINANCIAMIENTO_REESTRUCTURA == "X", na.rm = TRUE) / nrow(MF_microempresa)
  sum(MF_dependiente$REFINANCIAMIENTO_REESTRUCTURA == "X", na.rm = TRUE) / nrow(MF_dependiente)
  
  
  #----- Perfil común
  alerta_microempresa <- MF_microempresa %>% filter(REFINANCIAMIENTO_REESTRUCTURA == "X")
  table(alerta_microempresa$INSTRUCCION)
  table(alerta_microempresa$PROVINCIA)
  summary(alerta_microempresa$INGRESOS)
  
  alerta_dependiente <- MF_dependiente %>% filter(REFINANCIAMIENTO_REESTRUCTURA == "X")
  table(alerta_dependiente$INSTRUCCION)
  table(alerta_dependiente$PROVINCIA)
  summary(alerta_dependiente$INGRESOS)
  
  
  
 

  
  
