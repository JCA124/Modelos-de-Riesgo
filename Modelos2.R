# ==============================================================================
# 1. Librerias
# ==============================================================================
options(scipen = 999)
library(dplyr)
library(tidyverse)
library(data.table)
library(lubridate)
library(formattable)
library(ggplot2)
library(gridExtra)
library(ranger)

# ==============================================================================
# 2. Tabla de performance (KS, ROC, GINI) por deciles de Score
# ==============================================================================
tabla_performance <- function(data, nrangos=10){# DataFrame con dos variables "Score" y "Var"
  rango_score <- function(vector){
    index <- aux <- seq(1:length(vector))
    res <- data.frame(id=index, val=vector)
    res <- res[order(res$val, decreasing = TRUE),]
    res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = nrangos+1),0), labels = seq(1,nrangos))
    res <- res[order(res$id),]
    return(as.numeric(res$aux))
  }
  data[, Rango := rango_score(Score)]
  # Tabla base
  tabla <- data[,list(Min=min(Score), Max=max(Score)), by=Rango][order(Rango)]
  tabla[nrow(tabla), 2] <- 1; tabla[1,3] <- 999; tabla <- tabla[,-c(1)]
  # Conteo de casos (Bueno, Malo, Indeterminados, Malo Observado y Sin desempeño)
  data <- data %>% group_by(Rango, Var) %>% summarise(Casos = n()) %>% spread(key = "Var", value = "Casos", fill = 0)
  data$Total <- data$'0' + data$'1'
  data$Z <- (data$'1')/(data$'0' + data$'1') # Tasa de malos ponderada
  data$W <- (data$'0')/(data$'0' + data$'1') # Tasa de buenos ponderada
  data$Otros <- 0 # Malos Observados + Sin desempeño
  data$MC <- ceiling(data$Otros*data$Z + data$'1') # Malos corregidos
  data$BC <- ceiling(data$Otros*data$W + data$'0') # Buenos corregidos
  # Columnas faltantes
  tabla$Total <- data$Total
  tabla$PTotal <- percent(tabla$Total/sum(tabla$Total), digits = 1)
  tabla$ATotal <- cumsum(tabla$PTotal)
  tabla$Malo <- data$MC
  tabla$PMalo <- percent(tabla$Malo/sum(tabla$Malo), digits = 1)
  tabla$AMalo <- cumsum(tabla$PMalo)
  tabla$RazonMalo <- percent(tabla$Malo/tabla$Total, digits = 1)
  tabla$AcumTotal <- cumsum(tabla$Total)
  tabla$AcumMalo <- cumsum(tabla$Malo)
  tabla$CumMalo <- percent(tabla$AcumMalo/tabla$AcumTotal, digits = 1)
  tabla$DecumMalo <- sum(tabla$Malo)
  for(k in 2:nrow(tabla)){
    tabla[k,"DecumMalo"] <- sum(tabla$Malo) - tabla[k-1,"AcumMalo"]
  }
  tabla$PDecumMalo <- tabla$DecumMalo/sum(tabla$Malo)
  tabla$Bueno <- data$BC
  tabla$PBueno <- percent(tabla$Bueno/sum(tabla$Bueno), digits = 1)
  tabla$AcumBueno <- cumsum(tabla$Bueno)
  tabla$DecumBueno <- sum(tabla$Bueno)
  for(k in 2:nrow(tabla)){
    tabla[k,"DecumBueno"] <- sum(tabla$Bueno) - tabla[k-1,"AcumBueno"]
  }
  tabla$PDecumBueno <- tabla$DecumBueno/sum(tabla$Bueno)
  tabla$KS <- percent(abs(tabla$PDecumMalo - tabla$PDecumBueno), digits = 1)
  tabla$ROC <- percent(0, digits = 1)
  for(k in 1:nrow(tabla)){
    if(k == nrow(tabla)){
      tabla[k, "ROC"] <- percent(tabla[k, "PBueno"]*(tabla[k, "PDecumMalo"])/2, digits = 1)
    } else {
      tabla[k, "ROC"] <- percent(tabla[k, "PBueno"]*(tabla[k, "PDecumMalo"] + tabla[k+1, "PDecumMalo"])/2, digits = 1)
    }
  }
  tper <- tabla[,c(1,2,3,4,5,6,7,8,9,12)]
  SumTotal <- 0
  for(k in 1:nrow(tper)){
    SumTotal <- SumTotal + tper[k,3]
  }
  SumMalos <- 0
  for(k in 1:nrow(tper)){
    SumMalos <- SumMalos + tper[k,6]
  }
  KSe <- max(tabla$KS)
  ROCe <- sum(tabla$ROC)
  GINIe <- 2*ROCe - 1
  res <- data.table("KS" = KSe, "ROC" = ROCe, "GINI" = GINIe)
  return(list(tper, res))
}


# ==============================================================================
# 3.5 Construccion de variables SCE (consolidadas) a partir de variables brutas
# ==============================================================================
datos[, ANTIGUEDAD_SCE := pmax(ANTIGUEDAD_OP_SBS, ANTIGUEDAD_TC_SBS, ANTIGUEDAD_OP_SC,
                                ANTIGUEDAD_TC_SC, ANTIGUEDAD_OP_SICOM, ANTIGUEDAD_TC_SICOM)]

datos[, MAX_DVEN_SCE_36M := pmax(MAX_DVEN_SBS_OP_36M, MAX_DVEN_SC_OP_36M, MAX_DVEN_SICOM_OP_36M,
                                  MAX_DVEN_OTROS_SIS_OP_36M, MAX_DVEN_SBS_TC_36M, MAX_DVEN_SC_TC_36M,
                                  MAX_DVEN_SICOM_TC_36M, MAX_DVEN_OTROS_SIS_TC_36M)]

datos[, NENT_VEN_SCE_12M := NENT_VEN_SBS_OP_12M + NENT_VEN_SBS_TC_12M + NENT_VEN_SC_OP_12M +
                             NENT_VEN_SC_TC_12M + NENT_VEN_SICOM_OP_12M + NENT_VEN_SICOM_TC_12M +
                             NENT_VEN_OTROS_OP_12M + NENT_VEN_OTROS_TC_12M]

datos[, PROM_VEN_SBS_6M := PROM_VEN_SBS_OP_6M + PROM_VEN_SBS_TC_6M + PROM_DEM_SBS_OP_6M +
                            PROM_CAS_SBS_OP_6M + PROM_DEM_SBS_TC_6M + PROM_CAS_SBS_TC_6M]

datos[, NOPE_VENC_31AMAS_OP_36M := NOPE_VENC_31A90_OP_36M + NOPE_VENC_91A180_OP_36M +
                                    NOPE_VENC_181A360_OP_36M + NOPE_VENC_MAYOR360_OP_36M +
                                    NOPE_DEMANDA_OP_36M + NOPE_CASTIGO_OP_36M]

datos[, NTC_VENC_31AMAS_TC_36M := NTC_VENC_31A90_TC_36M + NTC_VENC_91A180_TC_36M +
                                   NTC_VENC_181A360_TC_36M + NTC_VENC_MAYOR360_TC_36M +
                                   NTC_DEMANDA_TC_36M + NTC_CASTIGO_TC_36M]

# ==============================================================================
# 4. Ingenieria de variables: consolidacion desde multiples fuentes (SBS/SC/SICOM/Otros)
# ==============================================================================
# 1. Antiguedad acotada
datos[, ANTIGUEDAD_SCE := ifelse(ANTIGUEDAD_SCE > 500, 500, ANTIGUEDAD_SCE)]

# 3. Monto vencido reciente 3M consolidado: TC + OP + SC
datos[, MVALVEN_SBS_OP_3M := rowSums(.SD, na.rm = TRUE),.SDcols = c(
  "MVALVEN_SBS_OP_3M","MVALVEN_SC_OP_3M","MVALVEN_SBS_TC_3M")]
datos[is.na(MVALVEN_SBS_OP_3M) | !is.finite(MVALVEN_SBS_OP_3M), MVALVEN_SBS_OP_3M := 0]

# 4. Aperturas 12M consolidadas: OP + TC + SCE
datos[, NTC_APERT_SCE_12M :=
        NTC_APERT_SBS_TC_12M +
        NTC_APERT_SC_TC_12M +
        NTC_APERT_SICOM_TC_12M +
        NTC_APERT_OTROS_TC_12M]

datos[, NOPE_APERT_SBS_OP_12M :=
        rowSums(.SD, na.rm = TRUE),
      .SDcols = c(
        "NOPE_APERT_SBS_OP_12M",
        "NTC_APERT_SBS_TC_12M",
        "NOPE_APERT_SC_OP_12M",
        "NOPE_APERT_SICOM_OP_12M",
        "NOPE_APERT_OTROS_OP_12M",
        "NTC_APERT_SCE_12M"
      )]

# 5. Cartera castigada 24M consolidada: OP + TC + SCE
datos[, MVAL_CASTIGO_DEMANDA_OP_24M :=
        rowSums(.SD, na.rm = TRUE),
      .SDcols = c(
        "MVAL_CASTIGO_SBS_OP_24M",
        "MVAL_CASTIGO_SC_OP_24M",
        "MVAL_CASTIGO_SICOM_OP_24M",
        "MVAL_CASTIGO_OTROS_OP_24M",
        "MVAL_CASTIGO_SBS_TC_24M",
        "MVAL_CASTIGO_SC_TC_24M",
        "MVAL_CASTIGO_SICOM_TC_24M",
        "MVAL_CASTIGO_OTROS_TC_24M",
        "MVAL_DEMANDA_SBS_OP_24M",
        "MVAL_DEMANDA_SC_OP_24M",
        "MVAL_DEMANDA_SICOM_OP_24M",
        "MVAL_DEMANDA_OTROS_OP_24M",
        "MVAL_DEMANDA_SBS_TC_24M",
        "MVAL_DEMANDA_SC_TC_24M",
        "MVAL_DEMANDA_SICOM_TC_24M",
        "MVAL_DEMANDA_OTROS_TC_24M"
      )]

mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]

# ==============================================================================
# 5. Analisis exploratorio de variables
# ==============================================================================
quantile(datos$ANTIGUEDAD_SCE, probs = seq(0,1,by=0.01))
prop.table(mod[,table(VarDep, genero)],2)
prop.table(mod[,table(VarDep, estadoCivil)],2)
quantile(datos$MVALVEN_SBS_OP_3M, probs = seq(0,1,by=0.01))
quantile(datos$MVAL_CASTIGO_DEMANDA_OP_24M, probs = seq(0,1,by=0.01))
quantile(datos$NOPE_APERT_SBS_OP_12M, probs = seq(0,1,by=0.01))
quantile(datos$MAX_DVEN_SCE_36M, probs = seq(0,1,by=0.01))
quantile(datos$NENT_VEN_SCE_12M, probs = seq(0,1,by=0.01))

cor(datos[,c("ANTIGUEDAD_SCE","MVALVEN_SBS_OP_3M","NOPE_APERT_SBS_OP_12M",
             "MVAL_CASTIGO_DEMANDA_OP_24M","MAX_DVEN_SCE_36M",
             "NENT_VEN_SCE_12M")],use="pairwise.complete.obs")

# ==============================================================================
# 6. Tratamiento de atipicos (acotacion)
# ==============================================================================
# NOPE_APERT_SBS_OP_12M
datos[NOPE_APERT_SBS_OP_12M > 35, NOPE_APERT_SBS_OP_12M := 35]
# MVALVEN_SBS_OP_3M
datos[MVALVEN_SBS_OP_3M > 4717.8286, MVALVEN_SBS_OP_3M := 4717.8286]
# MVAL_CASTIGO_DEMANDA_OP_24M
datos[MVAL_CASTIGO_DEMANDA_OP_24M > 8231.5950 , MVAL_CASTIGO_DEMANDA_OP_24M := 8231.5950 ]
datos[MAX_DVEN_SCE_36M > 3924.05 , MAX_DVEN_SCE_36M := 3924.05 ]
datos[PROM_VEN_SBS_6M > 2562.4260     , PROM_VEN_SBS_6M := 2562.4260     ]
datos[NOPE_VENC_31AMAS_OP_36M > 10       , NOPE_VENC_31AMAS_OP_36M := 10       ]
datos[NTC_VENC_31AMAS_TC_36M > 10       , NTC_VENC_31AMAS_TC_36M := 10       ]

# Dummy de Apertura
datos[, d_NOPE_APERT_SBS_OP_12M := ifelse(NOPE_APERT_SBS_OP_12M > 0, 1, 0)]
# Dummy de Diaz vencidos
datos[, d_MAX_DVEN_SCE_36M := ifelse(MAX_DVEN_SCE_36M > 0, 1, 0)]

# Transformacion Estado Civil y Genero
datos[, genero := fifelse(genero == "1", "MASCULINO", "FEMENINO")]
datos[, estadoCivil := fcase(
  estadoCivil == "1", "SOLTERO",
  estadoCivil == "2", "CASADO",
  estadoCivil == "3", "DIVORCIADO",
  estadoCivil == "4", "VIUDO",
  estadoCivil == "5", "UNION_HECHO"
)]

# NENT_VEN_SCE_12M
datos[NENT_VEN_SCE_12M > 3, NENT_VEN_SCE_12M := 3]

# ==============================================================================
# 7. Particion modelamiento (mod) y validacion (val)
# ==============================================================================
# Funcion de reemplazo de NA's 
reemplazo_col = function(dt, vars, valor){
  na.replace = function(v, value=valor) { v[is.na(v)] = value; v }
  for (i in vars)
    eval(parse(text=paste("dt[,",i,":=na.replace(",i,")]")))
}

mod <- datos[ModVal == 0 & VarDep %in% c(0,1)]
val <- datos[ModVal == 1 & VarDep %in% c(0,1)]

# ==============================================================================
# 8. H2O: inicializacion y variables por modelo
# ==============================================================================
library(h2o)
h2o.init(ip = "localhost", nthreads = -1, max_mem_size = "4G")

vars_glm <- c("VarDep","MAX_DVEN_SCE_36M","NENT_VEN_SCE_12M","NTC_VENC_31AMAS_TC_36M",
              "NOPE_VENC_31AMAS_OP_36M","MVALVEN_SBS_OP_3M","MVAL_CASTIGO_DEMANDA_OP_24M",
              "PROM_VEN_SBS_6M","NOPE_APERT_SBS_OP_12M","NTC_APERT_SCE_12M",
              "ANTIGUEDAD_SCE")

vars_rf  <- c("VarDep","MAX_DVEN_SCE_36M","NENT_VEN_SCE_12M","NTC_VENC_31AMAS_TC_36M",
              "NOPE_VENC_31AMAS_OP_36M","MVALVEN_SBS_OP_3M","MVAL_CASTIGO_DEMANDA_OP_24M",
              "PROM_VEN_SBS_6M","NOPE_APERT_SBS_OP_12M","NTC_APERT_SCE_12M",
              "ANTIGUEDAD_SCE","genero","estadoCivil")

vars_gbm <- c("VarDep","MAX_DVEN_SCE_36M","NENT_VEN_SCE_12M","NTC_VENC_31AMAS_TC_36M",
              "MVALVEN_SBS_OP_3M","MVAL_CASTIGO_DEMANDA_OP_24M",
              "PROM_VEN_SBS_6M","NOPE_APERT_SBS_OP_12M","NTC_APERT_SCE_12M",
              "ANTIGUEDAD_SCE","genero","estadoCivil")

vars_nn  <- c("VarDep","MAX_DVEN_SCE_36M","NENT_VEN_SCE_12M","NTC_VENC_31AMAS_TC_36M",
              "NOPE_VENC_31AMAS_OP_36M","MVALVEN_SBS_OP_3M","MVAL_CASTIGO_DEMANDA_OP_24M",
              "PROM_VEN_SBS_6M","NOPE_APERT_SBS_OP_12M","NTC_APERT_SCE_12M",
              "ANTIGUEDAD_SCE","genero","estadoCivil")

vars_ens <- vars_rf

# Funcion auxiliar para convertir categoricas en H2OFrame
set_factors <- function(hf){
  if("VarDep"      %in% names(hf)) hf[,"VarDep"]      <- as.factor(hf[,"VarDep"])
  if("genero"      %in% names(hf)) hf[,"genero"]      <- as.factor(hf[,"genero"])
  if("estadoCivil" %in% names(hf)) hf[,"estadoCivil"] <- as.factor(hf[,"estadoCivil"])
  return(hf)
}

mod_em_glm <- set_factors(as.h2o(setDT(mod)[, vars_glm, with=FALSE]))
mod_em_rf  <- set_factors(as.h2o(setDT(mod)[, vars_rf,  with=FALSE]))
mod_em_gbm <- set_factors(as.h2o(setDT(mod)[, vars_gbm, with=FALSE]))
mod_em_nn  <- set_factors(as.h2o(setDT(mod)[, vars_nn,  with=FALSE]))
mod_em_ens <- set_factors(as.h2o(setDT(mod)[, vars_ens, with=FALSE]))

val_em_glm <- set_factors(as.h2o(setDT(val)[, vars_glm, with=FALSE]))
val_em_rf  <- set_factors(as.h2o(setDT(val)[, vars_rf,  with=FALSE]))
val_em_gbm <- set_factors(as.h2o(setDT(val)[, vars_gbm, with=FALSE]))
val_em_nn  <- set_factors(as.h2o(setDT(val)[, vars_nn,  with=FALSE]))
val_em_ens <- set_factors(as.h2o(setDT(val)[, vars_ens, with=FALSE]))

y_em   <- "VarDep"
x_glm  <- setdiff(names(mod_em_glm), y_em)
x_rf   <- setdiff(names(mod_em_rf),  y_em)
x_gbm  <- setdiff(names(mod_em_gbm), y_em)
x_nn   <- setdiff(names(mod_em_nn),  y_em)
x_ens  <- setdiff(names(mod_em_ens), y_em)
nfolds <- 5

# ==============================================================================
# 9. Modelos individuales
# ==============================================================================
my_glm <- h2o.glm(x = x_glm, y = y_em,
                  model_id = "GLM", training_frame = mod_em_glm,
                  alpha = 0.1, remove_collinear_columns = TRUE,
                  nfolds = nfolds, fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE, seed = 12345)

my_rf  <- h2o.randomForest(x = x_rf, y = y_em,
                           model_id = "RF", training_frame = mod_em_rf,
                           ntrees = 200, min_rows = 800, mtries = 3,
                           nfolds = nfolds, fold_assignment = "Stratified",
                           keep_cross_validation_predictions = TRUE, seed = 12345)

my_gbm <- h2o.gbm(x = x_gbm, y = y_em,
                  model_id = "GBM", training_frame = mod_em_gbm,
                  distribution = "bernoulli", ntrees = 200, max_depth = 3,
                  min_rows = 800, learn_rate = 0.02,
                  nfolds = nfolds, fold_assignment = "Stratified",
                  keep_cross_validation_predictions = TRUE, seed = 12345)

my_nn  <- h2o.deeplearning(x = x_nn, y = y_em,
                           model_id = "NN", training_frame = mod_em_nn,
                           activation = "Rectifier", hidden = c(20,20),
                           epochs = 100, nfolds = nfolds,
                           keep_cross_validation_predictions = TRUE, seed = 12345)

# ==============================================================================
# 10. Modelos base para ensambles (todos sobre vars_ens)
# h2o.stackedEnsemble requiere mismo training_frame y fold_assignment
# ==============================================================================
glm_ens <- h2o.glm(x = x_ens, y = y_em, training_frame = mod_em_ens,
                   alpha = 0.1, remove_collinear_columns = TRUE,
                   nfolds = nfolds, fold_assignment = "Stratified",
                   keep_cross_validation_predictions = TRUE, seed = 12345)

rf_ens  <- h2o.randomForest(x = x_ens, y = y_em, training_frame = mod_em_ens,
                            ntrees = 200, min_rows = 800, mtries = 3,
                            nfolds = nfolds, fold_assignment = "Stratified",
                            keep_cross_validation_predictions = TRUE, seed = 12345)

gbm_ens <- h2o.gbm(x = x_ens, y = y_em, training_frame = mod_em_ens,
                   distribution = "bernoulli", ntrees = 200, max_depth = 3,
                   min_rows = 800, learn_rate = 0.02,
                   nfolds = nfolds, fold_assignment = "Stratified",
                   keep_cross_validation_predictions = TRUE, seed = 12345)

nn_ens  <- h2o.deeplearning(x = x_ens, y = y_em, training_frame = mod_em_ens,
                            activation = "Rectifier", hidden = c(20,20),
                            epochs = 100, nfolds = nfolds,
                            keep_cross_validation_predictions = TRUE, seed = 12345)

# ==============================================================================
# 11. Ensambles
# ==============================================================================
e1m <- h2o.stackedEnsemble(x = x_ens, y = y_em,
                           training_frame = mod_em_ens,
                           model_id = "Ensamble_1m",
                           metalearner_algorithm = "glm",
                           base_models = list(rf_ens, glm_ens))

e2m <- h2o.stackedEnsemble(x = x_ens, y = y_em,
                           training_frame = mod_em_ens,
                           model_id = "Ensamble_2m",
                           metalearner_algorithm = "glm",
                           base_models = list(glm_ens, gbm_ens, nn_ens))

e3m <- h2o.stackedEnsemble(x = x_ens, y = y_em,
                           training_frame = mod_em_ens,
                           model_id = "Ensamble_3m",
                           metalearner_algorithm = "glm",
                           base_models = list(glm_ens, gbm_ens, nn_ens, rf_ens))

# ==============================================================================
# 12. Funciones auxiliares (no modificar)
# ==============================================================================
rango_score <- function(vector){
  index <- aux <- seq(1:length(vector))
  res <- data.frame(id=index, val=vector)
  res <- res[order(res$val, decreasing = TRUE),]
  res$aux <- cut(aux, breaks = round(seq(0, length(vector),length.out = 11),0), labels = seq(1,10))
  res <- res[order(res$id),]
  return(as.numeric(res$aux))
}

res_fun <- function(valida, resultado){
  res <- data.frame(VarDep=valida$VarDep,
                    Score=1000-round(1000*as.data.frame(resultado)[,3],0),
                    Rango=rango_score(1000-round(1000*as.data.frame(resultado)[,3],0)))
  return(res)
}

# ==============================================================================
# 13. Evaluacion de modelos individuales (Performance + PSI)
# ==============================================================================
# GLM
mod_glm <- setDT(res_fun(mod, h2o.predict(my_glm, newdata = mod_em_glm)))
colnames(mod_glm)[1] <- "Var"
tabla_performance(mod_glm)
val_glm <- setDT(res_fun(val, h2o.predict(my_glm, newdata = val_em_glm)))
colnames(val_glm)[1] <- "Var"
tabla_performance(val_glm)
PSI <- data.table(Decil=1:10,
                  MOD=as.numeric(suppressMessages(tabla_performance(mod_glm))[[1]]$RazonMalo),
                  VAL=as.numeric(suppressMessages(tabla_performance(val_glm))[[1]]$RazonMalo))
PSI[, `:=`(VM=VAL-MOD, LN_VM=log(VAL/MOD), Indice=(VAL-MOD)*log(VAL/MOD))]
PSI[, .(Decil, MOD=round(MOD,6), VAL=round(VAL,6), VM=round(VM,6),
        LN_VM=round(LN_VM,6), Indice=round(Indice,6))]
cat("PSI GLM:", round(PSI[, sum(Indice)*100], 6), "\n")

# RF
mod_rf <- setDT(res_fun(mod, h2o.predict(my_rf, newdata = mod_em_rf)))
colnames(mod_rf)[1] <- "Var"
tabla_performance(mod_rf)
val_rf <- setDT(res_fun(val, h2o.predict(my_rf, newdata = val_em_rf)))
colnames(val_rf)[1] <- "Var"
tabla_performance(val_rf)
PSI <- data.table(Decil=1:10,
                  MOD=as.numeric(suppressMessages(tabla_performance(mod_rf))[[1]]$RazonMalo),
                  VAL=as.numeric(suppressMessages(tabla_performance(val_rf))[[1]]$RazonMalo))
PSI[, `:=`(VM=VAL-MOD, LN_VM=log(VAL/MOD), Indice=(VAL-MOD)*log(VAL/MOD))]
PSI[, .(Decil, MOD=round(MOD,6), VAL=round(VAL,6), VM=round(VM,6),
        LN_VM=round(LN_VM,6), Indice=round(Indice,6))]
cat("PSI RF:", round(PSI[, sum(Indice)*100], 6), "\n")

# GBM
mod_gbm <- setDT(res_fun(mod, h2o.predict(my_gbm, newdata = mod_em_gbm)))
colnames(mod_gbm)[1] <- "Var"
tabla_performance(mod_gbm)
val_gbm <- setDT(res_fun(val, h2o.predict(my_gbm, newdata = val_em_gbm)))
colnames(val_gbm)[1] <- "Var"
tabla_performance(val_gbm)
PSI <- data.table(Decil=1:10,
                  MOD=as.numeric(suppressMessages(tabla_performance(mod_gbm))[[1]]$RazonMalo),
                  VAL=as.numeric(suppressMessages(tabla_performance(val_gbm))[[1]]$RazonMalo))
PSI[, `:=`(VM=VAL-MOD, LN_VM=log(VAL/MOD), Indice=(VAL-MOD)*log(VAL/MOD))]
PSI[, .(Decil, MOD=round(MOD,6), VAL=round(VAL,6), VM=round(VM,6),
        LN_VM=round(LN_VM,6), Indice=round(Indice,6))]
cat("PSI GBM:", round(PSI[, sum(Indice)*100], 6), "\n")


# Ensamble 1
mod_e1m <- setDT(res_fun(mod, h2o.predict(e1m, newdata = mod_em_ens)))
colnames(mod_e1m)[1] <- "Var"
tabla_performance(mod_e1m)
val_e1m <- setDT(res_fun(val, h2o.predict(e1m, newdata = val_em_ens)))
colnames(val_e1m)[1] <- "Var"
tabla_performance(val_e1m)

# Ensamble 2
mod_e2m <- setDT(res_fun(mod, h2o.predict(e2m, newdata = mod_em_ens)))
colnames(mod_e2m)[1] <- "Var"
tabla_performance(mod_e2m)
val_e2m <- setDT(res_fun(val, h2o.predict(e2m, newdata = val_em_ens)))
colnames(val_e2m)[1] <- "Var"
tabla_performance(val_e2m)

# Ensamble 3
mod_e3m <- setDT(res_fun(mod, h2o.predict(e3m, newdata = mod_em_ens)))
colnames(mod_e3m)[1] <- "Var"
tabla_performance(mod_e3m)
val_e3m <- setDT(res_fun(val, h2o.predict(e3m, newdata = val_em_ens)))
colnames(val_e3m)[1] <- "Var"
tabla_performance(val_e3m)

# ==============================================================================
# 15. Guardado de modelos entrenados para el Shiny
# ==============================================================================
# Los 4 modelos que usa el aplicativo: GLM, RF, GBM y el Ensamble_3m como "Ensamble".
# Se guardan con nombre de archivo fijo para que el Shiny los cargue sin ambiguedad.
dir_modelos <- "Aplicativo Shiny/modelos"
dir.create(dir_modelos, showWarnings = FALSE, recursive = TRUE)

guardar_modelo <- function(modelo, nombre_archivo){
  ruta_tmp <- h2o.saveModel(object = modelo, path = dir_modelos, force = TRUE)
  ruta_final <- file.path(dir_modelos, nombre_archivo)
  if (file.exists(ruta_final)) file.remove(ruta_final)
  file.rename(ruta_tmp, ruta_final)
  cat(nombre_archivo, "guardado en", ruta_final, "\n")
  invisible(ruta_final)
}

guardar_modelo(my_glm, "modelo_glm")
guardar_modelo(my_rf,  "modelo_rf")
guardar_modelo(my_gbm, "modelo_gbm")
guardar_modelo(e3m,    "modelo_ensamble")


