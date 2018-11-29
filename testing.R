#### Validación #####

# Paquetes ------
library(pacman)

p_load(tidyverse, 
       glmnet, 
       caret, 
       readxl, 
       caTools,
       plotROC)

# Cargar los datos del entrenamiento -----
load("adicciones.RData")

# Datos de los adictos ------
datos <- read.delim("TMS_NEURODATA.csv", sep = "\t", header = TRUE)

# Seleccionar variables: adictos -------
adictos <- datos %>% 
  select(Edad = Edad,
         Categorías_concluidas = BCST.CC,
         Categorías_experimentadas = BCST.CE,
         Respuestas_correctas = BCST.RC,
         Total_errores = BCST.TE,
         Respuestas_perseverativas = BCST.RP,
         Errores_perseverativos = BCST.EP,
         Errores_no_perseverativos = BCST.ENP,
         Errores_únicos = BCST.EU,
         Fracasos_para_mantener_set = BCST.FMS,
         Aciertos_congruentes = F.AC,
         Aciertos_incongruentes = F.AI,
         Errores_congruentes =  F.EC,
         Errores_incongruentes = F.EI,
         Tiempo_reacción_incongruentes =F.TRI,
         Tiempo_reacción_congruentes =  F.TRC,
         Tiempo_reacción_errores.congruentes = F.TREC, 
         Tiempo_reacción_errores.incongruentes = F.TREI, 
         Errores_comisión = F.EC1,
         Aciertos_No_Go =  F.ANG, 
         Tiempo_reacción_errores.comisión = F.TREC1, 
         Efecto_flanker = F.EF,
         R_DOI = RDOI,
         L_N = LN,
         Correctas = TL.C, 
         Total_movimientos = TL.TM,
         Tiempo_inicio = TL.TI, 
         Tiempo_ejecución = TL.TE,
         Tiempo_total = TL.TT, 
         Total = IGT.T, 
         Respuestas_ventajosas = IGT.RV, 
         TR_ventajosas = IGT.TRV,
         Respuestas_desventajosas = IGT.RD,
         TR_desventajosas = IGT.TRD,
         Total_aciertos = RMET.TA,
         Aciertos_hombres = RMET.AH,
         Aciertos_mujeres = RMET.AM, 
         Aciertos_positivos = RMET.AP, 
         Aciertos_negativos = RMET.AN, 
         Aciertos_neutros = RMET.ANE) 

# Datos de los controles ------
datos_2 <- read.csv("HEALTHY_CONTROLS.csv")

# Seleccionar variables: controles -------
controles <- datos_2 %>% 
  select(Edad = Edad,
         Categorías_concluidas = BCST.CC,
         Categorías_experimentadas = BCST.CE,
         Respuestas_correctas = BCST.RC,
         Total_errores = BCST.TE,
         Respuestas_perseverativas = BCST.RP,
         Errores_perseverativos = BCST.EP,
         Errores_no_perseverativos = BCST.ENP,
         Errores_únicos = BCST.EU,
         Fracasos_para_mantener_set = BCST.FMS,
         Aciertos_congruentes = F.AC,
         Aciertos_incongruentes = F.AI,
         Errores_congruentes =  F.EC,
         Errores_incongruentes = F.EI,
         Tiempo_reacción_incongruentes =F.TRI,
         Tiempo_reacción_congruentes =  F.TRC,
         Tiempo_reacción_errores.congruentes = F.TREC, 
         Tiempo_reacción_errores.incongruentes = F.TREI, 
         Errores_comisión = F.EC1,
         Aciertos_No_Go =  F.ANG, 
         Tiempo_reacción_errores.comisión = F.TREC1, 
         Efecto_flanker = F.EF,
         R_DOI = RDOI,
         L_N = ln,
         Correctas = TL.C, 
         Total_movimientos = TL.TM,
         Tiempo_inicio = TL.TI, 
         Tiempo_ejecución = TL.TE,
         Tiempo_total = TL.TT, 
         Total = IGT.T, 
         Respuestas_ventajosas = IGT.RV, 
         TR_ventajosas = IGT.TRV,
         Respuestas_desventajosas = IGT.RD,
         TR_desventajosas = IGT.TRD,
         Total_aciertos = RMET.TA,
         Aciertos_hombres = RMET.AH,
         Aciertos_mujeres = RMET.AM, 
         Aciertos_positivos = RMET.AP, 
         Aciertos_negativos = RMET.AN, 
         Aciertos_neutros = RMET.ANE) 

# Test set ------
test_set <- adictos %>% 
  rbind(controles) %>% 
  mutate(Edad = round(Edad),
         Grupo = c(rep(1, 23), 
                   rep(0, 152))) %>% 
  select(Grupo, everything())

# Hacer factor el grupo ------
test_set$Grupo <- factor(test_set$Grupo, 
                         levels = c(1, 0), 
                         labels = c("Adicto", "Control"))   

# Corregir caracteres ------
test_set <- test_set %>% 
  mutate(Tiempo_reacción_errores.congruentes = 
           as.double(Tiempo_reacción_errores.congruentes),
         Tiempo_reacción_errores.comisión = 
           as.double(Tiempo_reacción_errores.comisión))

# Remover NA ----
test_set <- na.omit(test_set)


# Submuestra de 20 x 20 -----
set.seed(666)
test <- test_set %>% 
  group_by(Grupo) %>% 
  sample_n(20) %>% 
  ungroup()

## Predicción con Glmnet -----

# Predicción en el train set -----
p_train <- predict(glmnet, d)
probs_tr <- predict(glmnet, d, type = "prob")

# Matriz de confusión glmnet train set -----
confusionMatrix(d$Grupo, p_train)

# Curva ROC glmnet train set -----
colAUC(probs_tr$Adicto, d$Grupo, plotROC = T)


# Predicción glmnet test set -----
p_test <- predict(glmnet, test)
probs_te <- predict(glmnet, test, type = "prob")

# Matriz de confusión glmnet test set ----
confusionMatrix(test$Grupo, p_test)

# Curva ROC glmnet test set -----
colAUC(probs_te$Adicto, test$Grupo, plotROC = T)


## Predicción con Random forest -----

# Predicción RF en el train set -----
p_train_rf <- predict(model_rf, d)
probs_tr_rf <- predict(model_rf, d, type = "prob")

# Matriz de confusión RF train set ----- 
confusionMatrix(d$Grupo, p_train_rf)

# Curva ROC RF train set: overfitting!
colAUC(probs_tr_rf$Adicto, d$Grupo, plotROC = T)

# Predicción RF en test set ------
p_test_rf <-  predict(model_rf, test)
probs_te_rf <- predict(model_rf, test, type = "prob")

# Matriz de confusión RF test -----
confusionMatrix(test$Grupo, p_test_rf)

# Curva ROC RF test ----
colAUC(probs_te_rf$Adicto, test$Grupo, plotROC = T)


## Predicción con Glm ------

# Predicción Glm train set -----
p_train_glm <- predict(glm, d)
probs_tr_glm <- predict(glm, d, type = "prob")

# Matriz de confusión Glm train set ----
confusionMatrix(d$Grupo, p_train_glm)

# Curva ROC Glm train set: overfitted!  ------
colAUC(probs_tr_glm$Adicto, d$Grupo, plotROC = T)

# Predicción Glm test set -----
p_test_glm <- predict(glm, test)
probs_te_glm <- predict(glm, test, type = "prob")

# Matriz de confusión Glm test set ----
confusionMatrix(test$Grupo, p_test_glm)

# Curva ROC Glm test set ------
colAUC(probs_te_glm$Adicto, test$Grupo, plotROC = T)

## Comparación de las tres curvas ROC en test set ----- 

# Datos para curva ROC -----
datos_roc <- data_frame(
  Model = gl(3, 40, labels = c("GlmNet", "Random Forest", "Glm")),
  Diagnostico = rep(test$Grupo, 3),
  D = if_else(Diagnostico == "Adicto", 1, 0),
  Adicto = c(probs_te$Adicto, probs_te_rf$Adicto, probs_te_glm$Adicto)
)
  
# ROC: GlmNet vs Rf vs Glm : Blanco y Negro ------  
ggplot(datos_roc, aes(d = D, m = Adicto, lty = Model, shape = Model)) +
  geom_roc(linealpha = 0.6, 
           pointsize = 0.6, 
           n.cuts = 10,
           labels = T,
           labelsize = 3,
           pointalpha = 1) +
  style_roc() +
  scale_linetype_manual(values=c("dashed", 
                                 "solid", 
                                 "longdash")) +
  labs(title = "ROC (Test Set)") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))

# ROC: GlmNet vs Rf vs Glm : Color ------  
ggplot(datos_roc, aes(d = D, 
                      m = Adicto, 
                      lty = Model, 
                      shape = Model,
                      col = Model)) +
  geom_roc(linealpha = 0.8, 
           pointsize = 0.6, 
           n.cuts = 10,
           labels = T,
           labelsize = 3,
           pointalpha = 1) +
  scale_color_manual(values = c("#EE9A00", 
                                "#3CB371", 
                                "#1874CD")) +
  scale_linetype_manual(values=c("dashed", 
                                 "solid", 
                                 "longdash")) +
  style_roc() +
  labs(title = "ROC (Test Set)") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold")) 

