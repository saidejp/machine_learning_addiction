#### GLMNET: Adicciones ####
setwd("~/Documents/R")

# Librerias ----
library(caret)
library(ggplot2)
library(dplyr)
library(glmnet)
library(caTools)
library(gridExtra)
library(readxl)
library(readr)
library(ranger)
library(mlbench)
library(AppliedPredictiveModeling)
library(ellipse)
library(tidyr)
library(plotROC)


# Cargar datos ----
d <- read.csv("CONECTOMA.csv", na.strings = c("NaN", "NAN", "NaNN", "NA", "Na"))

# Grupo en factor ----
d$Grupo <- factor(d$Grupo,
                  levels=c(1, 0),
                  labels=c("Adicto", "Control"))   


# Variables seleccionadas ----
d <- d %>% 
  select(Grupo = Grupo,
         Edad = Edad,
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


# Objeto tibble ----
d <- as_tibble(d)

# Quitar los NA ----
d <- na.omit(d)


#### Modelo glmnet: sin resample ####

# La semilla de la bestia
set.seed(666) 

control <- trainControl(
  method = "repeatedcv", number = 10, repeats = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = "final"
)

# Ajustar el modelo 
(glmnet <- train(Grupo ~ .,
               data = d,
               method = "glmnet",
               metric = "ROC",
               preProcess = c("center", "scale"),
               trControl = control))

# Descriptivos model_glm
glmnet_res <- glmnet$results
psych::describe(glmnet_res)

# Desempeño del modelo
#ggplot(model_glmnet)
#plot(model_glmnet$finalModel)

# Predicciones
#p_gorro <- predict(model_glm, type = "raw")

# Matriz de confusión
#caret::confusionMatrix(p_gorro, d$Grupo, positive = "Adicto")

# Curva ROC
#colAUC(as.numeric(p_gorro), d$Grupo, plotROC = TRUE)


##### Modelo glmnet con resampling "up" #####
# http://topepo.github.io/caret/subsampling-for-class-imbalances.html

set.seed(666)

control_up <- trainControl(
  method = "repeatedcv", number = 10, repeats = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = "final",
  sampling = "up"
)

# Ajustar el modelo 
(glmnet_up <- train(Grupo ~ .,
                    data = d,
                    method = "glmnet",
                    metric = "ROC",
                    preProcess = c("center", "scale"),
                    trControl = control_up))


# Descriptivos modelo 2
glmnet_up_res <- glmnet_up$results
psych::describe(glmnet_up_res)


### Modelo glmnet con resampling "down" ####
set.seed(666)

control_down <- trainControl(
  method = "repeatedcv", number = 10, repeats = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  verboseIter = TRUE,
  savePredictions = "final",
  sampling = "down"
)

# Ajustar el modelo 
(glmnet_down <- train(Grupo ~ .,
                     data = d,
                     method = "glmnet",
                     metric = "ROC",
                     preProcess = c("center", "scale"),
                     trControl = control_down))

# Descriptivos model3_glm
glmnet_down_res <- glmnet_down$results
psych::describe(glmnet_down_res)


#### Random Forest ####
set.seed(666)

# Ajustar el modelo 
(model_rf <- train(Grupo ~ .,
                  data = d,
                  method = "ranger",
                  metric = "ROC",
                  preProcess = c("center", "scale"),
                  trControl = control))
  
# Descriptivos model_rf
psych::describe(model_rf$results)


#### Random Forest "up" ######
set.seed(666)

# Ajustar el modelo 
(model_rf_up <- train(Grupo ~ .,
                   data = d,
                   method = "ranger",
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   trControl = control_up))

# Descriptivos random forest
psych::describe(model_rf_up$results)

#ggplot(model_rf)
#plot(model_rf)


#### Random forest "down" ####
set.seed(666)

# Ajustar el modelo 
(model_rf_down <- train(Grupo ~ .,
                      data = d,
                      method = "ranger",
                      metric = "ROC",
                      preProcess = c("center", "scale"),
                      trControl = control_down))


# Descriptivos random forest down
psych::describe(model_rf_down$results)


#### Modelo glm (normal)  ####
set.seed(666)

# Ajustar el modelo 
(glm <- train(Grupo ~ .,
                   data = d,
                   method = "glm",
                   family = binomial("logit"), 
                   maxit = 1000,  
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   trControl = control))

#### Modelo glm "up" ####
set.seed(666)

# Ajustar el modelo 
(glm_up <- train(Grupo ~ .,
                    data = d,
                    method = "glm",
                    family = "binomial",
                    metric = "ROC",
                    preProcess = c("center", "scale"),
                    trControl = control_up))


#### Modelo glm "down" ####
set.seed(666)

# Ajustar el modelo
(glm_down <- train(Grupo ~ .,
                  data = d,
                  method = "glm",
                  family = "binomial",
                  metric = "ROC",
                  preProcess = c("center", "scale"),
                  trControl = control_down))



#### Comparación de modelos ####
lista_modelos <- list(
  glmnet = glmnet,
  glmnet_up = glmnet_up,
  glmnet_down = glmnet_down,
  ran_for = model_rf,
  ran_for_up = model_rf_up,
  ran_for_down = model_rf_down,
  glm = glm,
  glm_up = glm_up,
  glm_down  = glm_down
)

# Función para comparar
resamps <- resamples(lista_modelos)

# Comparación
summary(resamps)

# Comparación de modelos visual
densityplot(resamps, metric = "ROC")
dotplot(resamps, metric = "ROC")
dotplot(resamps, metric = "Spec")
dotplot(resamps, metric = "Sens")
bwplot(resamps, metric = "ROC")
xyplot(resamps, metric = "ROC")

# Diferencias entre modelos
diff <- diff(resamps)
summary(diff)

# Desempeño de todos los modelos
splom(resamps, metric = "ROC")
splom(resamps, metric = "Sens")
splom(resamps, metric = "Spec")


#### Gráficas #####
globales <- as_data_frame(resamps$values)

# Parte en dos la columna resample
globales <- globales %>% 
 separate(Resample, into = c("fold", "rep"))

# Descriptivos ROC
(desc_roc <- globales %>% 
  select(contains("roc")) %>% 
  gather(key = "modelo") %>% 
  group_by(modelo) %>% 
  summarise(media = mean(value),
            sd = sd(value)))

# ROC visual
globales %>% 
  select(contains("roc")) %>% 
  gather(key = "modelo") %>% 
  ggplot(aes(x = value, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(col = modelo)) +
  geom_vline(data = desc_roc, 
             aes(xintercept = media, 
                 col = modelo), 
             lty = 2,
             size = 1) +
  facet_wrap(~modelo, ncol = 3) +
  theme_minimal()

# Descriptivos Sens
(desc_sens <- globales %>% 
  select(contains("sens")) %>% 
  gather(key = "modelo") %>% 
  group_by(modelo) %>% 
  summarise(media = mean(value),
            sd = sd(value)))

# Sensibilidad Visual
globales %>% 
  select(contains("sens")) %>% 
  gather(key = "modelo") %>% 
  ggplot(aes(x = value, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(col = modelo)) +
  geom_vline(data = desc_sens, 
             aes(xintercept = media, 
                 col = modelo), 
             lty = 2,
             size = 1) +
  facet_wrap(~modelo, ncol = 3) +
  theme_minimal()


# Descriptivos especificidad
(desc_spec <- globales %>% 
  select(contains("spec")) %>% 
  gather(key = "modelo") %>% 
  group_by(modelo) %>% 
  summarise(media = mean(value),
            sd = sd(value)))

# Especificidad visual
globales %>% 
  select(contains("spec")) %>% 
  gather(key = "modelo") %>% 
  ggplot(aes(x = value, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(col = modelo)) +
  geom_vline(data = desc_spec, 
             aes(xintercept = media, 
                 col = modelo), 
             lty = 2,
             size = 1) +
  facet_wrap(~modelo, ncol = 3) +
  theme_minimal()

#### Curvas ROC ####

# glmnet: ROC ----
glmnet_p <- predict(glmnet, type = "prob") %>% 
  mutate(Diagnostico = d$Grupo, 
         D = if_else(Diagnostico == "Adicto", 1, 0)) %>% 
  select(Diagnostico, D, everything())

ggplot(glmnet_p, aes(d = D, m = Adicto)) + 
  geom_roc(col = "#EE9A00") +
  labs(title = "Glmnet") +
  style_roc() 

# random forest: ROC ----
ran_for_p <- as_data_frame(model_rf$finalModel$predictions) %>% 
  mutate(Diagnostico = d$Grupo, 
         D = if_else(Diagnostico == "Adicto", 1, 0)) %>% 
  select(Diagnostico, D, everything())

ggplot(ran_for_p, aes(d = D, m = Adicto)) +
  geom_roc(col = "#00CD66") +
  labs(title = "Random Forest") +
  style_roc()

# glmnet vs rf: ROC  ----
curves <- glmnet_p %>% 
  mutate(Diagnostico = d$Grupo, Modelo = "glmnet")  %>% 
  select(Modelo, Diagnostico, Adicto)

curves_1 <- ran_for_p %>% 
  mutate(Diagnostico = d$Grupo, Modelo = "ran_for") %>% 
  select(Modelo, Diagnostico, Adicto)

roc_curves <- curves %>% 
  bind_rows(curves_1) %>% 
  mutate(D = if_else(Diagnostico == "Adicto", 1, 0)) %>% 
  select(Modelo, Diagnostico, D, Adicto)


roc_curves$Modelo <- factor(roc_curves$Modelo,
                            levels = c("ran_for",
                                       "glmnet"),
                            labels = c("Random Forest",
                                       "GlmNet"))
roc_curves <- roc_curves %>% 
  rename(Model = Modelo)

# Color
ggplot(roc_curves, aes(d = D, m = Adicto, col = Model, lty = Model)) +
  geom_roc() +
  scale_color_manual(values = c("#00CD66", "#EE9A00")) +
  style_roc() +
  labs(title = "ROC") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold"))
# Blanco y Negro
ggplot(roc_curves, aes(d = D, m = Adicto, lty = Model, shape = Model)) +
  geom_roc(pointsize = .6) +
  style_roc() +
  labs(title = "ROC") +
  theme(plot.title = element_text(hjust = 0.5,
                                  face = "bold")) 


