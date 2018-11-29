# Gráficas para paper
load("adicciones.Rdata")

# Cambia el formato de los datos a tidy
glob_tidy <- globales %>% 
  select(-contains("up"), -contains("down"), -fold, -rep) %>% 
  gather(key = "modelo") 

# El modelo como factor
glob_tidy$modelo <- factor(glob_tidy$modelo, 
                           levels = c("glm~ROC",      
                                      "glm~Sens",     
                                      "glm~Spec",
                                      "ran_for~ROC",
                                      "ran_for~Sens",
                                      "ran_for~Spec",
                                      "glmnet~ROC",   
                                      "glmnet~Sens",  
                                      "glmnet~Spec"),
                           labels = c("Glm - ROC",
                                      "Glm - Sens",
                                      "Glm - Spec",
                                      "Rf - ROC",
                                      "Rf - Sens",
                                      "Rf - Spec",
                                      "GlmNet - ROC",
                                      "GlmNet - Sens",
                                      "GlmNet - Spec"))
# Descriptivos de los indicadores
glob_tidy_desc <- glob_tidy %>% 
  group_by(modelo) %>% 
  summarise(media = mean(value),
            sd = sd(value))


# Indicadores de todos los modelos : Color
ggplot(glob_tidy, aes(x = value, fill = modelo)) +
  geom_density(alpha = 0.5) +
  geom_rug(aes(col = modelo)) +
  geom_vline(data = glob_tidy_desc, 
             aes(xintercept = media, 
                 col = modelo), 
             lty = 2,
             size = 1) +
  facet_wrap(~modelo, 
             ncol = 3,
             scales = "free") +
  labs(x = "",
       y = "Density") +
  scale_x_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 3.5, 1),
                     limits = c(0, 3.5)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))


# Indicadores de todos los modelos : blanco y negro
ggplot(glob_tidy, aes(x = value, fill = modelo)) +
  geom_density(alpha = 0.5, fill = "dimgray") +
  geom_rug() +
  geom_vline(data = glob_tidy_desc, 
             aes(xintercept = media), 
             lty = 2,
             size = 1) +
  facet_wrap(~modelo, 
             ncol = 3,
             scales = "free") +
  labs(x = "",
       y = "Density") +
  scale_x_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00),
                     limits = c(0, 1)) +
  scale_y_continuous(breaks = seq(0, 3.5, 1),
                     limits = c(0, 3.5)) +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))



# Estimadores puntuales -----
todos <- globales %>% 
  select(-fold, -rep) %>% 
  gather(key = "modelo") %>%
  separate(modelo, into = c("model", "stat"), sep = "~")
  
# Modelo como factor
todos$model <- factor(todos$model, 
                      levels = c("glm",  
                                 "glm_up", 
                                 "glm_down",                  
                                 "ran_for",      
                                 "ran_for_up", 
                                 "ran_for_down",
                                 "glmnet",  
                                 "glmnet_up",
                                 "glmnet_down"),
                      labels = c("Glm",
                                 "Glm - up",
                                 "Glm - down",
                                 "Rf",
                                 "Rf - up",
                                 "Rf - down",
                                 "GlmNet",
                                 "GlmNet - up",
                                 "GlmNet - down"))
# Estadístico como factor
todos$stat <- factor(todos$stat,
                     levels = c("ROC",
                                "Sens",
                                "Spec"),
                     labels = c("ROC",
                                "Sensitivity",
                                "Specificity"))

# Descriptivos de todos los modelos
todos %>% 
  group_by(model, stat) %>% 
  summarise(media = mean(value),
            sd = sd(value)) %>% 
  ungroup() 



library(Hmisc)

grafica <- function(indicador = "ROC") { 
todos %>% 
  filter(stat == indicador) %>% 
  ggplot(aes(x = model, y = value, col = model)) +
    geom_point(alpha = 0.3) +
    stat_summary(fun.data =  "mean_cl_normal", 
                 col = "black", 
                 shape = 5) +
    geom_vline(xintercept = c(3.5, 6.5), 
               lty = 2,
               alpha = 0.3) +
    labs(x = "Model",
       y = indicador) +
    theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 9.5)) +
    coord_flip() +
    scale_x_discrete(limits = rev(levels(todos$model)))
}

grafica("Sensitivity")  
grafica("Specificity")

roc_col <- grafica()

todos <- todos %>% 
  mutate(family = gl(3, 900, labels = c("GlmNet", "Rf", "Glm"))) %>% 
  select(family, everything()) 

# ROC blanco y negro
# Tamaño máximo 9.05 inches x 7.25 inches
roc <- 
todos %>% 
  filter(stat == "ROC") %>% 
  ggplot(aes(x = model, y = value)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun.data =  "mean_cl_normal", 
               col = "black", 
               shape = 5) +
  geom_vline(xintercept = c(3.5, 6.5), 
             lty = 2,
             alpha = 0.3) +
  labs(x = "Model",
       y = "ROC") +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 9.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(todos$model))) 

# Sensitivity
sens <- 
todos %>% 
  filter(stat == "Sensitivity") %>% 
  ggplot(aes(x = model, y = value, shape = family)) +
  #geom_boxplot(alpha = 0.5) +
  stat_summary(fun.data = "mean_cl_normal", col = "gray") +
  geom_vline(xintercept = c(3.5, 6.5), lty = 2) +
  labs(x = "Model",
       y = "Sensitivity")  +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "white"),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 9.5)) +
  scale_y_continuous(breaks = c(0.0, 0.25, 0.50, 0.75, 1.00),
                     limits = c(0.0, 1)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(todos$model))) 
