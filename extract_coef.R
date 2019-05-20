
# Extract coefficients  ---------------------------------------------------


# Packages ----------------------------------------------------------------

pacman::p_load(caret, tidyverse, caTools, gridExtra, 
               ranger, mlbench, AppliedPredictiveModeling,
               plotROC, glmnet, selectiveInference)



# Data --------------------------------------------------------------------

load("adicciones.RData")



# Betas -------------------------------------------------------------------

coef(glmnet$finalModel, s = glmnet$bestTune$lambda) %>% 
  broom::tidy() %>% 
  mutate(row1 = reorder(row, value)) %>% 
  slice(-1) %>% 
  ggplot(aes(x = row1, y = value)) +
  geom_point() +
  coord_flip()


betas <- coef(glmnet$finalModel, s = glmnet$bestTune$lambda) %>% 
  broom::tidy() %>% 
  arrange(desc(value)) %>% 
  mutate(value = round(value, 3))



# Selective Inference -----------------------------------------------------

set.seed(43)
n <- nrow(d)
p <- ncol(d[-1])
x <- as.matrix(d[-1])
x <- scale(x, TRUE, TRUE)
y <- ifelse(d$Grupo == "Adicto", 1, 0)

# extract coef for a given lambda; note the 1/n factor!
# (and here  we DO  include the intercept term)
lambda <- glmnet$bestTune$lambda
beta_hat <- coef(glmnet$finalModel, s = glmnet$bestTune$lambda/n)

# compute fixed lambda p-values and selection intervals
out <- fixedLassoInf(x, y, beta_hat, lambda, family = "binomial")
out


