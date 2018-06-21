# Extract coefficients 
coef(glmnet$finalModel, s = glmnet$bestTune$lambda) %>% 
  broom::tidy() %>% 
  mutate(row1 = reorder(row, value)) %>% 
  slice(-1) %>% 
  ggplot(aes(x = row1, y = value)) +
  geom_point() +
  coord_flip()


betas <- 
coef(glmnet$finalModel, s = glmnet$bestTune$lambda) %>% 
  broom::tidy() %>% 
  arrange(desc(value)) %>% 
  mutate(value = round(value, 3))


writexl::write_xlsx(betas, path = "~/Google Drive/Paper_EJN/betas.xlsx")
