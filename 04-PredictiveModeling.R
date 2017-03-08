library(readr)
library(glmnet)
library(tidyverse)
library(caret)

ri.padel <- read_csv("../Dropbox/RStudio/RRecoding/03-PaDEL-Descriptor and Rekharsky and Inoue.csv")

ri.padel.bpredictors <-  ri.padel %>%
  filter(host == "Beta-CD") %>% 
  filter(!is.na(log.K)) %>%
  select(-X1:-T.K, -log.K.Uncertainty:-`bind.aff, kcal/mol`)

ri.padel.bthermodynamics <-  ri.padel %>%
  filter(host == "Beta-CD") %>%
  select(X1:T.K, log.K.Uncertainty:`bind.aff, kcal/mol`)


sparse.ri <- sparse.model.matrix(~., ri.padel.bpredictors)

set.seed(1001)
train.index <- sample(x = 1:nrow(sparse.ri), size = round(0.75*nrow(sparse.ri)))

train <- sparse.ri[train.index,]
test <- sparse.ri[-train.index,]


glm.model <- cv.glmnet(x = train[,-1:-2], y = train[, 2])
predict(glm.model, test[,-1:-2]) %>% 
  cbind(test[,2]) %>% 
  data.frame() %>% 
  rename(predicted = X1, experimental = V2) %>%
  ggplot(., aes(x = experimental, y = predicted))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)


# Trial 2 with DeltaG

ri.padel.bpredictors.2 <-  ri.padel %>%
  filter(host == "Beta-CD") %>% 
  filter(!is.na(DelG)) %>%
  select(-X1:-log.K.Uncertainty, -DelG.Uncertainty:-`bind.aff, kcal/mol`)

sparse.ri.2 <- sparse.model.matrix(~., ri.padel.bpredictors.2)

train.2 <- sparse.ri.2[train.index,]
test.2 <- sparse.ri.2[-train.index,]

glm.model.2 <- cv.glmnet(x = train.2[,-1:-2], y = train.2[, 2])
predict(glm.model.2, test.2[,-1:-2]) %>% 
  cbind(test.2[,2]) %>% 
  data.frame() %>% 
  rename(predicted = X1, experimental = V2) %>%
  ggplot(., aes(x = experimental, y = predicted))+
  geom_point()+
  geom_abline(slope = 1, intercept = 0)

