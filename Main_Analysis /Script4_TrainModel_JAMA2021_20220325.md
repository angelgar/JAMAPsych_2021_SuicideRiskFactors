---
title: "Script 4: Run Balanced Random Forest on NESARC Data"
author: "Angel Garcia de la Garza"
date: "March 25th, 2022"
output: html_document
---


```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE, 
                      results = 'asis',
                      cache = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      cache.lazy = FALSE)

library(stringi)
library(pROC)
library(doParallel)
library(beepr)
library(summarytools)
library(psych)
library(knitr)
library(forcats)
library(furrr)
library(randomForest)
library(gbm)
library(e1071)
library(caret)
library(glmnet)
library(tidyverse)




```

## R Markdown


```{r load data, cache = TRUE}

load(here("Data","Processed_Data", "w1w2suicidedata_Clean_May2020.RData"))

```


## SubSample

```{r}

set.seed(1)

w1w2suicide <- w1w2suicide %>%
            dplyr::select(-IDNUM, -remove.unique.cat,
                   -WEIGHT_,
                   -STRATUM_, -PSU_,
                   -CDAY_quantiles_,
                   -CYEAR_,-CMON_,
                   -S1Q24FT_, -S1Q24IN_, -S1Q24LB_quantiles_,
                   -S1Q1E_) %>%
            dplyr::select_if(is.factor) %>%
            dplyr::mutate(suicide_222_ = ifelse(suicide_222_ == 1,
                                                    "yes",
                                                    "no")) %>%
            rename(suicide_222 = suicide_222_) %>%
            mutate(suicide_222 = as.factor(suicide_222),
                   suicide_222 = relevel(suicide_222, "yes")) %>%
            select(-suicide_334_)
            
```



## Debugging Steps

```{r}

lev <- w1w2suicide %>%
          dplyr::mutate_if(is.factor, droplevels) %>%
          lapply(., levels)

## count number of levels
num.lev <- lengths(lev)

rm.num.lev <- which(num.lev == 1)

w1w2suicide <- w1w2suicide %>%
                      dplyr::select(-rm.num.lev)

table(w1w2suicide$suicide_222)

beep()

```


```{r}

rm(lev)
rm(w1vars1)
rm(w1w2_datatype)

gc()

```


# Models 


## Define Sampling Framework

```{r}

smotest <- list(name = "SMOTE with more neighbors!",
                func = function(x, y) {
                        checkInstall("DMwR")
                        library(DMwR)
                        dat <-
                          if (is.data.frame(x)) {
                            if (inherits(x, "tbl_df"))
                                as.data.frame(x)
                                else
                                  x
                          }
                        else
                          as.data.frame(x)
                        dat$.y <- y
                        dat <- SMOTE(.y ~ ., data = dat, perc.under = 150)
                        list(x = dat[,!grepl(".y", colnames(dat), fixed = TRUE), drop = FALSE],
                             y = dat$.y)
                      },
                first = TRUE)

```

## Create Train Matrix

```{r}

set.seed(1)

x_train <- w1w2suicide[-which(names(w1w2suicide) == "suicide_222")]
y_train <- w1w2suicide$suicide_222

folds <- 10
cvIndex_outer <- createFolds(factor(y_train), folds, returnTrain = T)


```


## Caret Balanced Random Forest

```{r}


brf.final.nested.cv <- as.list(rep(NA, 10))

set.seed(1)

rf.sample.fraction = c(222/34653, 222/34653)

cl2 <- makePSOCKcluster(1)
registerDoParallel(cl2)


rangerGrid <-  expand.grid(mtry = c(512,768,1024,1280,1500,1700,1961),
                        splitrule = "gini",
                        min.node.size = c(1))

for (i in 1:10) {
  
  x_train_outer <- x_train[cvIndex_outer[[i]],]
  y_train_outer <- y_train[cvIndex_outer[[i]]]
  cvIndex_inner <- createFolds(factor(y_train_outer), folds, returnTrain = T)
  train.balanced.rf <- caret::train(x = x_train_outer, y = y_train_outer,
                      method = "ranger",
                      tuneGrid = rangerGrid,
                      metric = "ROC",
                      num.trees = 2000,
                      importance = "impurity",
                      num.threads = 10,
                      sample.fraction = rf.sample.fraction,
                      trControl = trainControl(method = "cv",
                                               verboseIter = TRUE,
                                               classProbs = TRUE,
                                               savePredictions = TRUE,
                                               summaryFunction = twoClassSummary,
                                               index = cvIndex_inner))
  brf.final.nested.cv[[i]] <- train.balanced.rf
  
  print(paste("Done with iteration", i))
  
  save.image(here("Data","Model_output", "NESARC_Suicide_BRF_Output_May2020.rda"))
  
}


beep()


## When you are done:
stopCluster(cl2)

```

# Build the Predictions

```{r}

y_test_outer <- rep(NA, 34653)

for (i in 1:10) {
  
  x_test_outer <- x_train[-cvIndex_outer[[i]],]
  
  y_test_outer[-cvIndex_outer[[i]]] <- predict(brf.final.nested.cv[[i]], 
                                               newdata = x_test_outer,
                                               type = "prob")$yes

  
}

pROC::roc(y_train, predictor = y_test_outer)
pROC::roc(y_train, predictor =  prediction.balanced.rf)

```



```{r}

load(here("Data","Processed_Data", "w1w2suicidedata_Clean_May2020.RData"))


library(pROC)
library(PRROC)
library(MLmetrics)



## Generate ROC / Predictions for Random Forest

prediction.balanced.rf <- train.balanced.rf$pred %>% 
                          arrange(rowIndex) %>%
                          filter(mtry == train.balanced.rf$bestTune$mtry,
                                 min.node.size == train.balanced.rf$bestTune$min.node.size)
prediction.balanced.rf <- prediction.balanced.rf$yes

```
