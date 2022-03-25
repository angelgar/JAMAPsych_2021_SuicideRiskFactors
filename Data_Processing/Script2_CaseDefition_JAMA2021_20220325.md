---
title: "Script 2: Define Wave 2 Suicide Attempters"
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
library(here)



```

## R Markdown


```{r load data, cache = TRUE}

load(here("Data","Raw_Data", "w1w2suicidedata_raw_May2020.RData"))

```


# Remove previous attempters

```{r}

w1w2suicide <- w1w2suicide.v2
rm(w1w2suicide.v2)

w1w2suicide.v3 <- w1w2suicide %>%
                mutate(suicide_sli = W2S4AQ3A16,
                       suicide_sli = ifelse(is.na(suicide_sli),0,suicide_sli),
                       suicide_sli = ifelse(suicide_sli == 2,0,suicide_sli),
                       suicide_sli = ifelse(suicide_sli == 9,1,suicide_sli)) %>%
                mutate(suicide_life = ifelse(W2S14Q17A == 1,1,0),
                       suicide_age = ifelse(W2S14Q17C == 0 &  (4 <= W2S14Q17B & W2S14Q17B <= 77),
                                            W2S14Q17B,
                                     ifelse(10 <= W2S14Q17C & W2S14Q17C <= 80,
                                            W2S14Q17C, NA))) %>%
                mutate(age_diff = W2AGE - suicide_age,
                       suicide_new = ifelse(0 <= age_diff & age_diff <= 3,
                                            1, 0),
                       suicide_new = ifelse(is.na(suicide_new), 0, suicide_new))
 

```



```{r}

save(w1w2suicide.v3, file = here("Data","Raw_Data", "w1w2suicidedata_WithCases_May2020.RData"))


```
