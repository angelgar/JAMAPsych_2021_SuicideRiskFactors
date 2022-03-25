---
title: "Script 1: Merge Raw SAS Wave1 and Wave2 Datasets"
author: "Angel Garcia de la Garza"
date: "March 25th, 2022"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(haven)
library(here)

```



```{r}

## Load Data

w1mother <- haven::read_sas(here("Data","Raw_Data", "w1mother.sas7bdat"))
w2mother <- read_csv(here("Data","Raw_Data", "w2mother_suicideonly.csv"))
load(here("Data","Raw_Data", "w1w2suicidedata.RData"))

## Generate Names

names(w1mother) <- toupper(names(w1mother))
keep <- toupper(names(w1mother)) %in% c(w1vars1$code, "INCPER3", "INCFAM3")

## Add Income Variables INCPER3 INCFAM3

not.mother <-  w1vars1$code[!(w1vars1$code %in% toupper(names(w1mother)))]

## Filter and Merge

w1mother.codebook <- w1mother %>% select(which(keep == TRUE)) ## I updated this in Feb 2020 cause code was broken
              
w1w2suicide.v2 <- w2mother %>%
                  select(IDNUM, W2S4AQ3A16, W2S14Q17A:W2S14Q17C, W2AGE) %>%
                  left_join(w1mother.codebook, by = "IDNUM")

save("w1w2suicide.v2", 
     file = here("Data","Raw_Data", "w1w2suicidedata_raw_May2020.RData"))

```
