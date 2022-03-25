---
title: "Script 5: Main Figures"
author: "Angel Garcia de la Garza"
date: "March 25th, 2022"
output: html_document
output:
  html_document:
    code_folding: hide
    toc: yes
    toc_float: yes
hitheme: tomorrow
highlighter: highlight.js
editor_options: 
  chunk_output_type: console
---




```{r setup, include=FALSE}

knitr::opts_chunk$set(echo = TRUE, 
                      results = 'asis',
                      cache = TRUE,
                      message = FALSE,
                      warning = FALSE,
                      cache.lazy = FALSE,
                      dpi= 100)

library(ggridges)
library(pROC)
library(doParallel)
library(beepr)
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



```{r load}

load(here("Data","Model_output", "w1w2suicidedata_Clean_May2020.RData"))

```

## Table 1: Section by Section Summary

```{r table_1, cache = T, eval= F}

w1w2suicide <- w1w2suicide.raw %>%
                select(-IDNUM, -suicide_222_)
rm(w1w2suicide.raw)

names.questions <- names(w1w2suicide)

nesarc.sections <- c("S1[Q]", "S2AQ", "S2BQ", "S2CQ", "S2DQ",
                     "S3AQ", "S3B[D-Q]", "S3C[D-Q]", "S3DQ", "S3EQ",
                     "S4AQ", "S4BQ", "S4CQ", "S5Q", "S6Q",
                     "S7Q", "S8Q", "S9Q", "S10Q", 
                     "S11AQ","S11BQ", "S12[Q-T]", "S13Q", "S14Q")

count.section <- NA
missing.section <- NA
index.all <- NA


for (i in nesarc.sections) {
  
  specific.section <- NA
  index.section <- which(str_starts(names.questions, i))
  
  if (i == "S1[Q]") {
    
    specific.section <- which(str_starts(names.questions, "CHLD"))
    specific.section <- c(specific.section, which(str_starts(names.questions, "NUM")))
    specific.section <- c(specific.section, 
                          which(names.questions %in% 
                                  c("CDAY_quantiles_", "AGE_quantiles_",
                                    "SEX_", "MOTHERIH_",
                                    "FATHERIH_", "SPOUSE_",
                                    "CMON_","CYEAR_","STRATUM_","MARITAL_",
                                    "ADULTCH_","OTHREL_","NONREL_quantiles_")))
    index.section <- c(index.section, specific.section)
    
  } else if (i == "S2AQ") {
    
    specific.section <- which(str_detect(names.questions, "ECF"))
    index.section <- c(index.section, specific.section)
    
  } else if (i == "S3AQ") {
    
    specific.section <- which(str_detect(names.questions, "TBTYPE"))
    specific.section <- c(specific.section, which(str_detect(names.questions, "CHECK32")))
    index.section <- c(index.section, specific.section)
    
  }
  
  count.section <- c(count.section, length(index.section))
  index.all <- c(index.all, index.section)
  
  subject.missing <- NA
  
  library(foreach)
  library(doParallel)

  # setup parallel backend to use many processors
  cores=detectCores()
  cl <- makeCluster(cores[1]-1) #not to overload your computer
  registerDoParallel(cl)
  
  subject.missing <- foreach(j = 1:dim(w1w2suicide)[1], .combine=cbind) %dopar% {
   sum(w1w2suicide[j, index.section] == "(Missing)")
  }
  
  # stop cluster
  stopCluster(cl)
  
  missing.section <- c(missing.section, max(subject.missing, na.rm = T))
  
  print(i)
  
}

subject.missing <- NA

View(names(w1w2suicide[-index.all[-1]]))

cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

subject.missing <- foreach(j = 1:dim(w1w2suicide)[1], .combine=cbind) %dopar% {
    sum(w1w2suicide[j, -index.all[-1]] == "(Missing)")
}

stopCluster(cl)


input.var.names <- names(w1w2suicide[-index.all[-1]])


cores=detectCores()
cl <- makeCluster(cores[1]-1) #not to overload your computer
registerDoParallel(cl)

input.var.missing <- foreach(j = input.var.names, .combine=cbind) %dopar% {
    sum(w1w2suicide[, j] == "(Missing)")
}

stopCluster(cl)

nesarc.sections <- c(nesarc.sections, "other")
count.section <- c(count.section, 2985 - length(index.all)+1)
missing.section <- c(missing.section, max(subject.missing, na.rm = T))

output.nesarc.structure <- as.data.frame(cbind(nesarc.sections, 
                                               count.section[-1], 
                                               missing.section[-1])) %>%
                            rename(Question_Num = V2,
                                   Max_Missing = V3) %>%
                            mutate(Question_Num = as.numeric(as.character(Question_Num)),
                                   Max_Missing = as.numeric(as.character(Max_Missing)),
                                   Min_Answered = Question_Num - Max_Missing)


## Note!!!  "S1F9C_"              "S1F9B_"              "S1F9A_"              "S1F7D_"              "S1F7C_"  "CKSTATUS_"
## They are not truly missing they are just does not apply
input.var.names[input.var.missing[1, ] != 0]

write_csv(output.nesarc.structure, here("Data","Model_output", "Nnesarc_counts.csv"))

```


```{r load}


load(here("Data","Model_output", "NESARC_Suicide_BRF_Output_May2020.rda"))
load(here("Data","Processed_Data", "w1w2suicidedata_Clean_May2020.RData"))

```



```{r, eval = F}

## Loading the Weights @ W2

library(haven)

wave2 <- read_sas(here("Data","Raw_Data", "w2mother.sas7bdat"))

wave2_weights <- wave2 %>%
                    as_tibble() %>%
                    select("IDNUM",
                           "W2WEIGHT")

write_csv(wave2_weights, here("Data","Processed_Data", "NESARC_W2_Weights.csv"))


```


```{r}

wave2_weights <- read_csv(here("Data","Processed_Data", "NESARC_W2_Weights.csv"))

w2_cases_weights <- w1w2suicide.raw %>%
                      select(IDNUM, suicide_222_) %>%
                      mutate(IDNUM = as.numeric(as.character(IDNUM))) %>%
                      left_join(wave2_weights, by = "IDNUM") %>%
                      mutate(W2WEIGHT = W2WEIGHT / sum(W2WEIGHT) * n())

```




```{r}

library(pROC)
library(PRROC)
library(MLmetrics)



## Generate ROC / Predictions for Random Forest

prediction.balanced.rf <- train.balanced.rf$pred %>% 
                          arrange(rowIndex) %>%
                          filter(mtry == train.balanced.rf$bestTune$mtry,
                                 min.node.size == train.balanced.rf$bestTune$min.node.size)
prediction.balanced.rf <- prediction.balanced.rf$yes


data.pred.prob <- w2_cases_weights %>%
                    mutate(prediction.balanced.rf = prediction.balanced.rf) %>%
                    rename(suicide = suicide_222_,
                           weight = W2WEIGHT)


```





# Figure 1: Performance of Model across thresholds

```{r}

calculate.vals <- function(threshold) {
  
  data <- data.pred.prob %>%
            mutate(prediction.bf = ifelse(prediction.balanced.rf > threshold,1,0)) %>%
            ## Create statistic at each set threshold
            summarize(Accuracy.rf = sum(weight[prediction.bf == suicide]) / n(),
                      Sensitivity.rf = sum(weight[(prediction.bf == suicide) & suicide == 1]) / sum(weight[suicide == 1]),
                      Specificity.rf = sum(weight[(prediction.bf == suicide) & suicide == 0]) / sum(weight[suicide == 0]),
                      PPV.rf = sum(weight[(prediction.bf == suicide) & suicide == 1]) / sum(weight[prediction.bf == 1]),
                      PPVDen.rf = sum(weight[prediction.bf == 1]),
                      PPVSE.rf = sqrt(PPV.rf * (1-PPV.rf) / sum(weight[prediction.bf == 1])),
                      NPV.rf = sum(weight[(prediction.bf == suicide) & suicide == 0]) / sum(weight[prediction.bf == 0]),
                      Alarms.rf = sum(weight[prediction.bf == 1]) / n() * 100,
                      NNE.rf = sum(weight[prediction.bf == 1]) / sum(weight[(prediction.bf == suicide) & suicide == 1])) %>%
            mutate(threshold = threshold)
  
  return(data)

}


cut.off <- data.frame(cutoff = sort(c(seq(0,1, 0.00125), 0.523)))

output <- cut.off$cutoff %>%
            map_dfr( ~ calculate.vals(threshold = .x)) %>%
            gather(type, statistic, -threshold) %>%
            separate(type, c("type","model"), "\\.") %>%
            spread(type, statistic)

output <- output %>%
          filter(model == "rf")

```





```{r}

pr.roc.plot1 <- output %>%
                  ggplot(aes(y=PPV, x=Sensitivity, fill = model, color = model)) +
                    geom_line() +
                    geom_ribbon(aes(ymin = PPV - 1.96*PPVSE, ymax = PPV + 1.96*PPVSE), alpha = 0.5) +
                    theme_bw() + 
                    scale_color_manual(name = "Model", values = c("#4392F1"),
                                       labels = c("Random Forest")) +
                    scale_fill_manual(name = "Model", values = c("#4392F1"),
                                       labels = c("Random Forest")) +
                    xlim(0.25,1) + 
                    scale_y_log10()

pr.roc.plot2 <- output %>%
                    filter(Sensitivity > 0.02) %>%
                    ggplot(aes(y=PPV, x=Sensitivity, fill = model, color = model)) +
                      geom_line() +
                      geom_ribbon(aes(ymin = PPV - 1.96*PPVSE, ymax = PPV + 1.96*PPVSE), alpha = 0.5) +
                      theme_bw() + 
                    scale_color_manual(name = "Model", values = c("#4392F1"),
                                       labels = c("Random Forest")) +
                    scale_fill_manual(name = "Model", values = c("#4392F1"),
                                       labels = c("Random Forest")) +
                    scale_y_continuous(trans='log10')
    
pr.roc.plot3 <- output %>%
                    ggplot(aes(y=PPV, x=Sensitivity, fill = model, color = model)) +
                      geom_line() +
                      geom_ribbon(aes(ymin = PPV - 1.96*PPVSE, ymax = PPV + 1.96*PPVSE), alpha = 0.5) +
                      theme_bw() + 
                      scale_color_manual(name = "Model", values = c("#4392F1"),
                                         labels = c("Random Forest")) +
                      scale_fill_manual(name = "Model", values = c("#4392F1"),
                                         labels = c("Random Forest")) + ylim(0,1) +
                      scale_y_continuous(trans='log10')


PR.AUC.RF <- output %>%
              filter(model == "rf") %>%
              select(Sensitivity, PPV) %>%
              unique() %>%
              arrange(desc(Sensitivity)) %>%
              filter(!is.nan(PPV)) %>%
              group_by(Sensitivity) %>%
              summarize(mean.PPV = mean(PPV, na.rm = T)) %>%
              mutate(lag.Sens = lag(Sensitivity),
                     delta.Sens = Sensitivity - lag.Sens) %>%
              filter(!is.na(delta.Sens))

PR.AUC.RF <- (PR.AUC.RF$mean.PPV %*% PR.AUC.RF$delta.Sens)

```


### Sensitivity and Specificity Curves

```{r}

sensitivity.plot <- output %>%
                    ggplot(aes(x=threshold, y=Sensitivity, color=model)) +
                      geom_line() +
                      theme_bw() + xlab("Threshold") + 
                      scale_color_manual(name = "Model", values = c("#4392F1"),
                                         labels = c("Random Forest"))

specificity.plot <- output %>%
                    ggplot(aes(x=threshold, y=Specificity, color=model)) +
                      geom_line() +
                      theme_bw() + xlab("Threshold") + 
                      scale_color_manual(name = "Model", values = c("#4392F1"),
                                         labels = c("Random Forest"))


```

### Number Needed to Evaluate and Alerts Per 100

```{r}

nne.plot <- output %>%
            ggplot(aes(x=threshold, y=NNE, color=model)) +
              geom_line() +
              theme_bw() + xlab("Threshold") + 
              scale_color_manual(name = "Model", values = c("#4392F1"),
                                 labels = c("Random Forest")) + ylab("NNE")

alarms.plot <- output %>%
                ggplot(aes(x=threshold, y=Alarms, color=model)) +
                  geom_line() +
                  theme_bw() + xlab("Threshold") + 
                  scale_color_manual(name = "Model", values = c("#4392F1"),
                                     labels = c("Random Forest")) + ylab("Alarms Per 100 Evaluations")

```


### PPV and NPV

```{r, }

ppv.plot <- output %>%
              ggplot(aes(x=threshold, y=PPV, color=model)) +
                geom_line() +
                theme_bw() + xlab("Threshold") + 
                scale_color_manual(name = "Model", values = c("#4392F1"),
                                   labels = c("Random Forest"))

```


```{r, }

npv.plot <- output %>%
              ggplot(aes(x=threshold, y=NPV, color=model)) +
                geom_line() +
                theme_bw() + xlab("Threshold") + 
                scale_color_manual(name = "Model", values = c("#4392F1"),
                                   labels = c("Random Forest"))

```


### Overall Accuracy 

```{r, }

accuracy.plot <- output %>%
              ggplot(aes(x=threshold, y=Accuracy, color=model)) +
                geom_line() +
                theme_bw() + xlab("Threshold") + 
                scale_color_manual(name = "Model", values = c("#4392F1"), 
                                   labels = c("Hoertel Regression",
                                                "Random Forest"))

```

### Assemble the Entire Plot


```{r}

## Create Thresholds

library(cutpointr)

output <- output %>%
          mutate(youden = Sensitivity + Specificity - 1)

threshold.low <- output$threshold[which(output$youden == max(output$youden))]

library(reldist)

threshold.med <- wtd.quantile(data.pred.prob$prediction.balanced.rf, 0.9, weight = data.pred.prob$weight) %>%
                 as.numeric()

threshold.high <- min(output$threshold[which(output$PPV > 0.1)])

```



```{r}

library(ggpubr)
library(ggrepel)

annotate.data <- output %>%
                  filter(threshold == threshold.low |
                         threshold == threshold.med |
                         threshold == threshold.high) %>%
                  mutate(group = c("Low / Medium",
                                   "Medium / High",
                                   "High / Very High"),
                         sens.label = paste("Sensitivty =", round(Sensitivity,3)),
                         spec.label = paste("Sensitivty =", round(Specificity,3)),
                         ppv.label = paste("PPV =", round(PPV,3)),
                         npv.label = paste("NPV =", round(NPV,3)),
                         alarms.label = paste("Alarms Per 100 =", round(Alarms)),
                         nne.label = paste("NNE =", round(NNE)),
                         roc.label = paste(group, sens.label, spec.label, sep = " \n "),
                         alarms.label = paste(group, alarms.label, sep = " \n "),
                         ppv.label = paste(group, ppv.label, sep = " \n "),
                         npv.label = paste(group, npv.label, sep = " \n "),
                         sens.label = paste(group, sens.label, sep = " \n "),
                         spec.label = paste(group, spec.label, sep = " \n "),
                         nne.label = paste(group, nne.label, sep = " \n "))

```


```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_ppv_plot.pdf"), width = 4, height = 4)

ppv.plot + 
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$ppv.label,
    x = annotate.data$threshold,
    y = log10(annotate.data$PPV),
    segment.color = "grey50",
    size = 5, 
    box.padding = 2.1,
    direction = "y",
    show.legend = FALSE
  ) + scale_y_continuous(trans='log10') +  guides(fill=FALSE,color = FALSE) + 
  ylab("Positive Predictive Value") + theme(text = element_text(size=20))


dev.off()


```


```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_npv_plot.pdf"), width = 4, height = 4)

npv.plot +
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$npv.label,
    x = annotate.data$threshold,
    y = annotate.data$NPV,
    segment.color = "grey50",
    size = 5, 
    box.padding = 2.4,
    direction = "x",
    show.legend = FALSE
  ) + guides(fill=FALSE,color = FALSE) + ylab("Negative Predictive Value")  + theme(text = element_text(size=20))


dev.off()


```


```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_alarms_plot.pdf"), width = 4, height = 4)

alarms.plot  +
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$alarms.label,
    x = annotate.data$threshold,
    y = annotate.data$Alarms,
    segment.color = "grey50",
    size = 2.5, 
    box.padding = 2.2,
    direction = "y",
    show.legend = FALSE
  ) + guides(fill=FALSE,color = FALSE)  + theme(text = element_text(size=20))


dev.off()


```



```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_nne_plot.pdf"), width = 4, height = 4)

nne.plot  +
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$nne.label,
    x = annotate.data$threshold,
    y = annotate.data$NNE,
    segment.color = "grey50",
    size = 2.5, 
    box.padding = 2.2,
    direction = "y",
    show.legend = FALSE
  ) + ylab("Number Needed to Evaluate") + guides(fill=FALSE,color = FALSE)  + theme(text = element_text(size=20))


dev.off()


```



```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_specificity_plot.pdf"), width = 4, height = 4)

specificity.plot + 
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$spec.label,
    x = annotate.data$threshold,
    y = annotate.data$Specificity,
    segment.color = "grey50",
    size = 2.5, 
    box.padding = 2.25,
    direction = "y",
    show.legend = FALSE
  ) + guides(fill=FALSE,color = FALSE)  + theme(text = element_text(size=20))

dev.off()


```


```{r, eval = F}

pdf(here("Data", "Model_output", "figure1_sensitivity_plot.pdf"), width = 4, height = 4)

sensitivity.plot + 
  geom_label_repel(
    data = annotate.data,
    label = annotate.data$sens.label,
    x = annotate.data$threshold,
    y = annotate.data$Sensitivity,
    segment.color = "grey50",
    size = 2.5, 
    box.padding = 1.80,
    direction = "x",
    show.legend = FALSE
  ) + guides(fill=FALSE,color = FALSE)  + theme(text = element_text(size=20))

dev.off()


```



# Table 1: Risk Group Summary

```{r}

data <- data.pred.prob %>%
            mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                                ifelse(between(prediction.balanced.rf,
                                               threshold.low, threshold.med),
                                       "medium",
                                ifelse(between(prediction.balanced.rf,
                                               threshold.med, threshold.high),
                                       "high", "very high"))),
                   prediction.bf = ifelse(risk.group == "low",
                                          0, 1)) %>%
            group_by(risk.group) %>%
            summarize(Total = n(),
                      N.Cases = sum(suicide == 1),
                      N.Controls = sum(suicide == 0),
                      Prop.Cases = N.Cases / Total,
                      Prop.Controls = N.Controls / Total,
                      Mean.Score = mean(prediction.balanced.rf) * 100,
                      Wght.Total = sum(weight),
                      Wght.Cases = sum(weight[suicide == 1]),
                      Wght.Controls = sum(weight[suicide == 0]),
                      Wght.Prop.Cases = Wght.Cases / Wght.Total,
                      Wght.Prop.Controls = Wght.Controls / Wght.Total,
                      Wght.Mean.Score = Hmisc::wtd.mean(prediction.balanced.rf, weights = weight)) %>%
            ungroup %>%
            slice(2,3,1,4)

data %>% 
  select(risk.group:Wght.Mean.Score) %>%
  kable(digits = 4, 
               caption = "Summary statistics of Risk Groups",
               col.names = c("Risk Group",
                             "Total",
                             "N Cases",
                             "N Controls",
                             "Prop Cases",
                             "Prop Controls",
                             "Mean Score",
                             "Weighted Total",
                             "Weighted N Cases",
                             "Weighted N Controls",
                             "Weighted Prop Cases",
                             "Weighted Prop Controls",
                             "Weighted Mean Score"))

data %>% write_csv(here("Data", "Model_output", "Table1.csv"))

percent.w2wave <- data$Total / dim(data.pred.prob)[1]



data.tot <- data.pred.prob %>%
            mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                                ifelse(between(prediction.balanced.rf,
                                               threshold.low, threshold.med),
                                       "medium",
                                ifelse(between(prediction.balanced.rf,
                                               threshold.med, threshold.high),
                                       "high", "very high"))),
                   prediction.bf = ifelse(risk.group == "low",
                                          0, 1)) %>%
            summarize(Total = n(),
                      N.Cases = sum(suicide == 1),
                      N.Controls = sum(suicide == 0),
                      Prop.Cases = N.Cases / Total,
                      Prop.Controls = N.Controls / Total,
                      Mean.Score = mean(prediction.balanced.rf) * 100,
                      Wght.Total = sum(weight),
                      Wght.Cases = sum(weight[suicide == 1]),
                      Wght.Controls = sum(weight[suicide == 0]),
                      Wght.Prop.Cases = Wght.Cases / Wght.Total,
                      Wght.Prop.Controls = Wght.Controls / Wght.Total,
                      Wght.Mean.Score = Hmisc::wtd.mean(prediction.balanced.rf, weights = weight)) %>%
            ungroup %>%
            slice(2,3,1,4)


```


# Figure 2: Predictions Broken Down by Risk

```{r}

data.pred.prob %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.low, threshold.med),
                                     "medium",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.med, threshold.high),
                                     "high", "very high"))),
         risk.group = as.factor(risk.group), 
         risk.group = relevel(risk.group, "medium"),
         risk.group = relevel(risk.group, "low")) %>%
  
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = "Test 2", fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density") +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  xlab("Score") + scale_y_discrete(expand = expand_scale(add = c(0, 5))) +
  xlim(0,1) +
  ylab("Density") +  theme_bw() + theme(axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank()) -> p1

p1



```




```{r}

data.pred.prob %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.low, threshold.med),
                                     "medium",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.med, threshold.high),
                                     "high", "very high"))),
         risk.group = as.factor(risk.group), 
         risk.group = relevel(risk.group, "medium"),
         risk.group = relevel(risk.group, "low")) %>%
  ggplot(aes(y="dummy", x=prediction.balanced.rf,
             fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) + 
  stat_density_ridges(geom = "density_ridges_gradient", scale = 1, trim = T) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  xlab("Score") + scale_y_discrete(expand = expand_scale(add = c(0, 2.7))) +
  ylab("Density") +  theme_bw() + theme(axis.text.y=element_blank(),
                                        axis.ticks.y=element_blank()) -> p3

p3 + xlim(0,1)



```

This is what the predictions look like broken down by risk. 

```{r}



data.pred.prob %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.low, threshold.med),
                                     "medium",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.med, threshold.high),
                                     "high", "very high"))),
         risk.group = as.factor(risk.group), 
         risk.group = relevel(risk.group, "medium"),
         risk.group = relevel(risk.group, "low"),
         suicide = ifelse(suicide == 1, "Yes", "No")) %>%
         group_by(suicide) %>%
         summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            count = n(),
            mean.prediction = mean(prediction.balanced.rf),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n(),
            Wght.Mean.Score = Hmisc::wtd.mean(prediction.balanced.rf, weights = weight)) %>%
  kable(digits = 4)






data.pred.prob %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.low, threshold.med),
                                     "medium",
                      ifelse(between(prediction.balanced.rf,
                                     threshold.med, threshold.high),
                                     "high", "very high"))),
         risk.group = as.factor(risk.group), 
         risk.group = relevel(risk.group, "medium"),
         risk.group = relevel(risk.group, "low"),
         suicide = ifelse(suicide == 1, "Yes", "No")) %>%
  
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = suicide, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.8, bw = 0.048) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  xlab("Score") + scale_y_discrete(expand = expand_scale(add = c(0, .7))) +
  ylab("Attempted Suicide at Wave 2") + theme_bw() + xlim(0, 1) -> p2

p2


ggarrange(p1 + ylab("Overall"), p2 + ylab("Future Suicide Attempt at Wave 2"), 
          labels = c("A", "B"), ncol = 2,
          common.legend = TRUE, legend = "bottom")
```

# Table 2: Variable Importance for Random Forest


# Figure 3: Appendix Mean Response Plots

For all of these plots, I calculated the mean response of all respondents that answered the question the same way. 


```{r}

data.partial <- cbind(x_train, data.pred.prob)

data.partial <- data.partial %>%
                  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.low, threshold.med),
                                        "medium",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.med, threshold.high),
                                        "high", "very high"))),
                       risk.group = as.factor(risk.group), 
                       risk.group = relevel(risk.group, "medium"),
                       risk.group = relevel(risk.group, "low"))

```


### Felt like wanted to die

```{r}

pdf(here("Data", "Model_output", "figure5.pdf"), width = 4.5, height = 4.5)


temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A18_, risk.group, weight) %>% 
  mutate(S4AQ4A18_ = ifelse(S4AQ4A18_ == 1,"Yes", 
                     ifelse(S4AQ4A18_ == 2, "No",
                     ifelse(S4AQ4A18_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A18_ = fct_relevel(S4AQ4A18_, "Yes","No", "Missing")) %>%
  group_by(S4AQ4A18_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)


temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A18_, weight) %>% 
  mutate(S4AQ4A18_ = ifelse(S4AQ4A18_ == 1,"Yes", 
                     ifelse(S4AQ4A18_ == 2, "No",
                     ifelse(S4AQ4A18_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A18_ = fct_relevel(S4AQ4A18_, "Yes","No", "Missing"))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S4AQ4A18_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.57) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Felt like wanted to die") + 
  xlab("Score") + theme_bw() + xlim(-0.1,1.1) -> p1

p1 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 0.65)))


```

### Thought about commiting suicide

```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A17_, risk.group, weight) %>% 
  mutate(S4AQ4A17_ = ifelse(S4AQ4A17_ == 1,"Yes", 
                     ifelse(S4AQ4A17_ == 2, "No",
                     ifelse(S4AQ4A17_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A17_ = fct_relevel(S4AQ4A17_, "Yes","No", "Missing")) %>%
  group_by(S4AQ4A17_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A17_, weight) %>% 
  mutate(S4AQ4A17_ = ifelse(S4AQ4A17_ == 1,"Yes", 
                     ifelse(S4AQ4A17_ == 2, "No",
                     ifelse(S4AQ4A17_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A17_ = fct_relevel(S4AQ4A17_, "Yes","No", "Missing"))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S4AQ4A17_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.57) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Thought about commiting suicide") + 
  xlab("Score") + theme_bw() + xlim(-0.1,1.1) -> p2

p2 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 0.7)))

```


### Attempted Suicide

```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A16_, risk.group, weight) %>% 
  mutate(S4AQ4A16_ = ifelse(S4AQ4A16_ == 1,"Yes", 
                     ifelse(S4AQ4A16_ == 2, "No",
                     ifelse(S4AQ4A16_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A16_ = fct_relevel(S4AQ4A16_, "Yes","No", "Missing")) %>%
  group_by(S4AQ4A16_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A16_, weight) %>% 
  mutate(S4AQ4A16_ = ifelse(S4AQ4A16_ == 1,"Yes", 
                     ifelse(S4AQ4A16_ == 2, "No",
                     ifelse(S4AQ4A16_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A16_ = fct_relevel(S4AQ4A16_, "Yes","No", "Missing"))


temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S4AQ4A16_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.57) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Attempted Suicide") + 
  xlab("Score") + theme_bw() + xlim(-0.1,1.1) -> p3

p3 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 0.7)))

```



### Age

```{r}


temp.data <- data.partial %>%
  select(prediction.balanced.rf, AGE_quantiles_, risk.group, weight) %>% 
  mutate(AGE_quantiles_ = fct_relevel(AGE_quantiles_, c("low", "mid", "high"))) %>%
  group_by(AGE_quantiles_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)

temp.data <- data.partial %>%
  select(prediction.balanced.rf, AGE_quantiles_, weight) %>% 
  mutate(AGE_quantiles_ = ifelse(AGE_quantiles_ == "low", "18-36 years old", 
                          ifelse(AGE_quantiles_ == "mid","37-53 years old ",
                                                         "> 53 years old")),
    AGE_quantiles_ = fct_relevel(AGE_quantiles_, c("18-36 years old", "37-53 years old ", "> 53 years old")))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = AGE_quantiles_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.57) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Age Quantiles") + 
  xlab("Score") + theme_bw() + xlim(-0.1,1.1) -> p4

p4 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 0.6)))

```

### DURING PAST 4 WEEKS, HOW OFTEN FELT DOWNHEARTED AND DEPRESSED


```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q213_, risk.group, weight) %>% 
  mutate(S1Q213_ = ifelse(S1Q213_ == 1, "All of the time", 
                     ifelse(S1Q213_ == 2, "Most of the time",
                     ifelse(S1Q213_ == 3, "Some of the time",
                     ifelse(S1Q213_ == 4, "A little of the time",
                     ifelse(S1Q213_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q213_ = fct_relevel(S1Q213_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 )) %>%
  group_by(S1Q213_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q213_, weight) %>% 
  mutate(S1Q213_ = ifelse(S1Q213_ == 1, "All of the time", 
                     ifelse(S1Q213_ == 2, "Most of the time",
                     ifelse(S1Q213_ == 3, "Some of the time",
                     ifelse(S1Q213_ == 4, "A little of the time",
                     ifelse(S1Q213_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q213_ = fct_relevel(S1Q213_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 ))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q213_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("During Past 4 Weeks, \n How Often Felt Downhearted And Depressed") + 
  xlab("Score") + theme_bw() + xlim(-0.1,1.1) -> p5


p5 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))
```



### DURING PAST 4 WEEKS, HOW OFTEN DID WORK OR OTHER ACTIVITIES LESS CAREFULLY THAN USUAL AS RESULT OF EMOTIONAL PROBLEMS


```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q191_, risk.group, weight) %>% 
  mutate(S1Q191_ = ifelse(S1Q191_ == 1, "All of the time", 
                     ifelse(S1Q191_ == 2, "Most of the time",
                     ifelse(S1Q191_ == 3, "Some of the time",
                     ifelse(S1Q191_ == 4, "A little of the time",
                     ifelse(S1Q191_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q191_ = fct_relevel(S1Q191_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 )) %>%
  group_by(S1Q191_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)


temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q191_, weight) %>% 
  mutate(S1Q191_ = ifelse(S1Q191_ == 1, "All of the time", 
                     ifelse(S1Q191_ == 2, "Most of the time",
                     ifelse(S1Q191_ == 3, "Some of the time",
                     ifelse(S1Q191_ == 4, "A little of the time",
                     ifelse(S1Q191_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q191_ = fct_relevel(S1Q191_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 ))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q191_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("During Past 4 Weeks, \n How Often Did Work Or Other Activities Less Carefully Than Usual \n As Result Of Emotional Problems") + 
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) -> p6

p6 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))

```


### DURING PAST 4 WEEKS, HOW OFTEN ACCOMPLISHED LESS THAN WOULD LIKE AS RESULT OF EMOTIONAL PROBLEMS


```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q192_, risk.group, weight) %>% 
  mutate(S1Q192_ = ifelse(S1Q192_ == 1, "All of the time", 
                     ifelse(S1Q192_ == 2, "Most of the time",
                     ifelse(S1Q192_ == 3, "Some of the time",
                     ifelse(S1Q192_ == 4, "A little of the time",
                     ifelse(S1Q192_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q192_ = fct_relevel(S1Q192_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 )) %>%
  group_by(S1Q192_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)


temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q192_, weight) %>% 
  mutate(S1Q192_ = ifelse(S1Q192_ == 1, "All of the time", 
                     ifelse(S1Q192_ == 2, "Most of the time",
                     ifelse(S1Q192_ == 3, "Some of the time",
                     ifelse(S1Q192_ == 4, "A little of the time",
                     ifelse(S1Q192_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q192_ = fct_relevel(S1Q192_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 ))

temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q192_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("During Past 4 Weeks, \n How Often Accomplished Less Than Would Like \n As Result Of Emotional Problems") + 
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) -> p7

p7 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))
```



### Experienced Major Financial Crisis

```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q2310_, risk.group, weight) %>% 
  mutate(S1Q2310_ = ifelse(S1Q2310_ == 1,"Yes", 
                     ifelse(S1Q2310_ == 2, "No","Unknown"))) %>%
  mutate(S1Q2310_ = fct_relevel(S1Q2310_, "Yes","No")) %>%
  group_by(S1Q2310_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n())

kable(temp.data, digits = 4)


temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q2310_, weight) %>% 
  mutate(S1Q2310_ = ifelse(S1Q2310_ == 1,"Yes", 
                     ifelse(S1Q2310_ == 2, "No","Unknown"))) %>%
  mutate(S1Q2310_ = fct_relevel(S1Q2310_, "Yes","No"))


temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q2310_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Experienced Major Financial Crisis") + 
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) -> p8

p8 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))

dev.off()


```

### GRADE LEVEL DURING 2000-2001 SCHOOL YEAR (S1Q7D_)

```{r}


pdf(here("Data", "Model_output", "figure5_2.pdf"), width = 7, height = 9)

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q7D_, risk.group, weight) %>% 
  group_by(S1Q7D_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n()) %>%
  ungroup %>%
  mutate(S1Q7D_ = case_when(S1Q7D_ == 1 ~ "High School - Any Grade Level",
                            S1Q7D_ == 2 ~ "Enrolled in GED",
                            S1Q7D_ == 3 ~ "1st Year Undergraduate - Never Attended College Before",
                            S1Q7D_ == 4 ~ "1st Year Undergraduate - Attended Before",
                            S1Q7D_ == 5 ~ "2nd Year Undergraduate",
                            S1Q7D_ == 6 ~ "3rd Year Undergraduate",
                            S1Q7D_ == 7 ~ "4th Year Undergraduate",
                            S1Q7D_ == 8 ~ "5th Year Other Undergraduate",
                            S1Q7D_ == 9 ~ "1st Year Graduate / Professional",
                            S1Q7D_ == 10 ~ "2nd Year Graduate / Professional",
                            S1Q7D_ == 11 ~ "3rd Year Graduate / Professional",
                            S1Q7D_ == 12 ~ "Other",
                            S1Q7D_ == "(Missing)" ~ "Not a student or NA"))

kable(temp.data, digits = 4)


temp.data <- data.partial  %>%
  mutate(S1Q7D_ = case_when(S1Q7D_ == 1 ~ "High School - Any Grade Level",
                            S1Q7D_ == 2 ~ "Enrolled in GED",
                            S1Q7D_ == 3 ~ "1st Year Undergraduate - Never Attended College Before",
                            S1Q7D_ == 4 ~ "1st Year Undergraduate - Attended Before",
                            S1Q7D_ == 5 ~ "2nd Year Undergraduate",
                            S1Q7D_ == 6 ~ "3rd Year Undergraduate",
                            S1Q7D_ == 7 ~ "4th Year Undergraduate",
                            S1Q7D_ == 8 ~ "5th Year Other Undergraduate",
                            S1Q7D_ == 9 ~ "1st Year Graduate / Professional",
                            S1Q7D_ == 10 ~ "2nd Year Graduate / Professional",
                            S1Q7D_ == 11 ~ "3rd Year Graduate / Professional",
                            S1Q7D_ == 12 ~ "Other",
                            S1Q7D_ == "(Missing)" ~ "Not a student or NA"),
         S1Q7D_ = fct_relevel(S1Q7D_, "High School - Any Grade Level",
                                      "Enrolled in GED",
                                      "1st Year Undergraduate - Never Attended College Before",
                                      "1st Year Undergraduate - Attended Before",
                                      "2nd Year Undergraduate",
                                      "3rd Year Undergraduate",
                                      "4th Year Undergraduate",
                                      "5th Year Other Undergraduate",
                                      "1st Year Graduate / Professional",
                                      "2nd Year Graduate / Professional",
                                      "3rd Year Graduate / Professional",
                                      "Other"))


temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q7D_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Grade level during 2000-2001 School Year") + 
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) -> p9

p9 + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))


```


### HIGHEST GRADE OR YEAR OF SCHOOL COMPLETED (S1Q6A)

```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S1Q6A_, risk.group, weight) %>% 
  group_by(S1Q6A_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / n(),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / n(),
            wgt.prop.high = sum(weight[risk.group == "high"]) / n(),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / n(),
            count = n()) %>%
  ungroup %>%
  mutate(S1Q6A_ = case_when(S1Q6A_ == 1 ~ "No Formal Schooling",
                            S1Q6A_ == 2 ~ "Completed Grade K, 1 or 2",
                            S1Q6A_ == 3 ~ "Completed Grade 3 or 4",
                            S1Q6A_ == 4 ~ "Completed Grade 5 or 6",
                            S1Q6A_ == 5 ~ "Completed Grade 7",
                            S1Q6A_ == 6 ~ "Completed Grade 8",
                            S1Q6A_ == 7 ~ "Some High School (Grades 9-11)",
                            S1Q6A_ == 8 ~ "Completed High School",
                            S1Q6A_ == 9 ~ "Completed GED",
                            S1Q6A_ == 10 ~ "Some College (No Degree)",
                            S1Q6A_ == 11 ~ "Completed Associate Other Technical 2-year Degree",
                            S1Q6A_ == 12 ~ "Completed College",
                            S1Q6A_ == 13 ~ "Some Graduate Studies",
                            S1Q6A_ == 14 ~ "Completed Graduate or Professional Degree"))

kable(temp.data, digits = 4)


temp.data <- data.partial  %>%
              mutate(S1Q6A_ = case_when(S1Q6A_ == 1 ~ "No Formal Schooling",
                                        S1Q6A_ == 2 ~ "Completed Grade K, 1 or 2",
                                        S1Q6A_ == 3 ~ "Completed Grade 3 or 4",
                                        S1Q6A_ == 4 ~ "Completed Grade 5 or 6",
                                        S1Q6A_ == 5 ~ "Completed Grade 7",
                                        S1Q6A_ == 6 ~ "Completed Grade 8",
                                        S1Q6A_ == 7 ~ "Some High School (Grades 9-11)",
                                        S1Q6A_ == 8 ~ "Completed High School",
                                        S1Q6A_ == 9 ~ "Completed GED",
                                        S1Q6A_ == 10 ~ "Some College (No Degree)",
                                        S1Q6A_ == 11 ~ "Completed Associate Other Technical 2-year Degree",
                                        S1Q6A_ == 12 ~ "Completed College",
                                        S1Q6A_ == 13 ~ "Some Graduate Studies",
                                        S1Q6A_ == 14 ~ "Completed Graduate or Professional Degree"),
                     S1Q6A_ = fct_relevel(S1Q6A_, "No Formal Schooling",
                                                  "Completed Grade K, 1 or 2",
                                                  "Completed Grade 3 or 4",
                                                  "Completed Grade 5 or 6",
                                                  "Completed Grade 7",
                                                  "Completed Grade 8",
                                                  "Some High School (Grades 9-11)",
                                                  "Completed High School",
                                                  "Completed GED",
                                                  "Some College (No Degree)",
                                                  "Completed Associate Other Technical 2-year Degree",
                                                  "Completed College",
                                                  "Some Graduate Studies")) 


temp.data %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = S1Q6A_, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 1) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  ylab("Highest Grade Or Year Of School Completed") + 
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) -> p10

p10  + theme(legend.position = "none") + scale_y_discrete(expand = expand_scale(add = c(0, 1.2)))
dev.off()

```



```{r}

ggarrange(p1 + theme(legend.position = "none"), 
          p2 + theme(legend.position = "none"), nrows  = 2, ncols = 2)


```


### Felt like wanted to Die, Age, and During Past 4 Weeks, How Often Felt Downhearted or Depressed

```{r}

temp.data <- data.partial %>%
  select(prediction.balanced.rf, S4AQ4A18_, AGE_quantiles_, S1Q213_, weight) %>% 
  mutate(S4AQ4A18_ = ifelse(S4AQ4A18_ == 1,"Yes", 
                     ifelse(S4AQ4A18_ == 2, "No",
                     ifelse(S4AQ4A18_ == 9, "Missing","NA, Never or Unknown")))) %>%
  mutate(S4AQ4A18_ = fct_relevel(S4AQ4A18_, "Yes","No", "Missing")) %>%
  mutate(S1Q213_ = ifelse(S1Q213_ == 1, "All of the time", 
                     ifelse(S1Q213_ == 2, "Most of the time",
                     ifelse(S1Q213_ == 3, "Some of the time",
                     ifelse(S1Q213_ == 4, "A little of the time",
                     ifelse(S1Q213_ == 5, "None of the time","Unknown")))))) %>%
  mutate(S1Q213_ = fct_relevel(S1Q213_, "All of the time",
                                 "Most of the time",
                                 "Some of the time",
                                 "A little of the time",
                                 "None of the time"
                                 )) %>%
  mutate(AGE_quantiles_ = fct_relevel(AGE_quantiles_, c("low", "mid", "high"))) %>%
  group_by(S4AQ4A18_, AGE_quantiles_, S1Q213_) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf)) %>%
  ungroup() %>%
  mutate(AGE_quantiles_ = Hmisc::capitalize(as.character(AGE_quantiles_)),
         AGE_quantiles_ = fct_relevel(AGE_quantiles_, c("Low", "Mid", "High")))

temp.data %>%
ggplot(aes(x=AGE_quantiles_, y=mean.prediction, color = S4AQ4A18_, group = S4AQ4A18_, weight)) + 
  geom_point() + 
  geom_line() +
  theme_bw() + 
  facet_grid(. ~ S1Q213_) +
  xlab("Age") + 
  ylab("Mean Score") + 
  ggtitle("Felt Like Wanted to Die") + 
  scale_color_discrete(name = "Experienced Major Financial Crisis")

```



# Supplement: Figure 4 Prediction by Time to Event

```{r}

load(here("Data","Raw_Data", "w1w2suicidedata_WithCases_May2020.RData"))

data.pred.prob %>%
  mutate(suicide_age = 3 - w1w2suicide.v3$age_diff) %>%
  mutate(suicide = ifelse(suicide == 1, "Yes", "No")) %>%
  filter(suicide == "Yes") %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.low, threshold.med),
                                        "medium",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.med, threshold.high),
                                        "high", "very high"))),
                       risk.group = as.factor(risk.group), 
                       risk.group = relevel(risk.group, "medium"),
                       risk.group = relevel(risk.group, "low")) %>%
  group_by(suicide_age) %>%
  summarize(mean.prediction = mean(prediction.balanced.rf),
            prop.low = sum(risk.group == "low") / n(),
            prop.medium = sum(risk.group == "medium") / n(),
            prop.high = sum(risk.group == "high") / n(),
            prop.very.high = sum(risk.group == "very high") / n(),
            wgt.prop.low = sum(weight[risk.group == "low"]) / sum(weight),
            wgt.prop.medium = sum(weight[risk.group == "medium"]) / sum(weight),
            wgt.prop.high = sum(weight[risk.group == "high"]) / sum(weight),
            wgt.prop.very.high = sum(weight[risk.group == "very high"]) / sum(weight),
            Wght.Mean.Score = Hmisc::wtd.mean(prediction.balanced.rf, weights = weight),
            count = n()) %>%
  kable(digits = 4)

data.pred.prob %>%
  mutate(suicide_age = 3 - w1w2suicide.v3$age_diff) %>%
  mutate(suicide = ifelse(suicide == 1, "Yes", "No")) %>%
  filter(suicide == "Yes") %>%
  mutate(risk.group = ifelse(prediction.balanced.rf < threshold.low, "low",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.low, threshold.med),
                                        "medium",
                                      ifelse(between(prediction.balanced.rf,
                                        threshold.med, threshold.high),
                                        "high", "very high"))),
                       risk.group = as.factor(risk.group), 
                       risk.group = relevel(risk.group, "medium"),
                       risk.group = relevel(risk.group, "low"),
         suicide_age = as.character(suicide_age),
         suicide_age = ifelse(suicide_age == "0", "Within the first year",
                       ifelse(suicide_age == "1", "Between the first and second year",
                       ifelse(suicide_age == "2", "Between the second and third year", 
                              "Between third year and wave 2 interview"))),
         suicide_age = as.factor(suicide_age),
         suicide_age = relevel(suicide_age, "Within the first year")) %>%
  filter(suicide == "Yes") %>%
  ggplot(aes(x = prediction.balanced.rf, weight = weight, height = ..density..,
             y = suicide_age, fill = relevel(relevel(as.factor(ifelse(..x.. < threshold.low, "low",
                                              ifelse(between(..x..,
                                                             threshold.low, threshold.med),
                                                             "medium",
                                              ifelse(between(..x..,
                                                             threshold.med, threshold.high),
                                                             "high", "very high")))), "medium"),"low"))) +
  geom_density_ridges_gradient(stat = "density", scale = 0.6) +
  scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                   name = "Risk Group", 
                                   labels = c("Low",
                                              "Medium",
                                              "High",
                                              "Very High")) +
  xlab("Score") + theme_bw() + theme(axis.title=element_text(size=10)) + xlim(-0.1,1.1) +
  ylab("Time to Suicide Attempt") + theme_bw() -> p11

p11


```


# Supplement: Table 3 Demographics

```{r}

library(pROC)


data.pred.prob %>%
  mutate(sex = w1w2suicide.v3$SEX,
         sex = ifelse(sex == 1, "Male", "Female"),
         suicide = as.character(suicide)) %>% 
  group_by(sex) %>%
  summarize(Total = n(),
            Prop.total = Total / 34653,
            N.Cases = sum(suicide == 1),
            N.Controls = sum(suicide == 0),
            Prop.Cases = N.Cases / Total,
            Prop.Controls = N.Controls / Total,
            Wght.Total = sum(weight),
            Wght.Cases = sum(weight[suicide == 1]),
            Wght.Controls = sum(weight[suicide == 0]),
            Wght.Prop.Cases = Wght.Cases / Wght.Total,
            Wght.Prop.Controls = Wght.Controls / Wght.Total,
            roc = pROC::roc(response = suicide, predictor = prediction.balanced.rf)$auc %>% as.numeric(),
            roc.ci.low = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[1],
            roc.ci.high = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[3],
            pred.cases = sum((prediction.balanced.rf > threshold.low) & suicide == 1),
            predicted.controls = sum((prediction.balanced.rf < threshold.low) & suicide == 0),
            sensitivity = pred.cases / N.Cases, 
            specificity = sum((prediction.balanced.rf < threshold.low) & suicide == 0) / sum(suicide == 0)) %>%
    mutate(AUC = paste0(round(roc,3), " (", round(roc.ci.low,3), "-", round(roc.ci.high, 3), ")")) %>%
    write_csv("~/Desktop/GradSchool/Papers/NESARC_Suicide/Analysis/Scripts_Final/Appendix_Table_Sex.csv")


data.pred.prob %>%
  mutate(race = w1w2suicide.v3$ETHRACE2A,
         race = ifelse(race == 1, "White", "Non-White"),
         suicide = as.character(suicide)) %>% 
  group_by(race) %>%
  summarize(Total = n(),
            Prop.total = Total / 34653,
            N.Cases = sum(suicide == 1),
            N.Controls = sum(suicide == 0),
            Prop.Cases = N.Cases / Total,
            Prop.Controls = N.Controls / Total,
            Wght.Total = sum(weight),
            Wght.Cases = sum(weight[suicide == 1]),
            Wght.Controls = sum(weight[suicide == 0]),
            Wght.Prop.Cases = Wght.Cases / Wght.Total,
            Wght.Prop.Controls = Wght.Controls / Wght.Total,
            roc = pROC::roc(response = suicide, predictor = prediction.balanced.rf)$auc %>% as.numeric(),
            roc.ci.low = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[1],
            roc.ci.high = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[3],
            pred.cases = sum((prediction.balanced.rf > threshold.low) & suicide == 1),
            predicted.controls = sum((prediction.balanced.rf < threshold.low) & suicide == 0),
            sensitivity = pred.cases / N.Cases, 
            specificity = sum((prediction.balanced.rf < threshold.low) & suicide == 0) / sum(suicide == 0)) %>%
    mutate(AUC = paste0(round(roc,3), " (", round(roc.ci.low,3), "-", round(roc.ci.high, 3), ")")) %>%
    write_csv("~/Desktop/GradSchool/Papers/NESARC_Suicide/Analysis/Scripts_Final/Appendix_Table_Race.csv")


data.pred.prob %>%
  mutate(AGE_quantiles_ = x_train$AGE_quantiles_) %>%
  mutate(AGE_quantiles_ = ifelse(AGE_quantiles_ == "low", "18-36 years old", 
                          ifelse(AGE_quantiles_ == "mid","37-53 years old ",
                                                         "> 53 years old")),
  AGE_quantiles_ = fct_relevel(AGE_quantiles_, c("18-36 years old", "37-53 years old ", "> 53 years old")),
         suicide = as.character(suicide)) %>% 
  group_by(AGE_quantiles_) %>%
  summarize(Total = n(),
            Prop.total = Total / 34653,
            N.Cases = sum(suicide == 1),
            N.Controls = sum(suicide == 0),
            Prop.Cases = N.Cases / Total,
            Prop.Controls = N.Controls / Total,
            Wght.Total = sum(weight),
            Wght.Cases = sum(weight[suicide == 1]),
            Wght.Controls = sum(weight[suicide == 0]),
            Wght.Prop.Cases = Wght.Cases / Wght.Total,
            Wght.Prop.Controls = Wght.Controls / Wght.Total,
            roc = pROC::roc(response = suicide, predictor = prediction.balanced.rf)$auc %>% as.numeric(),
            roc.ci.low = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[1],
            roc.ci.high = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[3],
            pred.cases = sum((prediction.balanced.rf > threshold.low) & suicide == 1),
            predicted.controls = sum((prediction.balanced.rf < threshold.low) & suicide == 0),
            sensitivity = pred.cases / N.Cases, 
            specificity = sum((prediction.balanced.rf < threshold.low) & suicide == 0) / sum(suicide == 0)) %>%
    mutate(AUC = paste0(round(roc,3), " (", round(roc.ci.low,3), "-", round(roc.ci.high, 3), ")")) %>%
    write_csv("~/Desktop/GradSchool/Papers/NESARC_Suicide/Analysis/Scripts_Final/Appendix_Table_Age.csv")



data.pred.prob %>%
  mutate(INCPER3_ = x_train$INCPER3_) %>%
  mutate(INCPER3_ = ifelse(INCPER3_ == 1,"$0-20000", 
                    ifelse(INCPER3_ == 2,"$20000-34999",
                    ifelse(INCPER3_ == 3,"$35000-69,999","$70,000 or more"))),
  INCPER3_ = fct_relevel(INCPER3_, c("$0-20000", "$20000-34999", "$35000-69,999", "$70,000 or more")),
         suicide = as.character(suicide)) %>% 
  group_by(INCPER3_) %>%
  summarize(Total = n(),
            Prop.total = Total / 34653,
            N.Cases = sum(suicide == 1),
            N.Controls = sum(suicide == 0),
            Prop.Cases = N.Cases / Total,
            Prop.Controls = N.Controls / Total,
            Wght.Total = sum(weight),
            Wght.Cases = sum(weight[suicide == 1]),
            Wght.Controls = sum(weight[suicide == 0]),
            Wght.Prop.Cases = Wght.Cases / Wght.Total,
            Wght.Prop.Controls = Wght.Controls / Wght.Total,
            roc = pROC::roc(response = suicide, predictor = prediction.balanced.rf)$auc %>% as.numeric(),
            roc.ci.low = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[1],
            roc.ci.high = pROC::roc(response = suicide, predictor = prediction.balanced.rf, ci = T)$ci[3],
            pred.cases = sum((prediction.balanced.rf > threshold.low) & suicide == 1),
            predicted.controls = sum((prediction.balanced.rf < threshold.low) & suicide == 0),
            sensitivity = pred.cases / N.Cases, 
            specificity = sum((prediction.balanced.rf < threshold.low) & suicide == 0) / sum(suicide == 0)) %>%
    mutate(AUC = paste0(round(roc,3), " (", round(roc.ci.low,3), "-", round(roc.ci.high, 3), ")")) %>%
    write_csv("~/Desktop/GradSchool/Papers/NESARC_Suicide/Analysis/Scripts_Final/Appendix_Table_Income.csv")






```


## Editorial Changes


```{r}

data.pred.prob <- data.pred.prob %>%
                  mutate(optimal.pred = ifelse(prediction.balanced.rf < threshold.low, 0, 1))

library(epiR)

table(ifelse(data.pred.prob$optimal.pred, "1. Yes", "2. No"), 
                ifelse(data.pred.prob$suicide == 1, "1. Yes", "2. No")) -> table.input

table.input[1,1] <- sum(data.pred.prob$weight[data.pred.prob$suicide == 1 &
                                              data.pred.prob$optimal.pred == 1])

table.input[2,1] <- sum(data.pred.prob$weight[data.pred.prob$suicide == 1 &
                                              data.pred.prob$optimal.pred == 0])

table.input[1,2] <- sum(data.pred.prob$weight[data.pred.prob$suicide == 0 &
                                              data.pred.prob$optimal.pred == 1])

table.input[2,2] <- sum(data.pred.prob$weight[data.pred.prob$suicide == 0 &
                                              data.pred.prob$optimal.pred == 0])

table.input

epi.tests(table.input) -> class.results 

class.results

class.results$elements$specificity * 100
class.results$elements$sensitivity * 100

```


## Weighted Proportion of Females

```{r}

sum(data.pred.prob$weight[which(w1w2suicide.v3$SEX == 2)]) / 34653

```

## Age at Wave 1

```{r}

wtd.mean(w1w2suicide.v3$AGE, data.pred.prob$weight)
sqrt(wtd.var(w1w2suicide.v3$AGE, data.pred.prob$weight))


```

## Age at Wave 2

```{r}

wtd.mean(w1w2suicide.v3$W2AGE, data.pred.prob$weight)
sqrt(wtd.var(w1w2suicide.v3$W2AGE, data.pred.prob$weight))


```


