This script generates figures from the supplement

    load(here("Data","Model_output", "w1w2suicidedata_Clean_May2020.RData"))
    load(here("Data","Model_output", "NESARC_Suicide_BRF_Output_May2020.rda"))


    library(pROC)
    library(PRROC)
    library(MLmetrics)



    ## Generate ROC / Predictions for Random Forest

    prediction.balanced.rf <- train.balanced.rf$pred %>% 
                              arrange(rowIndex) %>%
                              filter(mtry == train.balanced.rf$bestTune$mtry,
                                     min.node.size == train.balanced.rf$bestTune$min.node.size)
    prediction.balanced.rf <- prediction.balanced.rf$yes


    data.pred.prob <-cbind(prediction.balanced.rf) %>%
                           as_tibble() %>%
                           mutate(suicide = y_train) 

    calculate.vals <- function(threshold) {
      
      data <- data.pred.prob %>%
                mutate(prediction.bf = ifelse(prediction.balanced.rf > threshold,1,0),
                       suicide = ifelse(suicide == "yes",1,0)) %>%
                ## Create statistic at each set threshold
                summarize(Accuracy.rf = sum(prediction.bf == suicide) / n(),
                          Sensitivity.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(suicide == 1),
                          Specificity.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(suicide == 0),
                          PPV.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(prediction.bf == 1),
                          PPVDen.rf = sum(prediction.bf == 1),
                          PPVSE.rf = sqrt(PPV.rf * (1-PPV.rf) / sum(prediction.bf == 1)),
                          NPV.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(prediction.bf == 0),
                          Alarms.rf = sum(prediction.bf == 1) / n() * 100,
                          NNE.rf = sum(prediction.bf == 1) / sum((prediction.bf == suicide) & suicide == 1)) %>%
                mutate(threshold = threshold)
      
      return(data)

    }


    cut.off <- data.frame(cutoff = sort(c(seq(0,1, 0.00125), 0.5335)))

    output <- cut.off$cutoff %>%
                map_dfr( ~ calculate.vals(threshold = .x)) %>%
                gather(type, statistic, -threshold) %>%
                separate(type, c("type","model"), "\\.") %>%
                spread(type, statistic)

    output <- output %>%
              filter(model == "rf")

# Table 1: Risk Group Summary

    very.high.cutoff <- min(output$threshold[which(output$PPV > 0.1)])


    data <- data.pred.prob %>%
                mutate(risk.group = ifelse(prediction.balanced.rf < 0.3425, "low",
                                    ifelse(between(prediction.balanced.rf,0.3425,
                                                   quantile(data.pred.prob$prediction.balanced.rf, 0.9)),
                                           "medium",
                                    ifelse(between(prediction.balanced.rf,quantile(data.pred.prob$prediction.balanced.rf, 0.9),
                                                   very.high.cutoff),
                                           "high", "very high"))),
                       prediction.bf = ifelse(risk.group == "low",
                                              0, 1),
                       suicide = ifelse(suicide == "yes",1,0)) %>%
                group_by(risk.group) %>%
                summarize(Total = n(),
                          N.Cases = sum(suicide == 1),
                          N.Controls = sum(suicide == 0),
                          Prop.Cases = N.Cases / Total,
                          Prop.Controls = N.Controls / Total,
                          Mean.Score = mean(prediction.balanced.rf) * 100,
                          Accuracy.rf = sum(prediction.bf == suicide) / n(),
                          Sensitivity.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(suicide == 1),
                          Specificity.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(suicide == 0),
                          PPV.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(prediction.bf == 1),
                          PPVDen.rf = sum(prediction.bf == 1),
                          PPVSE.rf = sqrt(PPV.rf * (1-PPV.rf) / sum(prediction.bf == 1)),
                          NPV.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(prediction.bf == 0),
                          Alarms.rf = sum(prediction.bf == 1) / n() * 100,
                          NNE.rf = sum(prediction.bf == 1) / sum((prediction.bf == suicide) & suicide == 1)) %>%
                ungroup %>%
                select(-PPVDen.rf, -PPVSE.rf) %>%
                slice(2,3,1,4)

    table1 <- data %>% 
              select(risk.group:Mean.Score)
                                 
    table2 <- data.pred.prob %>%
                mutate(risk.group = ifelse(prediction.balanced.rf < 0.3425, "low",
                                    ifelse(between(prediction.balanced.rf,0.3425,
                                                   quantile(data.pred.prob$prediction.balanced.rf, 0.9)),
                                           "medium",
                                    ifelse(between(prediction.balanced.rf,quantile(data.pred.prob$prediction.balanced.rf, 0.9),
                                                   very.high.cutoff),
                                           "high", "very high"))),
                       prediction.bf = ifelse(risk.group == "low",
                                              0, 1),
                       suicide = ifelse(suicide == "yes",1,0)) %>%
                summarize(Total = n(),
                          N.Cases = sum(suicide == 1),
                          N.Controls = sum(suicide == 0),
                          Prop.Cases = N.Cases / Total,
                          Prop.Controls = N.Controls / Total,
                          Mean.Score = mean(prediction.balanced.rf) * 100)
                          
                          


    rbind(table1, c("Total",as.numeric(table2))) %>%
          kable(digits = 4, 
                   caption = "Summary statistics of Risk Groups",
                   col.names = c("Risk Group",
                                 "Total",
                                 "N Cases",
                                 "N Controls",
                                 "Prop Cases",
                                 "Prop Controls",
                                 "Mean Score"))

# Supplement 2: Prediction by Time to Event

    load("Data","Model_output", "w1w2suicidedata_Clean_May2020.RData")

    data.pred.prob %>%
      mutate(suicide_age = 3 - w1w2suicide.v3$age_diff) %>%
      mutate(risk.group = ifelse(prediction.balanced.rf < 0.3425, "low",
                                    ifelse(between(prediction.balanced.rf,
                                                   0.3425,
                                                   quantile(data.pred.prob$prediction.balanced.rf,
                                                            0.9)), "medium",
                          ifelse(between(prediction.balanced.rf,
                                         quantile(data.pred.prob$prediction.balanced.rf,
                                                  0.9), very.high.cutoff),
                                           "high", "very high"))),
             risk.group = as.factor(risk.group), 
             risk.group = relevel(risk.group, "medium"),
             risk.group = relevel(risk.group, "low"),
             suicide = ifelse(suicide == "yes", "Yes", "No")) %>%
      filter(suicide == "Yes") %>%
             group_by(suicide_age) %>%
             summarize(mean.prediction = mean(prediction.balanced.rf),
                prop.low = sum(risk.group == "low") / n(),
                prop.medium = sum(risk.group == "medium") / n(),
                prop.high = sum(risk.group == "high") / n(),
                prop.very.high = sum(risk.group == "very high") / n(),
                count = n()) %>%
      kable(digits = 4, col.names = c("Time to Suicide Attempt",
                                      "Mean Prediction",
                                      "% Low",
                                      "% Medium",
                                      "% High",
                                      "% Very High",
                                      "N"))

    data.pred.prob %>%
      mutate(suicide_age = 3 - w1w2suicide.v3$age_diff) %>%
      mutate(risk.group = ifelse(prediction.balanced.rf < 0.3425, "low",
                                    ifelse(between(prediction.balanced.rf,
                                                   0.3425,
                                                   quantile(data.pred.prob$prediction.balanced.rf,
                                                            0.9)), "medium",
                          ifelse(between(prediction.balanced.rf,
                                         quantile(data.pred.prob$prediction.balanced.rf,
                                                  0.9), very.high.cutoff),
                                           "high", "very high"))),
             risk.group = as.factor(risk.group), 
             risk.group = relevel(risk.group, "medium"),
             risk.group = relevel(risk.group, "low"),
             suicide = ifelse(suicide == "yes", "Yes", "No"),
             suicide_age = as.character(suicide_age),
             suicide_age = ifelse(suicide_age == "0", "Within the first year",
                           ifelse(suicide_age == "1", "Between the first and second year",
                           ifelse(suicide_age == "2", "Between the second and third year", 
                                  "Between third year and wave 2 interview"))),
             suicide_age = as.factor(suicide_age),
             suicide_age = relevel(suicide_age, "Within the first year")) %>%
      filter(suicide == "Yes") %>%
      ggplot(aes(y=suicide_age, x=prediction.balanced.rf, 
                 fill = relevel(relevel(as.factor(ifelse(..x.. < 0.3425, "low",
                                                  ifelse(between(..x..,
                                                                 0.3425,
                                                                 quantile(data.pred.prob$prediction.balanced.rf, 0.9)), "medium",
                                                         ifelse(between(..x..,quantile(data.pred.prob$prediction.balanced.rf,
                                                                                       0.9),very.high.cutoff), "high",
                                                                "very high")))), "medium"),"low"))) + 
      stat_density_ridges(geom = "density_ridges_gradient", scale = 0.8) +
      scale_fill_manual(values = c("#7FB069","#FFF9A5","#E6AA68","#D65D4A"),
                                       name = "Risk Group", 
                                       labels = c("Low",
                                                  "Medium",
                                                  "High",
                                                  "Very High")) +
      xlab("Score") + scale_y_discrete(expand = expand_scale(add = c(0, .7))) +
      ylab("Time to Suicide Attempt") + theme_bw() -> p2

    p2

# Supplement 3: CART with top 5 Variables

    tree.data <- data.pred.prob %>%
                  mutate(felt.dying = w1w2suicide$S4AQ4A18_,
                         attempted.suicide = w1w2suicide$S4AQ4A16_,
                         age = w1w2suicide$AGE_quantiles_,
                         thought.suicide = w1w2suicide$S4AQ4A17_,
                         depression = as.numeric(as.character(w1w2suicide$S1Q213_))) %>%
                  mutate(node = case_when((felt.dying == 1 | felt.dying == 9) &
                                          attempted.suicide == 1 ~ "1",
                                          (felt.dying == 1 | felt.dying == 9)  &
                                          attempted.suicide != 1 &
                                          depression < 5 &
                                          age != "high" ~ "2",
                                          (felt.dying == 1 | felt.dying == 9)  &
                                          attempted.suicide != 1 &
                                          depression < 5 &
                                          age == "high" &
                                          thought.suicide == 1 ~ "3",
                                          (felt.dying == 1 | felt.dying == 9)  &
                                          attempted.suicide != 1 &
                                          depression < 5 &
                                          age == "high" & 
                                          thought.suicide != 1 ~ "4",
                                          (felt.dying == 1 | felt.dying == 9)  &
                                          attempted.suicide != 1 &
                                          depression > 4 &
                                          age == "low" ~ "5",
                                          (felt.dying == 1 | felt.dying == 9)  &
                                          attempted.suicide != 1 &
                                          depression > 4 &
                                          age != "low" ~ "6",
                                          (felt.dying == 2 | felt.dying == "(Missing)") &
                                          age != "high" &
                                          depression < 4 ~ "7",
                                          (felt.dying == 2 | felt.dying == "(Missing)") &
                                          age != "high" &
                                          depression > 3 ~ "8",
                                          (felt.dying == 2 | felt.dying == "(Missing)") &
                                          age == "high" ~ "9",
                                          TRUE ~ "Error"
                                          ))


    tree.data <- tree.data %>%
                mutate(risk.group = ifelse(prediction.balanced.rf < 0.3425, "low",
                                    ifelse(between(prediction.balanced.rf,0.3425,
                                                   quantile(data.pred.prob$prediction.balanced.rf, 0.9)),
                                           "medium",
                                    ifelse(between(prediction.balanced.rf,quantile(data.pred.prob$prediction.balanced.rf, 0.9),
                                                   very.high.cutoff),
                                           "high", "very high"))),
                       prediction.bf = ifelse(risk.group == "low",
                                              0, 1),
                       suicide = ifelse(suicide == "yes",1,0)) %>%
                group_by(node) %>%
                summarize(Total = n(),
                          N.Cases = sum(suicide == 1),
                          N.Controls = sum(suicide == 0),
                          Prop.Cases = N.Cases / Total,
                          Prop.Controls = N.Controls / Total,
                          Mean.Score = mean(prediction.balanced.rf) * 100,
                          Accuracy.rf = sum(prediction.bf == suicide) / n(),
                          Sensitivity.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(suicide == 1),
                          Specificity.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(suicide == 0),
                          PPV.rf = sum((prediction.bf == suicide) & suicide == 1) / sum(prediction.bf == 1),
                          PPVDen.rf = sum(prediction.bf == 1),
                          PPVSE.rf = sqrt(PPV.rf * (1-PPV.rf) / sum(prediction.bf == 1)),
                          NPV.rf = sum((prediction.bf == suicide) & suicide == 0) / sum(prediction.bf == 0),
                          Alarms.rf = sum(prediction.bf == 1) / n() * 100,
                          NNE.rf = sum(prediction.bf == 1) / sum((prediction.bf == suicide) & suicide == 1)) %>%
                ungroup %>%
                select(node:Mean.Score)

    med.cutoff <- 0.3425
    high.cutoff <- quantile(data.pred.prob$prediction.balanced.rf, 0.9)

    tree.data <- tree.data %>%
                 mutate(Mean.Score = Mean.Score / 100,
                        risk.group = case_when(Mean.Score < med.cutoff  ~ "low",
                                               Mean.Score > med.cutoff & Mean.Score < high.cutoff ~ "medium",
                                               Mean.Score > high.cutoff & Mean.Score < very.high.cutoff ~ "high",
                                               Mean.Score > very.high.cutoff ~ "very.high"),
                        Mean.Score = Mean.Score * 100,
                        tree.prediction = case_when(node %in% c(1,2,3,5,7) ~ "yes",
                                                    TRUE ~ "no"))


    tree.data %>%
          kable(digits = 4, 
                   caption = "Summary statistics of Risk Groups",
                   col.names = c("Node",
                                 "Total",
                                 "N Cases",
                                 "N Controls",
                                 "Prop Cases",
                                 "Prop Controls",
                                 "Mean Score",
                                 "Risk Group",
                                 "Tree Prediction"))

# Supplement 4: AUC of the Model with top 5 and top 10 Variables

    rf.imp <- varImp(train.balanced.rf)$importance %>%
                as.matrix() %>%
                as_tibble(rownames = "variable") %>%
                arrange(desc(abs(Overall))) %>% 
                head(50)


    rf.imp[1:10, -2] %>%
      kable()

    set.seed(1)

    rf.sample.fraction = c(222/34653, 222/34653)

    x_train_top10 <- x_train %>%
                        select(rf.imp$variable[1:10])


    train.rf.top10 <- caret::train(x = x_train_top10, y = y_train,
                          method = "ranger",
                          tuneLength = 20,
                          metric = "ROC",
                          num.trees = 4000,
                          importance = "impurity",
                          num.threads = 5,
                          sample.fraction = rf.sample.fraction,
                          trControl = trainControl(method = "cv",
                                                   verboseIter = TRUE,
                                                   classProbs = TRUE,
                                                   savePredictions = TRUE,
                                                   summaryFunction = twoClassSummary,
                                                   index = cvIndex))


    beep()

    train.rf.top10
