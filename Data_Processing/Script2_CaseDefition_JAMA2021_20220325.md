In this script I defined the cases for this analysis as detailed in the
Methods section of the paper.

    load(here("Data","Raw_Data", "w1w2suicidedata_raw_May2020.RData"))

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

    save(w1w2suicide.v3, file = here("Data","Raw_Data", "w1w2suicidedata_WithCases_May2020.RData"))
