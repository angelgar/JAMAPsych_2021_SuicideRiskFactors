This script processes and organizes the risk factors variables in the
data. This script retypes variables into categorical vs. numeric,
identifies duplicated variables, and codes missing values. A more
detailed explanation of this process is included in the supplement (see
“Organizing the risk factor variables”).

# Load Data

    load(here("Data","Raw_Data", "w1w2suicidedata.RData"))
    load(here("Data","Raw_Data", "w1w2suicidedata_WithCases_May2020.RData"))

# Create Suicide Variables

    w1w2suicide <- w1w2suicide.v3

    rm(w1w2suicide.v3)

    w1w2suicide <- w1w2suicide %>%
                    mutate(suicide_sli = suicide_new) %>%
                    mutate(W2S4AQ3A16 = ifelse(is.na(W2S4AQ3A16), 
                                               2, W2S4AQ3A16),
                           suicide_334 = ifelse(W2S4AQ3A16 == 1,
                                                1, suicide_sli)) %>%
                    rename(suicide_222 = suicide_sli) %>%
                    select(-W2S4AQ3A16,
                           -W2S14Q17A,
                           -W2S14Q17B,
                           -W2S14Q17C,
                           -suicide_life,
                           -suicide_age,
                           -age_diff,
                           -suicide_new,
                           -W2AGE)

# Sort out numeric from non numeric

I created a decision tree that uses two flag the numeric variables.

-   All those variables that include number in either it’s name or
    description
-   All those variables that have more than 20 unique values

## Find those variables in the code book with “number”

    w1vars1 <- w1vars1 %>%
                mutate(num.description = str_detect(description, "NUM"),
                       scale.description = str_detect(description, "SCALE"),
                       age.description = str_detect(description, "AGE AT"),
                       num.name = str_detect(code, "NUM"),
                       num.logical = num.description | scale.description | num.name | age.description)

## Find variables with more than 20 unique values

    unique_values <- function(x) {
      length(unique(x))
    }

    w1w2_datatype <- w1w2suicide %>% 
                      map_dfc(~unique_values(.)) %>%
                      gather(key = "code", "unique") %>%
                      mutate(cont_twenty_logic = unique > 19)

## Merge these two together to create one single rule

    w1w2_datatype <- w1w2_datatype %>%
                      left_join(w1vars1, by = "code") %>%
                      mutate(is_numeric = cont_twenty_logic | num.logical) %>%
                      select(code, description, is_numeric) %>%
                      unique() %>%
                      mutate(is_numeric = ifelse(is.na(is_numeric), FALSE, is_numeric)) %>%
                      mutate(is_numeric = ifelse(code == "S1Q1E", FALSE, is_numeric),
                             is_numeric = ifelse(code == "WEIGHT", FALSE, is_numeric),
                             is_numeric = ifelse(code == "STRATUM", FALSE, is_numeric),
                             is_numeric = ifelse(code == "PSU", FALSE, is_numeric),
                             is_numeric = ifelse(code == grepl("scale", 
                                                               description, 
                                                               ignore.case = T), FALSE, is_numeric)
                             ) %>%
                      select(code, is_numeric)

# Create Dummy Variables for Missing Continuous

    column.is.numeric <- w1w2_datatype$code[which(w1w2_datatype$is_numeric == TRUE)]


    w1w2suicide_num <- w1w2suicide %>%
                        select(column.is.numeric)


    numeric.tocat <- function(x) {
      
      dummy.unknown <- ifelse(x == 99, "unknown", NA)
      x.no.99 <- ifelse(x == 99, NA, x)
      
      quantiles <- ntile(x.no.99, 3) %>% as.character()
      quantiles <- ifelse(quantiles == "1", "low", 
              ifelse(quantiles == "2", "mid", "high"))
      
      quantiles <- ifelse(is.na(quantiles), dummy.unknown, quantiles)
      
      return(quantiles)

    }


    w1w2_numeric_quantiles <- w1w2suicide_num %>% 
                            map_dfc(~numeric.tocat(.))

    names(w1w2_numeric_quantiles)[-1] <- paste(names(w1w2_numeric_quantiles)[-1], "quantiles", sep="_")
    w1w2_numeric_quantiles$IDNUM <- w1w2suicide_num$IDNUM

# Retype categorical variables

    mutate.columns.tofactor <- w1w2_datatype$code[which(w1w2_datatype$is_numeric == FALSE)]

    w1w2suicide <- w1w2suicide %>% 
                        select(IDNUM, mutate.columns.tofactor) %>%
                        left_join(w1w2_numeric_quantiles, by = "IDNUM") %>%
                        mutate_all(as.factor) %>%
                        mutate_all(fct_explicit_na)
                        

    names(w1w2suicide)[-1] <- paste0(names(w1w2suicide)[-1], "_")

    write_csv(w1w2_datatype, here("Data","Processed_Data", "NESARC_Codebook_DataType_Variables.csv"))

# Check for variables with unique values.

    remove.unique.cat <- w1w2suicide %>%
                          future_map_dfc(~length(unique(.))) %>%
                          t() %>%
                          as_tibble(rownames = "variable") %>%
                          rename(unique = V1) %>%
                          filter(unique == 1) %>%
                          .$variable

# Get Rid of Duplicated Variables

    duplicated.names <- names(w1w2suicide)[duplicated(as.list(w1w2suicide))]

    duplicated.names %>%
        as.data.frame() %>%
        write_csv(x = . , here("Data","Processed_Data", "NESARC_duplicated_Variables.csv"))


    w1w2suicide.raw <- w1w2suicide %>%
                        select(-suicide_334_)

    w1w2suicide <- w1w2suicide %>%
                    select(-duplicated.names)

# Write out data

    rm(w1w2_numeric_quantiles, w1w2suicide_num)

    save.image(here("Data","Processed_Data", "w1w2suicidedata_Clean_May2020.RData"))
