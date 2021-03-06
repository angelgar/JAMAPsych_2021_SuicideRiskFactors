This script will read in the raw Wave 1 data (w1mother.sas7bdat), W2
suicide variables (w2mother\_suicideonly.csv), and derived income
measures (not in the raw dataset only in w1w2suicidedata.RData) and
merge them all. To execute this script. You need to have the raw data
files “w1mother.sas7bdat”, “w2mother\_suicideonly.csv”, and
“w1w2suicidedata.RData” under the “/Data/Raw\_Data” directory. Start
with this script and then move on to Script 2, Script 3, and so on.

    ## Load Data

    w1mother <- haven::read_sas(here("Data","Raw_Data", "w1mother.sas7bdat"))
    w2mother <- read_csv(here("Data","Raw_Data", "w2mother_suicideonly.csv"))
    load(here("Data","Raw_Data", "w1w2suicidedata.RData"))

    ## Generate Names

    names(w1mother) <- toupper(names(w1mother))
    keep <- toupper(names(w1mother)) %in% c(w1vars1$code, "INCPER3", "INCFAM3")

    ## Add Income Variables INCPER3 INCFAM3 not in the raw dataset (this is derived income)

    not.mother <-  w1vars1$code[!(w1vars1$code %in% toupper(names(w1mother)))]

    ## Filter and Merge

    w1mother.codebook <- w1mother %>% select(which(keep == TRUE)) ## I updated this in Feb 2020 cause code was broken
                  
    w1w2suicide.v2 <- w2mother %>%
                      select(IDNUM, W2S4AQ3A16, W2S14Q17A:W2S14Q17C, W2AGE) %>%
                      left_join(w1mother.codebook, by = "IDNUM")

    save("w1w2suicide.v2", 
         file = here("Data","Raw_Data", "w1w2suicidedata_raw_May2020.RData"))
