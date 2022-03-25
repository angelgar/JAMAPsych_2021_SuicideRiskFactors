# Identification of Suicide Attempt Risk Factors in a National US Survey Using Machine Learning

This is an accompaning github repository containing all codes related to the paper <Identification of Suicide Attempt Risk Factors in a National US Survey Using Machine Learning>(https://jamanetwork.com/journals/jamapsychiatry/fullarticle/2774348) published in JAMA Psychiatry 2021.

## Abstract

**Importance**  Because more than one-third of people making nonfatal suicide attempts do not receive mental health treatment, it is essential to extend suicide attempt risk factors beyond high-risk clinical populations to the general adult population.

**Objective**  To identify future suicide attempt risk factors in the general population using a data-driven machine learning approach including more than 2500 questions from a large, nationally representative survey of US adults.

**Design, Setting, and Participants**  Data came from wave 1 (2001 to 2002) and wave 2 (2004 to 2005) of the National Epidemiologic Survey on Alcohol and Related Conditions (NESARC). NESARC is a face-to-face longitudinal survey conducted with a national representative sample of noninstitutionalized civilian population 18 years and older in the US. The cumulative response rate across both waves was 70.2% resulting in 34 653 wave 2 interviews. A balanced random forest was trained using cross-validation to develop a suicide attempt risk model. Out-of-fold model prediction was used to assess model performance, including the area under the receiver operator curve, sensitivity, and specificity. Survey design and nonresponse weights allowed estimates to be representative of the US civilian population based on the 2000 census. Analyses were performed between May 15, 2019, and June 10, 2020.

**Main Outcomes and Measures**  Attempted suicide in the 3 years between wave 1 and wave 2 interviews.

**Results**  Of 34653 participants, 20089 were female (weighted proportion, 52.1%). The weighted mean (SD) age was 45.1 (17.3) years at wave 1 and 48.2 (17.3) years at wave 2. Attempted suicide during the 3 years between wave 1 and wave 2 interviews was self-reported by 222 of 34 653 participants (0.6%). Using survey questions measured at wave 1, the suicide attempt risk model yielded a cross-validated area under the receiver operator characteristic curve of 0.857 with a sensitivity of 85.3% (95% CI, 79.8-89.7) and a specificity of 73.3% (95% CI, 72.8-73.8) at an optimized threshold. The model identified 1.8% of the US population to be at a 10% or greater risk of suicide attempt. The most important risk factors were 3 questions about previous suicidal ideation or behavior; 3 items from the 12-Item Short Form Health Survey, namely feeling downhearted, doing activities less carefully, or accomplishing less because of emotional problems; younger age; lower educational achievement; and recent financial crisis.

**Conclusions and Relevance**  In this study, after searching through more than 2500 survey questions, several well-known risk factors of suicide attempt were confirmed, such as previous suicidal behaviors and ideation, and new risks were identified, including functional impairment resulting from mental disorders and socioeconomic disadvantage. These results may help guide future clinical assessment and the development of new suicide risk scales.

## Data

The data used for this analysis came from Wave 1 and Wave 2 of the National Epidemiologic Survey on Alcohol and Related Conditions (NESARC). This data is available to download directly from the NESARC Website <http://niaaa.census.gov>. Additional information on how to download this data is listed here: <https://pubs.niaaa.nih.gov/publications/arh29-2/74-78.htm>. 

The initial files used for this analysis are the Wave 1 Mother dataset (w1mother.sas7bdat) and the subset of suicide questions from Wave 2 (w2mother_suicideonly.csv)

## Code 

### Processing

The code is divided into two folders. The first (Data_Processing) contains all the necessary scripts to process the data into R. There are three scripts in this folder that have to be run in order:

1. Script1_GenerateSample_JAMA2021_20220325: Merges the raw SAS version of the Wave 1 with the suicide variables from Wave 2
2. Script2_CaseDefition_JAMA2021_20220325: Defines the cases for this analysis as detailed in the Methods section of the paper
3. Script3_CleanData_JAMA2021_20220325: This script processes and organizes the risk factors variables in the data. This script retypes variables into categorical vs. numeric, identifies duplicated variables, and codes missing values. A more detailed explanation of this process is included in the supplement (see "Organizing the risk factor variables").   

### Main Analysis 

The main analysis folder contains three scripts that have to be run in order:

1) Script4_TrainModel_JAMA2021_20220325: This script trains the Balanced Random Forest model used for the rest of the analysis 
2) Script5_MainFigures_JAMA2021_20220325: This script generates the  figures from the main body of the text 
3) Script6_SupplementFigures_JAMA2021_202203250: This script generates figures from the supplement