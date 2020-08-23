---
title: "Overdose crisis data - EDA"
author: "Nayef Ahmad"
date: "8/22/2020"
output: 
  html_document: 
    keep_md: yes
    code_folding: hide
    toc: true
    toc_float:
      collapsed: false
    toc_folding: false
---



# Libraries 

```r
library(tidyverse)
library(here)
```

# Overview 



# Wide data 

```r
df1_raw <- read_csv(here::here("data", "ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv"))

nrow(df1_raw)
```

```
## [1] 717
```

```r
# str(df1_raw)
names(df1_raw)
```

```
##   [1] "record_id"                        "redcap_event_name"               
##   [3] "redcap_repeat_instrument"         "redcap_repeat_instance"          
##   [5] "paper_type"                       "name"                            
##   [7] "dv_name"                          "year_pub"                        
##   [9] "country_yn"                       "country"                         
##  [11] "country_income___1"               "country_income___2"              
##  [13] "country_income___3"               "country_income___4"              
##  [15] "country_region___1"               "country_region___2"              
##  [17] "country_region___3"               "country_region___4"              
##  [19] "country_region___5"               "country_region___6"              
##  [21] "country_region___7"               "target_substance___a"            
##  [23] "target_substance___b"             "target_substance___c"            
##  [25] "target_substance___d"             "target_substance___e"            
##  [27] "target_substance___f"             "population_specific___0"         
##  [29] "population_specific___1"          "population_specific___2"         
##  [31] "population_specific___3"          "population_specific___4"         
##  [33] "population_specific___5"          "population_specific___6"         
##  [35] "population_specific___7"          "population_specific___9"         
##  [37] "population_specific___999"        "population_other"                
##  [39] "population_specific_affect___0"   "population_specific_affect___1"  
##  [41] "population_specific_affect___2"   "population_specific_affect___3"  
##  [43] "population_specific_affect___4"   "population_specific_affect___5"  
##  [45] "population_specific_affect___6"   "population_specific_affect___7"  
##  [47] "population_specific_affect___9"   "population_specific_affect___999"
##  [49] "population_other_2"               "competing_interests_yn"          
##  [51] "competing_interests"              "empirical"                       
##  [53] "year_study"                       "study_design___1"                
##  [55] "study_design___2"                 "study_design___3"                
##  [57] "study_design___4"                 "study_design___5"                
##  [59] "study_design___7"                 "study_design___6"                
##  [61] "study_design___8"                 "study_design___9"                
##  [63] "study_design___10"                "study_design___999"              
##  [65] "study_design_other"               "longitudinal_yn"                 
##  [67] "data_sources___1"                 "data_sources___2"                
##  [69] "data_sources___3"                 "data_sources___6"                
##  [71] "data_sources___4"                 "data_sources___5"                
##  [73] "data_sources___999"               "data_sources_other"              
##  [75] "analysis___0"                     "analysis___1"                    
##  [77] "analysis___2"                     "analysis___3"                    
##  [79] "analysis___6"                     "analysis___4"                    
##  [81] "analysis___5"                     "analysis___7"                    
##  [83] "analysis___8"                     "analysis___999"                  
##  [85] "analysis_other"                   "policy_type___1"                 
##  [87] "policy_type___2"                  "policy_type___3"                 
##  [89] "policy_type___4"                  "policy_type___5"                 
##  [91] "policy_type___6"                  "policy_type___7"                 
##  [93] "policy_type___8"                  "gov_level___1"                   
##  [95] "gov_level___2"                    "gov_level___3"                   
##  [97] "gov_level___4"                    "court_level___1"                 
##  [99] "court_level___2"                  "court_level___3"                 
## [101] "court_level___4"                  "court_level___5"                 
## [103] "court_level___6"                  "appeal_yn"                       
## [105] "policies___1"                     "policies___2"                    
## [107] "policies___3"                     "policies___4"                    
## [109] "policies___5"                     "policies___6"                    
## [111] "policies___7"                     "policies___8"                    
## [113] "policies___999"                   "policies___998"                  
## [115] "policies___997"                   "policy_criminal___1"             
## [117] "policy_criminal___2"              "policy_criminal___999"           
## [119] "policy_criminal___998"            "policy_criminal___997"           
## [121] "policy_criminal_other"            "policy_criminal_other_2"         
## [123] "policy_criminal_other_3"          "policy_legislative___1"          
## [125] "policy_legislative___2"           "policy_legislative___3"          
## [127] "policy_legislative___4"           "policy_legislative___5"          
## [129] "policy_legislative___999"         "policy_legislative___998"        
## [131] "policy_legislative___997"         "policy_legislative_other"        
## [133] "policy_legislative_other_2"       "policy_legislative_other_3"      
## [135] "policy_law_enforce___1"           "policy_law_enforce___2"          
## [137] "policy_law_enforce___999"         "policy_law_enforce___998"        
## [139] "policy_law_enforce___997"         "policy_law_enforce_other"        
## [141] "policy_law_enforce_other_2"       "policy_law_enforce_other_3"      
## [143] "policy_harm___1"                  "policy_harm___3"                 
## [145] "policy_harm___4"                  "policy_harm___5"                 
## [147] "policy_harm___999"                "policy_harm___998"               
## [149] "policy_harm___997"                "policy_harm_other"               
## [151] "policy_harm_other_2"              "policy_harm_other_3"             
## [153] "policy_prescription___1"          "policy_prescription___2"         
## [155] "policy_prescription___3"          "policy_prescription___999"       
## [157] "policy_prescription___998"        "policy_prescription___997"       
## [159] "policy_prescription_other"        "policy_prescription_other_2"     
## [161] "policy_prescription_other_3"      "policy_clinical___1"             
## [163] "policy_clinical___2"              "policy_clinical___3"             
## [165] "policy_clinical___4"              "policy_clinical___5"             
## [167] "policy_clinical___6"              "policy_clinical___7"             
## [169] "policy_clinical___999"            "policy_clinical___998"           
## [171] "policy_clinical___997"            "policy_clinical_other"           
## [173] "policy_clinical_other_2"          "policy_clinical_other_3"         
## [175] "policy_social___1"                "policy_social___2"               
## [177] "policy_social___3"                "policy_social___999"             
## [179] "policy_social___998"              "policy_social___997"             
## [181] "policy_social_other"              "policy_social_other_2"           
## [183] "policy_social_other_3"            "policy_international___1"        
## [185] "policy_international___999"       "policy_international___998"      
## [187] "policy_international___997"       "policy_international_other"      
## [189] "policy_international_other_2"     "policy_international_other_3"    
## [191] "policy_theme_other"               "policy_theme_other_2"            
## [193] "policy_theme_other_3"             "outcomes_yn"                     
## [195] "outcome_type___1"                 "outcome_type___10"               
## [197] "outcome_type___2"                 "outcome_type___3"                
## [199] "outcome_type___4"                 "outcome_type___5"                
## [201] "outcome_type___6"                 "outcome_type___7"                
## [203] "outcome_type___8"                 "outcome_type___999"              
## [205] "outcome_type___998"               "outcome_type___997"              
## [207] "outcomes_prescription___1"        "outcomes_prescription___2"       
## [209] "outcomes_prescription___3"        "outcomes_prescription___5"       
## [211] "outcomes_prescription___999"      "outcomes_prescription___998"     
## [213] "outcomes_prescription___997"      "outcomes_pres_other"             
## [215] "outcomes_pres_other_2"            "outcomes_pres_other_3"           
## [217] "impact_opr___0"                   "impact_opr___1"                  
## [219] "impact_opr___2"                   "impact_opr___3"                  
## [221] "impact_opr___4"                   "impact_opr___999"                
## [223] "impact_opb___0"                   "impact_opb___1"                  
## [225] "impact_opb___2"                   "impact_opb___3"                  
## [227] "impact_opb___4"                   "impact_opb___999"                
## [229] "impact_oa___0"                    "impact_oa___1"                   
## [231] "impact_oa___2"                    "impact_oa___3"                   
## [233] "impact_oa___4"                    "impact_oa___999"                 
## [235] "impact_ou___0"                    "impact_ou___1"                   
## [237] "impact_ou___2"                    "impact_ou___3"                   
## [239] "impact_ou___4"                    "impact_ou___999"                 
## [241] "impact_pres_other___0"            "impact_pres_other___1"           
## [243] "impact_pres_other___2"            "impact_pres_other___3"           
## [245] "impact_pres_other___4"            "impact_pres_other___999"         
## [247] "outcomes_non_pres___1"            "outcomes_non_pres___2"           
## [249] "outcomes_non_pres___999"          "outcomes_non_pres___998"         
## [251] "outcomes_non_pres___997"          "outcome_non_pres_other"          
## [253] "outcome_non_pres_other_2"         "outcome_non_pres_other_3"        
## [255] "impact_non_pres_avail___0"        "impact_non_pres_avail___1"       
## [257] "impact_non_pres_avail___2"        "impact_non_pres_avail___3"       
## [259] "impact_non_pres_avail___4"        "impact_non_pres_avail___999"     
## [261] "impact_non_pres_use___0"          "impact_non_pres_use___1"         
## [263] "impact_non_pres_use___2"          "impact_non_pres_use___3"         
## [265] "impact_non_pres_use___4"          "impact_non_pres_use___999"       
## [267] "impact_non_pres_other___0"        "impact_non_pres_other___1"       
## [269] "impact_non_pres_other___2"        "impact_non_pres_other___3"       
## [271] "impact_non_pres_other___4"        "impact_non_pres_other___999"     
## [273] "outcomes_treatment___1"           "outcomes_treatment___2"          
## [275] "outcomes_treatment___999"         "outcomes_treatment___998"        
## [277] "outcomes_treatment___997"         "outcomes_treatment_other"        
## [279] "outcomes_treatment_other_2"       "outcomes_treatment_other_3"      
## [281] "impact_access___0"                "impact_access___1"               
## [283] "impact_access___2"                "impact_access___3"               
## [285] "impact_access___4"                "impact_access___999"             
## [287] "impact_tfo___0"                   "impact_tfo___1"                  
## [289] "impact_tfo___2"                   "impact_tfo___3"                  
## [291] "impact_tfo___4"                   "impact_tfo___999"                
## [293] "impact_of___0"                    "impact_of___1"                   
## [295] "impact_of___2"                    "impact_of___3"                   
## [297] "impact_of___4"                    "impact_of___999"                 
## [299] "impact_of_2___0"                  "impact_of_2___1"                 
## [301] "impact_of_2___2"                  "impact_of_2___3"                 
## [303] "impact_of_2___4"                  "impact_of_2___999"               
## [305] "impact_of_3___0"                  "impact_of_3___1"                 
## [307] "impact_of_3___2"                  "impact_of_3___3"                 
## [309] "impact_of_3___4"                  "impact_of_3___999"               
## [311] "impact_overdose___0"              "impact_overdose___1"             
## [313] "impact_overdose___2"              "impact_overdose___3"             
## [315] "impact_overdose___4"              "impact_overdose___999"           
## [317] "impact_crim___0"                  "impact_crim___1"                 
## [319] "impact_crim___2"                  "impact_crim___3"                 
## [321] "impact_crim___4"                  "impact_crim___999"               
## [323] "outcome_infectious___1"           "outcome_infectious___2"          
## [325] "outcome_infectious___999"         "outcome_infectious___998"        
## [327] "outcome_infectious___997"         "outcome_infectious_other"        
## [329] "outcome_infectious_other_2"       "outcome_infectious_other_3"      
## [331] "impact_hiv___0"                   "impact_hiv___1"                  
## [333] "impact_hiv___2"                   "impact_hiv___3"                  
## [335] "impact_hiv___4"                   "impact_hiv___999"                
## [337] "impact_hepc___0"                  "impact_hepc___1"                 
## [339] "impact_hepc___2"                  "impact_hepc___3"                 
## [341] "impact_hepc___4"                  "impact_hepc___999"               
## [343] "impact_infect_other___0"          "impact_infect_other___1"         
## [345] "impact_infect_other___2"          "impact_infect_other___3"         
## [347] "impact_infect_other___4"          "impact_infect_other___999"       
## [349] "outcome_illegal___1"              "outcome_illegal___2"             
## [351] "outcome_illegal___999"            "outcome_illegal___998"           
## [353] "outcome_illegal___997"            "outcome_illegal_other"           
## [355] "outcome_illegal_other_2"          "outcome_illegal_other_3"         
## [357] "impact_idu___0"                   "impact_idu___1"                  
## [359] "impact_idu___2"                   "impact_idu___3"                  
## [361] "impact_idu___4"                   "impact_idu___999"                
## [363] "impact_dui___0"                   "impact_dui___1"                  
## [365] "impact_dui___2"                   "impact_dui___3"                  
## [367] "impact_dui___4"                   "impact_dui___999"                
## [369] "impact_dui_2___0"                 "impact_dui_2___1"                
## [371] "impact_dui_2___2"                 "impact_dui_2___3"                
## [373] "impact_dui_2___4"                 "impact_dui_2___999"              
## [375] "impact_health___0"                "impact_health___1"               
## [377] "impact_health___2"                "impact_health___3"               
## [379] "impact_health___4"                "impact_health___999"             
## [381] "impact_non_pres_avail_2___1"      "impact_non_pres_avail_2___2"     
## [383] "impact_otc___0"                   "impact_otc___1"                  
## [385] "impact_otc___2"                   "impact_otc___3"                  
## [387] "impact_otc___4"                   "impact_otc___999"                
## [389] "outcome_other"                    "impact_other___0"                
## [391] "impact_other___1"                 "impact_other___2"                
## [393] "impact_other___3"                 "impact_other___4"                
## [395] "impact_other___999"               "outcome_other_2"                 
## [397] "impact_other_2___0"               "impact_other_2___1"              
## [399] "impact_other_2___2"               "impact_other_2___3"              
## [401] "impact_other_2___4"               "impact_other_2___999"            
## [403] "outcome_other_3"                  "impact_other_3___0"              
## [405] "impact_other_3___1"               "impact_other_3___2"              
## [407] "impact_other_3___3"               "impact_other_3___4"              
## [409] "impact_other_3___999"             "comments"                        
## [411] "data_extraction_form_complete"
```

## Data wrangling

```r
df2_clean <- 
    df1_raw %>% 
    mutate(country_income_check = country_income___1 + 
               country_income___2 + 
               country_income___3 + 
               country_income___4
           )

df2_clean %>% 
    filter(country_yn == 1) %>% 
    count(country_income_check)
```

```
## # A tibble: 1 x 2
##   country_income_check     n
##                  <dbl> <int>
## 1                    1   640
```


## Counts 

```r
table(df1_raw$country_yn)
```

```
## 
##   0   1 
##  77 640
```

```r
df1_raw %>% count(country)
```

```
## # A tibble: 32 x 2
##    country     n
##      <dbl> <int>
##  1       8    24
##  2       9     1
##  3      16     1
##  4      30    37
##  5      44     4
##  6      50     1
##  7      57     1
##  8      58     6
##  9      60     1
## 10      61     1
## # ... with 22 more rows
```


# Long data 

```r
df3_long_raw_01 <- read_csv(here("data",
                                 "outfile_policies_impacts_20200712.csv"))

df4_long_raw_02 <- read_csv(here("data",
                                 "outfile_remaining_vars_2020_07_13.csv"))

names(df3_long_raw_01)
```

```
##  [1] "record_id"           "policy_type"         "policy_op"          
##  [4] "policy_specific"     "policy_specific_new" "outcomes_yn"        
##  [7] "out_op"              "out_specific"        "out_code"           
## [10] "out_code_name"       "impact_var"          "other_outcome"      
## [13] "impact"
```

```r
names(df4_long_raw_02)
```

```
##  [1] "record_id"                       "redcap_event_name.factor"       
##  [3] "redcap_repeat_instrument.factor" "redcap_repeat_instance"         
##  [5] "paper_type.factor"               "name"                           
##  [7] "dv_name"                         "year_pub"                       
##  [9] "country_yn.factor"               "country.factor"                 
## [11] "competing_interests_yn.factor"   "competing_interests.factor"     
## [13] "empirical.factor"                "year_study"                     
## [15] "appeal_yn.factor"                "comments"                       
## [17] "data_extraction_form_complete"   "country_region"                 
## [19] "target_substance"                "population_specific"            
## [21] "population_specific_affect"      "study_design"                   
## [23] "data_sources"                    "analysis"                       
## [25] "country_income"
```


# Questions 
1. What does 9090 represent in colnames? 
