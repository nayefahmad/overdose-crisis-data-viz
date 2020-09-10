
#' Get cleaned data 
#'
#' @param data Raw dataframe, from file ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv 
#'
#' @return Cleaned dataframe (before converting to long format) 
#' 
get_clean_data <- function(data){
    data <- 
        data %>% mutate(year_pub = ifelse(year_pub %in% c("Unclear", "unclear"), NA, year_pub))
    
    data <- 
        data %>% mutate(paper_type = ifelse(record_id %in% c("Mallatt_2019_2", "Sears_2019"), 1, paper_type), 
                            outcomes_yn = ifelse(record_id == "Minhee_2019", 0, outcomes_yn))
    
    # renaming the over the counter variable - for some reason redcap coding it as an impact variable
    data <- data %>% 
        rename(outcome_otc___1 = impact_non_pres_avail_2___1, 
               outcome_otc___2 = impact_non_pres_avail_2___2, 
               outcomes_non_pres_other = outcome_non_pres_other, 
               outcomes_non_pres_other_2 = outcome_non_pres_other_2, 
               outcomes_non_pres_other_3 = outcome_non_pres_other_3, 
               outcomes_prescription_other = outcomes_pres_other, 
               outcomes_prescription_other_2 = outcomes_pres_other_2, 
               outcomes_prescription_other_3 = outcomes_pres_other_3)
    
    data_n <- data %>% mutate_at(11:411, as.character)  # todo: why not factor? 
    sapply(data_n, class)
    
    # rename variables ----
    # will need this later for pivot_longer function
    test <- data_n %>% 
        select(-longitudinal_yn) %>%
        rename_at(vars(ends_with("_other")), 
                  funs(str_replace(., "_other", "___9090"))) %>% 
        rename_at(vars(ends_with("_other_2")), 
                  funs(str_replace(., "_other", "___9090"))) %>% 
        rename_at(vars(ends_with("_other_3")), 
                  funs(str_replace(., "_other", "___9090"))) %>% 
        select(1:10, 50:53, 103, 193, 409:410, everything()) %>% 
        rename("population_specific___9090" = population___9090, 
               "population_specific_affect___9090" = population___9090_2)
    
    return(test)
}


get_data_long <- function(test){
    data_long <- test %>% 
        pivot_longer(cols = country_income___1:impact_other_3___999,  # everything after the first 18 cols 
                     names_to = c("variables", "code"),
                     names_pattern = "(.*)___(.*)",  
                     values_to = "out") %>% 
        filter(out != 0)
    return(data_long)
}


get_data_long_code <- function(data_long, lookup_a){
    data_long_code <- 
        data_long %>% 
        mutate(variable_merge = ifelse(str_detect(variables, pattern = "impact_"), 
                                       "impact",
                                       variables)) %>% 
        left_join(lookup_a, by = c("variable_merge", "code")) %>% 
        mutate(code_name = ifelse(code %in% c("9090", "9090_2", "9090_3"), 
                                  out,
                                  code_name))
    return(data_long_code)
    
}


get_data_long_w2 <- function(data_long_code){
    data_long_w2 <- data_long_code %>% 
        select(-c(variable_merge, out)) %>% 
        # filter(code != "999") %>%
        unite(code_comb, c(code, code_name)) %>%
        group_by(record_id, variables) %>%
        arrange(record_id) %>%
        mutate(row = row_number()) %>%
        pivot_wider(names_from = variables, 
                    values_from = code_comb) %>% 
        
        ungroup() %>% 
        group_by(record_id) %>% 
        fill(country_income:policy_international)
    
    return(data_long_w2)
}


get_policy_types <- function(data_long_w2){
    policy_types <- data_long_w2 %>% 
        select(record_id, policy_type) %>% 
        unique() %>% 
        filter(!is.na(policy_type))
    return(policy_types)
    
}


get_specifics_policy_f <- function(data_long_w2, policy_types){
    pecific_policy <- data_long_w2 %>%  
        select(record_id, starts_with("policy")) %>% 
        select(-policy_type) %>% 
        unique()
    
    # some rows that are missing across all policy columns - remove them
    specific_policy$sum_missing <- apply(specific_policy, 
                                         MARGIN = 1,
                                         function(x) sum(is.na(x)))
    
    specific_policy <- specific_policy %>% 
        filter(sum_missing != 10) %>% 
        select(-sum_missing)
    
    specifics_long <- specific_policy %>% 
        group_by(record_id) %>% 
        tidyr::gather(policy_op, policy_specific, starts_with("policy")) %>% 
        filter(!is.na(policy_specific)) %>% 
        filter(!str_detect(policy_specific, "999|998|997")) %>% 
        arrange(record_id)
    
    specifics_policy_f <- policy_types %>% 
        left_join(specifics_long, by="record_id") %>% 
        arrange(record_id)
    
    return(specifics_policy_f)
}



