
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
    specific_policy <- data_long_w2 %>%  
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


get_other_out_onlyb <- function(data_long_w2) {

    other_out_only <- 
        data_long_w2 %>% 
        select(record_id, outcome_type, outcome)
    
    other_ids <- unique(other_out_only$record_id[str_detect(other_out_only$outcome_type, "other")])
    
    other_out_onlyb <- other_out_only %>% 
        filter(record_id %in% other_ids) %>% 
        mutate(outcome_type = ifelse(str_detect(outcome_type, "other"), outcome_type, NA)) %>% 
        drop_na() %>% 
        
        # nest(data=c(outcome_type, outcome)) %>% 
        # mutate(data = map(data, ~ map_dfc(., na.omit))) %>% 
        # unnest(cols=c(data)) %>% 
        
        rename(other_outcome = outcome, 
               out_specific = outcome_type) %>% 
        mutate(out_op = "outcome_type") 
    
    return(other_out_onlyb)
}


get_specifics_out_long <- function(data_long_w2) {
    specific_outcomes <- data_long_w2 %>%  
        select(record_id, starts_with("outcome")) %>% 
        mutate(outcome_type = ifelse(outcome_type %in% c("999_other 1", "998_other 2", "997_other 3", "4_overdose rates", "5_criminal activity", "8_health and  social services systems costs"), outcome_type, NA)) %>% 
        #select(-outcome_type) %>% 
        unique()
    
    specifics_out_long <- specific_outcomes %>% 
        group_by(record_id, outcomes_yn) %>% 
        tidyr::gather(key = out_op, 
                      value = out_specific, 
                      3:ncol(specific_outcomes)) %>% 
        filter(!is.na(out_specific)) %>% #filter out missing
        #filter(!(str_detect(out_specific, "9090") & out_op == "outcome")) %>% 
        #filter(!str_detect(out_specific, "998|999|997")) %>% 
        #filter(!str_detect(out_specific, "997")) %>% 
        mutate(out_op = ifelse(out_op == "outcome", "outcome_other", out_op)) %>% 
        arrange(record_id, out_specific) %>% 
        filter(!(str_detect(out_specific, "9090")))
    
    return(specifics_out_long)
    
}


get_other_out_sub <- function(specifics_out_long) {
    other_out_sub <- specifics_out_long %>% 
        ungroup() %>% 
        filter(str_detect(out_specific, "9090|999|997|998") & out_op != "outcome_type") %>% 
        select(-outcomes_yn) %>% 
        #group_by(record_id, out_op) %>% 
        arrange(record_id) %>% 
        mutate(merge_code = ifelse(str_detect(out_specific, "9090_2"), 998, 
                                   ifelse(str_detect(out_specific, "9090_3"), 997, 
                                          ifelse(str_detect(out_specific, "999|998|997"), NA, 
                                                 999)
                                   )
        )
        )
    return(other_out_sub)
    
}


get_other_out_onlyc <- function(other_out_sub, other_out_onlyb) {
    a_sub <- other_out_sub %>% 
        filter(is.na(merge_code)) %>% 
        mutate(merge_code = as.numeric(substr(out_specific, 1, 3)))
    
    b_sub <- other_out_sub %>%
        filter(!is.na(merge_code)) %>% 
        rename(other_outcome = out_specific)
    
    combo_sub <- a_sub %>% 
        left_join(b_sub, by = c("record_id", "out_op", "merge_code")) %>% 
        select(-merge_code)
    
    other_out_onlyc <- bind_rows(other_out_onlyb, combo_sub)
    
    return(other_out_onlyc)
}


get_final_mod_cor_policy <- function(specifics_policy_f, 
                                     data_long_w2, 
                                     specifics_out_long, 
                                     lookup_b, 
                                     other_out_onlyc) {
    
    specifics_policy_out_f <- specifics_policy_f %>% 
        left_join(specifics_out_long, by = "record_id") %>% 
        mutate(outcomes_yn = ifelse(is.na(outcomes_yn), "0", outcomes_yn)) %>% 
        left_join(lookup_b, by = c("out_op", "out_specific"))
    
    specific_impact <- data_long_w2 %>%  
        select(record_id, starts_with("impact")) %>% 
        unique() %>% 
        pivot_longer(2:ncol(.), names_to = "impact_var", 
                     values_to = "impact", 
                     values_drop_na = T 
        )
    
    final <- specifics_policy_out_f %>% 
        left_join(other_out_onlyc, by=c("record_id", "out_op", "out_specific")) %>% 
        left_join(specific_impact, by = c("record_id", "impact_var")) %>% 
        rename(out_code = code, out_code_name = code_name)
    
    final_mod <- final %>%  
        mutate(out_specific = ifelse(!is.na(other_outcome), other_outcome, out_specific))
    
    final_mod_cor_policy<- final_mod %>% 
        mutate(policy_op = ifelse(str_detect(policy_specific, regex("9090_prior_authorization", ignore_case = T)), "policy_social",
                                  ifelse(str_detect(policy_specific, regex("9090_medicaid", ignore_case = T)), "policy_social", 
                                         ifelse(str_detect(policy_specific, regex("9090_naloxone|9090_2_naloxone|9090_mandated co-prescriptions of naloxone|nalxone|take home naloxone|take home nalaxone|prescription of naloxone|9090_access to naloxone", ignore_case = T)), "policy_harm", 
                                                ifelse(str_detect(policy_specific, regex("9090_bill 123", ignore_case=T)), "policy_clinical", 
                                                       ifelse(str_detect(policy_specific, regex("9090_bill 123", ignore_case=T)), "policy_clinical", 
                                                              ifelse(str_detect(policy_specific, regex("9090_involuntary treatment", ignore_case=T)), "policy_clinical", 
                                                                     ifelse(str_detect(policy_specific, regex("prescription heroin|heroin prescription|heroine-assisted treatment|9090_HAT", ignore_case=T)), "policy_clinical", policy_op))))))), 
               policy_specific = ifelse(str_detect(policy_specific, regex("9090_prior authorization", ignore_case = T)), "2_medical insurance changes",
                                        ifelse(str_detect(policy_specific, regex("9090_medicaid", ignore_case = T)), "2_medical insurance changes", 
                                               ifelse(str_detect(policy_specific, regex("9090_involuntary treatment", ignore_case=T)), "7_compulsory_treatment",
                                                      ifelse(str_detect(policy_specific, regex("9090_bill 123", ignore_case=T)), "7_compulsory_treatment", policy_specific))))) %>% 
        mutate(policy_specific_new = ifelse(str_detect(policy_specific, regex("9090_naloxone|9090_2_naloxone|9090_mandated co-prescriptions of naloxone|nalxone|take home naloxone|take home nalaxone|prescription of naloxone|9090_access to naloxone", ignore_case = T)), "9090_naloxone_access_policies", 
                                            ifelse(str_detect(policy_specific, regex("pregnant|prenatal|pregnancy|prenanacy", ignore_case = T)), "9090_pregnancy_related_drug_charges", 
                                                   ifelse(str_detect(policy_specific, regex("9090_diversion", ignore_case = T)), "9090_diversion", 
                                                          ifelse(str_detect(policy_specific, regex("services available on release from corrections|pre-release (from prison) counselling", ignore_case = T)), "9090_release_from_prison_services",
                                                                 ifelse(str_detect(policy_specific, regex("prescription heroin|heroin prescription|heroine-assisted treatment|9090_HAT", ignore_case=T)), "9090_prescription_heroin",
                                                                        ifelse(str_detect(policy_specific, regex("ER/LA|risk evaluation and mitigation|REMS", ignore_case = T)), "9090_ER/LA Risk Evaluation and Mitigation Strategy", policy_specific))))
                                            ))) %>% 
        
        select(record_id, 
               policy_type, 
               policy_op, 
               policy_specific, 
               policy_specific_new, 
               everything())
    
    return(final_mod_cor_policy)
    
    
}

