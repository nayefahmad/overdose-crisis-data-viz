###Corrrect for the missing years
data <- data %>% mutate(year_pub = 
                          ifelse(year_pub %in% c("Unclear", "unclear"), NA, year_pub)
)

####A couple of papers were missign paper type or outcomes y/n
data <- data %>% mutate(paper_type = ifelse(record_id %in% c("Mallatt_2019_2", "Sears_2019"), 1, paper_type), 
                        outcomes_yn = ifelse(record_id == "Minhee_2019", 0, outcomes_yn))


####renameing the over teh counter variable - for some reason redcap coding it as an impact variable
data <- data %>% 
  rename("outcome_otc___1" = impact_non_pres_avail_2___1, 
         "outcome_otc___2" = impact_non_pres_avail_2___2, 
         "outcomes_non_pres_other" = outcome_non_pres_other, 
         "outcomes_non_pres_other_2" = outcome_non_pres_other_2, 
         "outcomes_non_pres_other_3" = outcome_non_pres_other_3, 
         "outcomes_prescription_other" = outcomes_pres_other, 
         "outcomes_prescription_other_2" = outcomes_pres_other_2, 
         "outcomes_prescription_other_3" = outcomes_pres_other_3)

######Some of the policies that are classified as other actually belonged in a different category -- recategorize (did this after i got the policies into their their own column)
#####


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
  select(record_id, policy_type, policy_op, policy_specific, policy_specific_new, everything())
