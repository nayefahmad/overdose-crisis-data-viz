library(tidyverse)
library(here)

data <- read_csv(here("data", 
                      "ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv"))
#check <- read_csv('ScopingReviewOfOpioi-FinalResults_DATA_2019-12-18_1354.csv')
data <- data %>% mutate(year_pub = 
                          ifelse(year_pub %in% c("Unclear", "unclear"), NA, year_pub)
)

table(data$paper_type)
table(data$outcomes_yn)

fix_domain <- data$record_id[is.na(data$paper_type)]
fix_outcomes <- data$record_id[is.na(data$outcomes_yn)]
fix_countries <- data$record_id[is.na(data$country_yn)]

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

data_n <- data %>% mutate_at(11:411, as.character)  # why not factor? 
sapply(data_n, class)

#rename variables, will need this later for pivot_longer function
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

dchk <- colnames(test)
dchk

# colnames with "_other" in the middle (not end). These weren't replaced above. 
lu <- dchk[str_detect(dchk, "_other")]
lu

###collect the 'single' columsn to own subset
single_cols <- test %>% select(1:18)


###need to gather multiple columns into multiple new colums - pivot_longer

data_long <- test %>% 
  pivot_longer(cols = country_income___1:impact_other_3___999, 
               names_to = c("variables", "code"),
               values_to = "out", 
               names_pattern = "(.*)___(.*)") %>% 
  filter(out != 0)
#want to 'spread' this out but glitching because some of the 'key' variables have multiple codes. 


lookup <-read_csv(file = "lookup_rd.csv")  # where is this file? 

lookup_a <- lookup %>% select(-impact_var)

data_long_code <- data_long %>% 
  mutate(variable_merge = ifelse(str_detect(variables, pattern = "impact_"), "impact", variables)) %>% 
  left_join(lookup_a, by = c("variable_merge", "code")) %>% 
  mutate(code_name = ifelse(code %in% c("9090", "9090_2", "9090_3"), out, code_name))

dara_long_w2 <- data_long_code %>% 
  select(-c(variable_merge, out)) %>% 
  # filter(code != "999") %>% 
  unite(code_comb, c(code, code_name)) %>% 
  group_by(record_id, variables) %>%
  arrange(record_id) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = variables, values_from = code_comb) %>%
  select(-row)
##keep the unite 
summary <- data_long %>% 
  group_by(variables, code) %>% 
  summarise(total = n_distinct(record_id))

varaiables <- data_long %>% 
  select(variables) %>% 
  unique()

#write_csv(varaiables, path="variable_list.csv")

##Need policy_type_ in one column, policies_ in one column and policy_criminal:policy_theme_other_3 in one column

policy_types <- dara_long_w2 %>% 
  select(record_id, policy_type) %>% 
  unique() %>% 
  filter(!is.na(policy_type))

specific_policy <- dara_long_w2 %>%  
  select(record_id, starts_with("policy")) %>% 
  select(-policy_type) %>% 
  unique()

##some rows that are missign across all policy columns - remove them
specific_policy$sum_missing <- apply(specific_policy, MARGIN = 1, function(x) sum(is.na(x)))

specific_policy <- specific_policy %>% 
  filter(sum_missing != 10) %>% 
  select(-sum_missing)

specifics_long <- specific_policy %>% 
  group_by(record_id) %>% 
  gather(policy_op, policy_specific, starts_with("policy")) %>% 
  filter(!is.na(policy_specific)) %>% 
  filter(!str_detect(policy_specific, "999|998|997")) %>% 
  arrange(record_id)

specifics_policy_f <- policy_types %>% 
  left_join(specifics_long, by="record_id") %>% 
  arrange(record_id)

####Outcomes

specific_outcomes <- dara_long_w2 %>%  
  select(record_id, starts_with("outcome")) %>% 
  mutate(outcome_type = ifelse(outcome_type %in% c("999_other 1", "998_other 2", "997_other 3", "4_overdose rates", "5_criminal activity", "8_health and  social services systems costs"), outcome_type, NA)) %>% 
  #select(-outcome_type) %>% 
  unique()

specifics_out_long <- specific_outcomes %>% 
  group_by(record_id, outcomes_yn) %>% 
  gather(out_op, out_specific, 3:ncol(specific_outcomes)) %>% 
  filter(!is.na(out_specific)) %>% #filter out missing
  #filter(!(str_detect(out_specific, "9090") & out_op == "outcome")) %>% 
  #filter(!str_detect(out_specific, "998|999|997")) %>% 
  #filter(!str_detect(out_specific, "997")) %>% 
  mutate(out_op = ifelse(out_op == "outcome", "outcome_other", out_op)) %>% 
  arrange(record_id, out_specific)

##can't filter out the 999,997 and 998 bc they are needed to link to the impact
other_out_only <- dara_long_w2 %>%  select(record_id, outcome_type, outcome)
other_ids <- unique(other_out_only$record_id[str_detect(other_out_only$outcome_type, "other")])

other_out_onlyb <- other_out_only %>% 
  filter(record_id %in% other_ids) %>% 
  mutate(outcome_type = ifelse(str_detect(outcome_type, "other"), outcome_type, NA)) %>% 
  nest(data=c(outcome_type, outcome))%>% 
  mutate(data = map(data, ~ map_dfc(., na.omit))) %>% 
  unnest(cols=c(data)) %>% 
  rename(other_outcome = outcome, out_specific = outcome_type) %>% 
  mutate(out_op = "outcome_type")

other_out_sub <- specifics_out_long %>% 
  ungroup() %>% 
  filter(str_detect(out_specific, "9090|999|997|998") & out_op != "outcome_type") %>% 
  select(-outcomes_yn) %>% 
  #group_by(record_id, out_op) %>% 
  arrange(record_id) %>% mutate(merge_code = ifelse(str_detect(out_specific, "9090_2"), 998, 
                                                    ifelse(str_detect(out_specific, "9090_3"), 997, 
                                                           ifelse(str_detect(out_specific, "999|998|997"), NA, 
                                                                  999))))

a_sub <-other_out_sub %>% filter(is.na(merge_code)) %>% 
  mutate(merge_code = as.numeric(substr(out_specific, 1, 3)))

b_sub <- other_out_sub %>%  filter(!is.na(merge_code)) %>% 
  rename(other_outcome = out_specific)


combo_sub <- a_sub %>% left_join(b_sub, by = c("record_id", "out_op", "merge_code")) %>% 
  select(-merge_code)

other_out_onlyc <- bind_rows(other_out_onlyb, combo_sub)
specifics_out_long <- filter(specifics_out_long, !(str_detect(out_specific, "9090")))
##create combo var in teh lookup
lookup_b <- lookup %>% rename(out_op = variable_merge) %>% 
  unite(out_specific, c(code, code_name), remove = FALSE)#%>% select(out_op, out_specific, impact_var)

##merge on specific out to specfic impact based on combo var (code + code_name) in lookup
##if out_op = outcome_type and out_specific = other 1 then impact is same as other 1
#lookup_c <- lookup_b %>%  filter()
specifics_policy_out_f <- specifics_policy_f %>% 
  left_join(specifics_out_long, by = "record_id") %>% 
  mutate(outcomes_yn = ifelse(is.na(outcomes_yn), "0", outcomes_yn)) %>% 
  left_join(lookup_b, by = c("out_op", "out_specific"))

specific_impact <- dara_long_w2 %>%  
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
###############Some of the other policies should be reclassified, if we correct the policy we also need to correct the policy option.

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


