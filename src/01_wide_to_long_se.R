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

# test for NAs: 
fix_domain <- data$record_id[is.na(data$paper_type)]
fix_outcomes <- data$record_id[is.na(data$outcomes_yn)]
fix_countries <- data$record_id[is.na(data$country_yn)]

# todo: why? 
data <- data %>% mutate(paper_type = ifelse(record_id %in% c("Mallatt_2019_2", "Sears_2019"), 1, paper_type), 
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

dchk <- colnames(test)
dchk

# colnames with "_other" in the middle (not end). These weren't replaced above. 
lu <- dchk[str_detect(dchk, "_other")]
lu

# collect the 'single' columns to own subset
single_cols <- test %>% select(1:18)


# pivot longer ---- 
# need to gather multiple columns into multiple new colums - pivot_longer
# this applies to everything after the first 18 cols 

# quick demo:  
# df <- data.frame(id = 1:3, 
#                  country_income___1 = c(0, 0, 1), 
#                  country_income___2 = c(1, 1, 0)); df
# 
# df %>% 
#   pivot_longer(cols = country_income___1:country_income___2,
#                names_to = c("variables", "code"),
#                names_pattern = "(.*)___(.*)",  
#                values_to = "out")

# actual implementation: 
data_long <- test %>% 
  pivot_longer(cols = country_income___1:impact_other_3___999,  # everything after the first 18 cols 
               names_to = c("variables", "code"),
               names_pattern = "(.*)___(.*)",  
               values_to = "out") %>% 
  filter(out != 0)
# todo: want to 'spread' this out but glitching because some of the 'key' variables have multiple codes. 

# import column lookup table ---- 
lookup <- read_csv(file = here("data", "lookup_rd.csv"))  
lookup_a <- lookup %>% select(-impact_var)

data_long_code <- data_long %>% 
  mutate(variable_merge = ifelse(str_detect(variables, pattern = "impact_"), 
                                 "impact",
                                 variables)) %>% 
  left_join(lookup_a, by = c("variable_merge", "code")) %>% 
  mutate(code_name = ifelse(code %in% c("9090", "9090_2", "9090_3"), 
                            out,
                            code_name))

data_long_w2 <- data_long_code %>% 
  select(-c(variable_merge, out)) %>% 
  # filter(code != "999") %>% 
  unite(code_comb, c(code, code_name)) %>% 
  group_by(record_id, variables) %>%
  arrange(record_id) %>% 
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = variables, values_from = code_comb) %>%
  select(-row)

# keep the unite 
summary <- data_long %>% 
  group_by(variables, code) %>% 
  summarise(total = n_distinct(record_id))

varaiables <- data_long %>% 
  select(variables) %>% 
  unique()

# write_csv(varaiables, path="variable_list.csv")

# policy cols ---- 
# Need policy_type_ in one column, policies_ in one column and 
# policy_criminal:policy_theme_other_3 in one column

policy_types <- data_long_w2 %>% 
  select(record_id, policy_type) %>% 
  unique() %>% 
  filter(!is.na(policy_type))

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
  tidyr::gather(key = policy_op, 
                value = policy_specific, 
                starts_with("policy")) %>% 
  filter(!is.na(policy_specific)) %>% 
  filter(!str_detect(policy_specific, "999|998|997")) %>% 
  arrange(record_id)

specifics_policy_f <- policy_types %>% 
  left_join(specifics_long, by="record_id") %>% 
  arrange(record_id)


#**********************************************************************
# Outcomes ---- 

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

# can't filter out the 999,997 and 998 bc they are needed to link to the impact
other_out_only <- data_long_w2 %>%  select(record_id, outcome_type, outcome)
other_ids <- unique(other_out_only$record_id[str_detect(other_out_only$outcome_type, "other")])

other_out_onlyb <- other_out_only %>% 
  filter(record_id %in% other_ids) %>% 
  mutate(outcome_type = ifelse(str_detect(outcome_type, "other"), outcome_type, NA)) %>% 
  nest(data=c(outcome_type, outcome)) %>% 
  mutate(data = map(data, ~ map_dfc(., na.omit))) %>% 
  unnest(cols=c(data)) %>% 
  rename(other_outcome = outcome, 
         out_specific = outcome_type) %>% 
  mutate(out_op = "outcome_type")

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


# create combo var in the lookup
lookup_b <- lookup %>% 
  rename(out_op = variable_merge) %>% 
  unite(out_specific, c(code, code_name), remove = FALSE)#%>% select(out_op, out_specific, impact_var)


# merge on specific out to specfic impact based on combo var (code + code_name) in lookup
# if out_op = outcome_type and out_specific = other 1 then impact is same as other 1
# lookup_c <- lookup_b %>%  filter()

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


# Some of the other policies should be reclassified, if we correct the policy 
# we also need to correct the policy option.

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


# "Remaining variables" ---- 
analy_pop <- data_long_w2 %>% 
  select(-c(starts_with("policy"), starts_with("outcome"), starts_with("impact"), gov_level, court_level)) %>% 
  unique() %>% 
  ungroup()


#poach redcaps code to assign values
analy_pop$redcap_event_name.factor = factor(analy_pop$redcap_event_name,levels=c("data_extraction_arm_1","pilot_arm_1"))
analy_pop$redcap_repeat_instrument.factor = factor(analy_pop$redcap_repeat_instrument,levels=c("data_extraction_form"))
analy_pop$paper_type.factor = factor(analy_pop$paper_type,levels=c("1","2","3"))
analy_pop$country_yn.factor = factor(analy_pop$country_yn,levels=c("1","0"))
analy_pop$country.factor = factor(analy_pop$country,levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","190","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150","151","152","153","154","155","156","157","158","159","160","161","162","163","164","165","166","167","168","169","170","171","172","173","174","175","176","177","178","179","180","181","182","183","184","185","186","187","188","189"))
analy_pop$competing_interests_yn.factor = factor(analy_pop$competing_interests_yn,levels=c("1","0"))
analy_pop$competing_interests.factor = factor(analy_pop$competing_interests,levels=c("1","2","3"))
analy_pop$empirical.factor = factor(analy_pop$empirical,levels=c("1","2"))
analy_pop$appeal_yn.factor = factor(analy_pop$appeal_yn,levels=c("1","0"))
#analy_pop$outcomes_yn.factor = factor(analy_pop$outcomes_yn,levels=c("1","0","999"))



levels(analy_pop$redcap_event_name.factor)=c("Data Extraction","Pilot")
levels(analy_pop$redcap_repeat_instrument.factor)=c("Data Extraction Form")
levels(analy_pop$paper_type.factor)=c("Health Sciences","Social Sciences","Legal")
levels(analy_pop$country_yn.factor)=c("Yes","No")
levels(analy_pop$country.factor)=c("Afghanistan","Albania","Algeria","Angola","Antigua and Barbuda","Argentina","Armenia","Australia","Austria","Azerbaijan","The Bahamas","Bahrain","Bangladesh","Barbados","Belarus","Belgium","Belize","Benin","Bhutan","Bolivia","Bosnia and Herzegovina","Botswana","Brazil","Brunei","Bulgaria","Burkina Faso","Burundi","Cambodia","Cameroon","Canada","Cape Verde","Central African Republic","Chad","Chile","Colombia","Comoros","Democratic Republic of the Congo","Republic of the Congo","Costa Rica","Côte dIvoire","Croatia","Cyprus","Czech Republic","Denmark","Djibouti","Dominica","Dominican Republic","East Timor","Ecuador","Egypt","El Salvador","Equatorial Guinea","Eritrea","Estonia","Ethiopia","Fiji","Finland","France","Gabon","Gambia","Georgia","Germany","Ghana","Greece","Grenada","Guatemala","Guinea","Guinea-Bissau","Guyana","Haiti","Honduras","Hungary","Iceland","India","Indonesia","Iran","Iraq","Ireland","Israel","Italy","Jamaica","Japan","Jordan","Kazakhstan","Kenya","Kiribati","Kosovo","Kuwait","Kyrgyzstan","Laos","Latvia","Lebanon","Lesotho","Liberia","Libya","Lithuania","Luxembourg","Macedonia","Madagascar","Malawi","Malaysia","Maldives","Mali","Malta","Marshall Islands","Mauritania","Mauritius","Mexico","Federated States of Micronesia","Moldova","Mongolia","Montenegro","Morocco","Mozambique","Myanmar","Namibia","Nauru","Nepal","Netherlands","New Zealand","Nicaragua","Niger","Nigeria","Norway","Oman","Pakistan","Palau","Panama","Papua New Guinea","Paraguay","Peoples Republic of China","Puerto Rico","Peru","Philippines","Poland","Portugal","Qatar","Romania","Russia","Rwanda","Saint Kitts and Nevis","Saint Lucia","Saint Vincent and the Grenadines","Samoa","San Marino","São Tomé and Príncipe","Saudi Arabia","Senegal","Serbia","Seychelles","Sierra Leone","Singapore","Slovakia","Slovenia","Solomon Islands","Somalia","South Africa","South Korea","South Sudan","Spain","Sri Lanka","Sudan","Suriname","Swaziland","Sweden","Switzerland","Syria","Tajikistan","Tanzania","Thailand","Togo","Tonga","Trinidad and Tobago","Tunisia","Turkey","Turkmenistan","Tuvalu","Uganda","Ukraine","United Arab Emirates","United Kingdom","United States","Uruguay","Uzbekistan","Vanuatu","Venezuela","Vietnam","Yemen","Zambia","Zimbabwe")
levels(analy_pop$appeal_yn.factor)=c("Yes","No")
#levels(analy_pop$outcomes_yn.factor)=c("Yes","No","Unclear")
levels(analy_pop$empirical.factor)=c("empirical","non-empirical")
levels(analy_pop$competing_interests_yn.factor)=c("Yes","No")
levels(analy_pop$competing_interests.factor)=c("Yes","No","Unclear")

analy_pop_f <- analy_pop %>% 
  mutate_all(as.character) %>% 
  select(record_id, redcap_event_name.factor, redcap_repeat_instrument.factor, redcap_repeat_instance, paper_type.factor, name, dv_name, year_pub, country_yn.factor, country.factor, competing_interests_yn.factor, competing_interests.factor, empirical.factor, year_study, appeal_yn.factor, comments, data_extraction_form_complete, country_income, country_region, target_substance, population_specific, population_specific_affect, study_design, data_sources, analysis)
