
library(tidyverse)
library(here)
library(drake)

source(here("src", "functions.R"))

# read in files:
raw_data = read_csv(file_in("data/ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv")) 
lookup = read_csv(file = file_in("data/lookup_rd.csv"))
ref_lookup = read_csv(file = file_in("data/references_lookup.csv")) %>% select(record_id, Reference)

# processing; 
lookup_a = lookup %>% select(-impact_var)
lookup_b = lookup %>% rename(out_op = variable_merge) %>% 
    unite(out_specific, c(code, code_name), remove = FALSE) 
clean_data = raw_data %>% get_clean_data()
data_long = get_data_long(clean_data)
data_long_code = get_data_long_code(data_long, lookup_a)
data_long_w2 = get_data_long_w2(data_long_code)

policy_types = get_policy_types(data_long_w2)
specifics_policy_f = get_specifics_policy_f(data_long_w2, policy_types)

other_out_onlyb = get_other_out_onlyb(data_long_w2)
specifics_out_long = get_specifics_out_long(data_long_w2)
other_out_sub = get_other_out_sub(specifics_out_long)
other_out_onlyc = get_other_out_onlyc(other_out_sub, other_out_onlyb)
final_mod_cor_policy = get_final_mod_cor_policy(specifics_policy_f, 
                                                data_long_w2, 
                                                specifics_out_long, 
                                                lookup_b, 
                                                other_out_onlyc)
analy_pop_f = get_analy_pop_f(data_long_w2)
join_pop_mod_cor = left_join(final_mod_cor_policy, 
                             analy_pop_f, 
                             by = c("record_id" = "record_id"))
join_with_references = left_join(join_pop_mod_cor, 
                                 ref_lookup)
