
library(tidyverse)
library(drake)
library(here)

source(here("src", "functions.R"))

plan <- drake_plan(
    raw_data = read_csv(file_in("data/ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv")), 
    lookup = read_csv(file = file_in("data/lookup_rd.csv")), 
    lookup_a = lookup %>% select(-impact_var), 
    lookup_b = lookup %>% rename(out_op = variable_merge) %>% 
        unite(out_specific, c(code, code_name), remove = FALSE), 
    clean_data = raw_data %>% get_clean_data(),
    clean_data_file = write_csv(clean_data, file_out("dst/clean_data.csv")),
    data_long = get_data_long(clean_data),
    data_long_code = get_data_long_code(data_long, lookup_a), 
    data_long_w2 = get_data_long_w2(data_long_code)
)

make(plan)
vis_drake_graph(plan)
vis_drake_graph(plan, file = here("dst", "drake_plan.html"))
