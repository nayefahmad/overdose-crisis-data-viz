library(drake)


plan <- drake_plan(
    raw_data = read_csv(file_in("data/ScopingReviewOfOpioi-FinalResults_DATA_2020-07-06_1942.csv")), 
    lookup = read_csv(file = file_in("data/lookup_rd.csv")), 
    clean_data = raw_data %>% mutate(year_pub = 
                                        ifelse(year_pub %in% c("Unclear", "unclear"), NA, year_pub))
)

vis_drake_graph(plan)
