library(tidyverse)
library(readxl)
library(reshape2)

##### Create base responses dataframe from raw data #####

#Specify na = "N/A" to change "N/A" strings to NA
#trim leading and trailing whitespace in cells
#Skip first "question" header column
df1 <- read_excel(
  here::here("data/raw_data/Tummons_Questionaire.xlsx"),
  na = "N/A",
  trim_ws = TRUE,
  skip = 1,
  col_types = "text"
) %>%
  #prefix each column with which question it pertains to
  rename_at(3:4, funs(paste0("usage.", .))) %>%
  rename_at(5:12, funs(paste0("resources.", .))) %>%
  rename_at(13:22, funs(paste0("consider_epd.", .))) %>%
  rename_at(23:31, funs(paste0("barrier_epd.", .))) %>%
  rename_at(32:37, funs(paste0("important_factors.", .))) %>%
  rename_at(38:44, funs(paste0("decision_makers.", .))) %>%
  rename_at(45:52, funs(paste0("learn.", .))) %>%
  #rename columns in R friendly way (no spaces etc)
  janitor::clean_names() %>%
  #remove completely empty rows and columns
  #(can sometimes result from Exel formatting)
  janitor::remove_empty() %>%
  #remove partial online entries since I got full dataset
  filter(sale_barn != "Online")

responses <- read_csv(
  here::here("data/raw_data/181203_online_data.csv"),
  trim_ws = TRUE,
  skip = 3,
  col_names = FALSE,
  col_types = cols(.default = "c"),
  na = c("N/A", "na", "n/a", "", " ", "NA")
) %>%
  select(9, 66:67, 18:65, 71:73) %>%
  mutate(sale_barn = "Online") %>%
  select(sale_barn, everything()) %>%
  setNames(object = ., colnames(df1)) %>%
  bind_rows(df1) %>%
  #janitor names things weird sometimes
  rename(important_factors_epds = important_factors_ep_ds) %>%
  #if they provided a size range for size, set to NA
  mutate(size_of_operation = if_else(str_detect(size_of_operation,
                                                "/ | -"),
                                     "0",
                                     size_of_operation)) %>%
  #remove words from size_of_operation
  mutate(size_of_operation = str_remove(size_of_operation, "[[:alpha:]]+")) %>%
  #make age and size of operation numeric
  mutate_at(.vars = vars(usage_percent_epd:age), as.numeric) %>%
  mutate(sex = if_else(sex == "1", "M", sex),
         sex = if_else(sex == "2", "F", sex)) %>%
  #Capitalize sex (all M or F)
  mutate(sex = R.utils::capitalize(sex)) %>%
  #Create a column for online vs. in-person
  mutate(medium = if_else(sale_barn == "Online" |
                            sale_barn == "Pilot",
                          "online",
                          "in person")) %>%
  #capitalize medium
  mutate(medium = str_to_title(medium)) %>% 
  #make sale barn, #, sex, medium, factors
  mutate_at(c("sale_barn",
              "survey_number",
              "sex",
              "medium"),
            as.factor) %>%
  #If one of % EPD vs visual is empty, fill in
  mutate(usage_percent_epd = if_else(
    is.na(usage_percent_epd) & !is.na(usage_percent_visual),
    100 - usage_percent_visual,
    usage_percent_epd
  )) %>%
  mutate(
    usage_percent_visual = if_else(
      is.na(usage_percent_visual) & !is.na(usage_percent_epd),
      100 - usage_percent_epd,
      usage_percent_visual
    )
  ) %>%
  #Some people gave EPD + visual percentages
  #that don't add up to 100: standardize
  mutate(epd_usage_stand =
           usage_percent_epd / (usage_percent_epd + usage_percent_visual)) %>%
  #Responses with 7s when only 6 options: remove
  mutate(learn_vet = replace(learn_vet, 7, NA),
         learn_vet = replace(learn_vet, 0, NA)) %>%
  mutate_at(vars(starts_with("decision_")),
            ~ (na_if(., 7))) %>%
  #Remove surveys where age is < 18 years
  #Remove surveys where size of operation = 0
  filter(age > 17,
         size_of_operation != 0)

##### verbose_variable #####

verbose_variable <- tribble(
  ~variable, ~verbose_variable,
  "resources_epds", "EPDs", 
  "resources_genomic_tests", "Genomic tests",
  "resources_visual_inspections", "Visual inspection",
  "resources_breed_staff", "Breed association staff",
  "resources_catalogs", "Breed/semen catalogs",
  "resources_distributors", "Semen distributors",
  "resources_trusted_breeder", "Trusted breeder",
  "resources_other_producers", "Other producers",
  "consider_epd_ced", "CED",
  "consider_epd_bw", "BW",
  "consider_epd_ww", "WW",
  "consider_epd_yw", "YW",
  "consider_epd_milk", "MILK",
  "consider_epd_cem", "CEM",
  "consider_epd_rea", "REA",
  "consider_epd_marb", "MARB",
  "consider_epd_breed_spec", "Breed-specific value indexes",
  "consider_epd_acc", "ACC",
  "barrier_epd_difficult_to_read", "Difficult to read",
  "barrier_epd_inconsistent", "Inconsistency between breed EPDs",
  "barrier_epd_difference_between", "Difficult to understand difference from baseline",
  "barrier_epd_overlap", "Too much overlap in composite data",
  "barrier_epd_too_many_bulls", "Too many bull EPDs",
  "barrier_epd_not_available", "Not available for the bulls I purchase",
  "barrier_epd_have_not_worked", "Have not worked in my situation",
  "barrier_epd_not_accurate", "Don't accurately reflect genetic merit",
  "barrier_epd_dont_reflect_factors", "Don't reflect all important factors",
  "important_factors_epds", "EPDs",
  "important_factors_visual", "Visual inspection",
  "important_factors_price", "Price",
  "important_factors_previous_use", "Previous use of specific animal/genetic line",
  "important_factors_breeder_recommendation", "Purebred breeder recommendation",
  "important_factors_other_producers_1", "Other producers",
  "decision_makers_me", "Me",
  "decision_makers_g_parents", "Grandparents",
  "decision_makers_parents", "Parents",
  "decision_makers_spouse", "Spouse",
  "decision_makers_siblings", "Siblings",
  "decision_makers_son_daughter", "Sons/daughters",
  "decision_makers_farm_manager", "Farm manager",
  "learn_pubs_magazines", "Trade publications/magazines",
  "learn_breed_assoc", "Breed associations",
  "learn_extension", "Local extension agent",
  "learn_ag_teacher", "Local ag teacher",
  "learn_online", "Online resources",
  "learn_vet", "Veterinarian",
  "learn_semen_salesman", "Semen salesmen",
  "learn_other_producers_2", "Other producers")

##### verbose_response #####

verbose_response <- tribble(
  ~response,                                                                        ~verbose_question,               ~verbose_response,
  1L,         "How do you learn about new breeding information and new industry technologies?",                         "Never",
  1L,      "How important are the following factors in choosing breeding stock for your farm?",                 "Not important",
  1L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                         "Never",
  1L,              "How often do you use the following items when selecting breeding animals?",                         "Never",
  1L,                              "Which of the following would prevent you from using EPDs?",                 "Not a barrier",
  1L,                                         "Who makes the breeding decisions on your farm?",           "Never/doesn't apply",
  2L,         "How do you learn about new breeding information and new industry technologies?",                        "Rarely",
  2L,      "How important are the following factors in choosing breeding stock for your farm?",          "Of little importance",
  2L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                        "Rarely",
  2L,              "How often do you use the following items when selecting breeding animals?",                        "Rarely",
  2L,                              "Which of the following would prevent you from using EPDs?",                 "Small barrier",
  2L,                                         "Who makes the breeding decisions on your farm?",                        "Rarely",
  3L,         "How do you learn about new breeding information and new industry technologies?",                  "Occasionally",
  3L,      "How important are the following factors in choosing breeding stock for your farm?",            "Somewhat important",
  3L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                  "Occasionally",
  3L,              "How often do you use the following items when selecting breeding animals?",                  "Occasionally",
  3L,                              "Which of the following would prevent you from using EPDs?",              "Moderate barrier",
  3L,                                         "Who makes the breeding decisions on your farm?",                  "Occasionally",
  4L,         "How do you learn about new breeding information and new industry technologies?",                    "Frequently",
  4L,      "How important are the following factors in choosing breeding stock for your farm?",                     "Important",
  4L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                    "Frequently",
  4L,              "How often do you use the following items when selecting breeding animals?",                    "Frequently",
  4L,                              "Which of the following would prevent you from using EPDs?",                 "Large barrier",
  4L,                                         "Who makes the breeding decisions on your farm?",                    "Frequently",
  5L,         "How do you learn about new breeding information and new industry technologies?",                         "Often",
  5L,      "How important are the following factors in choosing breeding stock for your farm?",                "Very important",
  5L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                         "Often",
  5L,              "How often do you use the following items when selecting breeding animals?",                         "Often",
  5L,                              "Which of the following would prevent you from using EPDs?",           "Prohibitive barrier",
  5L,                                         "Who makes the breeding decisions on your farm?",                         "Often",
  6L,         "How do you learn about new breeding information and new industry technologies?",                        "Always",
  6L, "How often do you use consider the following EPD indexes when selecting breeding stock?",                        "Always",
  6L,              "How often do you use the following items when selecting breeding animals?",                        "Always",
  6L,                                         "Who makes the breeding decisions on your farm?",                        "Always",
  7L, "How often do you use consider the following EPD indexes when selecting breeding stock?", "I do not know what this means"
)

##### responses_long #####

responses_long <- responses %>%
  melt(
    id = c(
      "sale_barn",
      "medium",
      "survey_number",
      "size_of_operation",
      "age",
      "sex",
      "usage_percent_epd",
      "usage_percent_visual",
      "epd_usage_stand"
    ),
    na.rm = FALSE
  ) %>%
  mutate(variable = as.character(variable)) %>%
  rename(response = value) %>% 
  mutate(verbose_question = if_else(str_detect(variable, "resources_"),
                                    "How often do you use the following items when selecting breeding animals?",
                                    as.character(NA)),
         verbose_question = if_else(str_detect(variable, "consider_"),
                                    "How often do you use consider the following EPD indexes when selecting breeding stock?",
                                    verbose_question),
         verbose_question = if_else(str_detect(variable, "barrier_"),
                                    "Which of the following would prevent you from using EPDs?",
                                    verbose_question),
         verbose_question = if_else(str_detect(variable, "important_factors_"),
                                    "How important are the following factors in choosing breeding stock for your farm?",
                                    verbose_question),
         verbose_question = if_else(str_detect(variable, "decision_makers_"),
                                    "Who makes the breeding decisions on your farm?",
                                    verbose_question),
         verbose_question = if_else(str_detect(variable, "learn_"),
                                    "How do you learn about new breeding information and new industry technologies?",
                                    verbose_question)
         
         
  ) %>% 
  left_join(verbose_variable) %>% 
  left_join(verbose_response)

