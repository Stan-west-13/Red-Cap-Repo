library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)


## Word Library ######################
phon_acc <- read_xlsx("data/WordLevelDatabase_Novel_Noun_R21_SRCLD_03_23_2026 1.xlsx",
                      sheet = "Phonetic_Accuracy") %>%
  select("Word", "Learning Condition", "Time", "Taught/Gen", "Set", "Variable_Name_Redcap" = "...10") %>%
  mutate(measure = "phonetic_accuracy") %>% 
  unique()
recall <- read_xlsx("data/WordLevelDatabase_Novel_Noun_R21_SRCLD_03_23_2026 1.xlsx",
                    sheet = "Word_Form") %>%
  select("Word", "Learning Condition", "Time", "Taught/Gen", "Set","Variable_Name_Redcap" ) %>%
  mutate(measure = "recall_accuracy") %>%
  unique()

lib <- rbind(phon_acc, recall) %>% unique()
##################################

d <- read.csv("data/RetrievalBasedWordLe_DATA_2026-08-10_1159.csv")
rules_path <- "data/Formatting Sheet.xlsx"

d_test <- d %>%
  select("record_id", "age_awl","examiners_id_awl", starts_with(c("test","total","gen"))) %>%
  filter(!record_id == 1) %>%
  pivot_longer(cols = starts_with(c("test_","total","gen")),
               names_to = "Variable_Name_Redcap",
               values_to = "value") %>%
  left_join(lib) %>%
  arrange(record_id, Word)

long_by_excel_rules <- function(data, rules_file, sheet = 1) {
  
  # Read rule table
  rules <- readxl::read_excel(rules_file, sheet = sheet)
  

   results <-  tidyr::pivot_longer(
      data,
      cols = starts_with(rules$pattern),
      names_to = unlist(str_split(rules$name_components, ", ")),
      names_pattern = rules$regex,
      values_to = rules$value_name
    )
  
  
  return(results)
}


parse_rest <- function(df, rest_col = "rest") {
  
  df %>%
    # 1. Split into tokens
    mutate(tokens = str_split(.data[[rest_col]], "_")) %>%
    
    # 2. Classify tokens
    mutate(
      version_tokens = map(tokens, ~ str_subset(.x, "^v\\d+$")),
      type_tokens = map(tokens, ~ str_subset(.x, "^(nwl|awl)$")),
      subtype_tokens = map(tokens, ~ str_subset(.x, "^[a-z]$")),
      cb_tokens = map(tokens, ~ str_subset(.x, "^c\\d$")),
      
      # "other" = tokens not in any category above
      other_tokens   = pmap(
        list(tokens, version_tokens, type_tokens, subtype_tokens, cb_tokens),
        function(toks, v, ty, st, cd) {
          setdiff(toks, c(v, ty, st, cd))
        }
      )
    ) %>%
    
    # 3. Collapse categories into useful columns
    mutate(
      type = map_chr(type_tokens, ~ first(.x) %||% NA_character_),
      subtype = map_chr(subtype_tokens, ~ first(.x) %||% NA_character_),
      versions = map_chr(version_tokens, ~ paste(.x, collapse = "_")),
      counterbalance = map_chr(cb_tokens, ~ paste(.x, collapse = "_")),
      other = map_chr(other_tokens, ~ paste(.x, collapse = "_"))
    ) %>%
    
    # 4. Drop intermediate list columns if you want
    select(-tokens, -version_tokens, -type_tokens, -subtype_tokens, -cb_tokens, -other_tokens)
}



x <- long_by_excel_rules(d_test, rules_path, sheet = 1)
z <- parse_rest(x,"extra")
write.csv(x, "data/Long-formatted_test.csv")

write.csv(z, "data/Long-formatted_extraparsed.csv")


