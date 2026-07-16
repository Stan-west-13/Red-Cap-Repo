library(readxl)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

d <- read.csv("data/RetrievalBasedWordLe-NEWRetrievalBasedWor_DATA_2026-07-15_1824.csv")
rules_path <- "data/Formatting Sheet.xlsx"


long_by_excel_rules <- function(data, rules_file, sheet = 1) {
  
  # Read rule table
  rules <- readxl::read_excel(rules_file, sheet = sheet)
  
  # Parse name_components into list-column
  rules <- rules %>%
    mutate(name_components = str_split(name_components, ","))
  
  # Apply each rule and bind results
  results <- map_dfr(seq_len(nrow(rules)), function(i) {
    
    rule <- rules[i, ]
    
    tidyr::pivot_longer(
      data,
      cols = tidyselect::matches(rule$pattern),
      names_to = unlist(rule$name_components),
      names_pattern = rule$regex,
      values_to = rule$value_name
    )
  })
  
  return(results)
}
x <- long_by_excel_rules(d, rules_path)

View(x[,c("item","response","value_item","value_responsee")])
