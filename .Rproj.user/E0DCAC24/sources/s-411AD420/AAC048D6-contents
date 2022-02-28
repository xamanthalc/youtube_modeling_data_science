library(tidyverse)

#Fitting to Analyse ---- 

final_lm_tune_workflow #get it after running Tuned.R 
#fitting it into the training data set 
final_lm_splines <- fit(final_lm_tune_workflow, youtube_training)
#saving it
#save(final_lm_splines, file = "data/final_lm_splines.rda")

#Most relevant factor ----

#check coefficients 
n <- tidy(final_lm_splines)
n 

#data wrangling to find the factor with the greatest 
#absolute estimate 
n %>%
  #turning all estimates into their absolute value
  mutate(estimate = abs(estimate)) %>%
  #getting rid off na values
  filter(!is.na(estimate)) %>% 
  #ordering from greatest to lowest
  arrange(desc(estimate))

#true value 
n %>%
  filter(term == "comments_disabled_TRUE.")

#comments were provided in the Rmd 

#Optimization categoryId----
#best categories
n %>%
  filter(stringr::str_starts(term, "category")) %>%
  filter(stringr::str_length(term) < 15) %>%
  arrange(desc(estimate)) %>%
  head(3)

#worst categories 
n %>%
  #getting only the coefficients of category factors
  filter(stringr::str_starts(term, "category")) %>%
  #not taking into account interactions of category with other variables 
  filter(stringr::str_length(term) < 15) %>%
  #ordering by greatest to lowest
  arrange(desc(estimate)) %>%
  #keeping last three observations 
  tail(3)

#comments were provided in the Rmd

#FINISH SECTION

