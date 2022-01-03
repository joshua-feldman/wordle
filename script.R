library(tidyverse)
# library(words) For dictionary of 5-letter words
library(parallel) # For parallel processing
library(dtplyr)

dict <- source("~/Desktop/wordle_dictionary.R")$value

df <- data.frame(word = dict) %>% arrange(word)
# df <- words %>% filter(word_length == 5) %>% select(-word_length)

df_wide <- df %>% 
  mutate(letter1 = str_split(word, "", simplify = TRUE)[,1]) %>% 
  mutate(letter2 = str_split(word, "", simplify = TRUE)[,2]) %>% 
  mutate(letter3 = str_split(word, "", simplify = TRUE)[,3]) %>% 
  mutate(letter4 = str_split(word, "", simplify = TRUE)[,4]) %>% 
  mutate(letter5 = str_split(word, "", simplify = TRUE)[,5])

calculate_problem_space <- function(x) {
  
  if(x[1] == x[2]) {print(paste0("Evaluating: ", x[1]))}
  
  answer_parsed <- strsplit(x[1], "")[[1]]
  guess_parsed <- strsplit(x[2], "")[[1]]
  
  # Returns a vector of columns that matches the game (e.g. green-green-grey-orange-grey)
  status <- ifelse(guess_parsed == answer_parsed, "green",
                   ifelse(guess_parsed %in% answer_parsed, "orange",
                          ifelse(!guess_parsed %in% answer_parsed, "grey", NA)))
  
  # # Use base R for filtering and conditionals to be more efficient
  df_wide <- df_wide[if(status[1] == "green") {guess_parsed[1] == df_wide$letter1}
                     else if (status[1] == "orange") {guess_parsed[1] == df_wide$letter2 | guess_parsed[1] == df_wide$letter3 | guess_parsed[1] == df_wide$letter4 | guess_parsed[1] == df_wide$letter5}
                     else {guess_parsed[1] != df_wide$letter1 & guess_parsed[1] != df_wide$letter2 & guess_parsed[1] != df_wide$letter3 & guess_parsed[1] != df_wide$letter4 & guess_parsed[1] != df_wide$letter5},]
  
  df_wide <- df_wide[if (status[2] == "green") {guess_parsed[2] == df_wide$letter2}
                     else if (status[2] == "orange") {guess_parsed[2] == df_wide$letter1 | guess_parsed[2] == df_wide$letter3 | guess_parsed[2] == df_wide$letter4 | guess_parsed[2] == df_wide$letter5}
                     else {guess_parsed[2] != df_wide$letter1 & guess_parsed[2] != df_wide$letter2 & guess_parsed[2] != df_wide$letter3 & guess_parsed[2] != df_wide$letter4 & guess_parsed[2] != df_wide$letter5},]
  
  df_wide <- df_wide[if(status[3] == "green") {guess_parsed[3] == df_wide$letter3}
                     else if(status[3] == "orange") {guess_parsed[3] == df_wide$letter1 | guess_parsed[3] == df_wide$letter2 | guess_parsed[3] == df_wide$letter4 | guess_parsed[3] == df_wide$letter5}
                     else {guess_parsed[3] != df_wide$letter1 & guess_parsed[3] != df_wide$letter2 & guess_parsed[3] != df_wide$letter3 & guess_parsed[3] != df_wide$letter4 & guess_parsed[3] != df_wide$letter5},]
  
  df_wide <- df_wide[if(status[4] == "green") {guess_parsed[4] == df_wide$letter4}
                     else if(status[4] == "orange") {guess_parsed[4] == df_wide$letter1 | guess_parsed[4] == df_wide$letter2 | guess_parsed[4] == df_wide$letter3 | guess_parsed[4] == df_wide$letter5}
                     else {guess_parsed[4] != df_wide$letter1 & guess_parsed[4] != df_wide$letter2 & guess_parsed[4] != df_wide$letter3 & guess_parsed[4] != df_wide$letter4 & guess_parsed[4] != df_wide$letter5},]
  
  df_wide <- df_wide[if(status[5] == "green") {guess_parsed[5] == df_wide$letter5}
                     else if(status[5] == "orange") {guess_parsed[5] == df_wide$letter1 | guess_parsed[5] == df_wide$letter2 | guess_parsed[5] == df_wide$letter3 | guess_parsed[5] == df_wide$letter4}
                     else {guess_parsed[5] != df_wide$letter1 & guess_parsed[5] != df_wide$letter2 & guess_parsed[5] != df_wide$letter3 & guess_parsed[5] != df_wide$letter4 & guess_parsed[5] != df_wide$letter5},]
  
  # Return the size of the updated problem space
  return(nrow(df_wide))
  
}

calculate_problem_space(c("mouse", "xylyl"))
calculate_problem_space(c("horse", "house"))

word_grid <- expand.grid(answer = df$word, guess = df$word)

# Run on smaller dataset to generate candidate guesses
word_grid_reduced <- word_grid %>%
  group_by(guess) %>%
  sample_n(100) %>%
  ungroup()

# # Parallel processing
nCores <- detectCores() - 1
cl <- makeCluster(nCores, outfile = "")
# clusterEvalQ(cl, {library(dplyr); library(magrittr)})
clusterExport(cl, varlist = c('df_wide'))
start <- Sys.time()
# word_grid_reduced$score <- parallel::mclapply(word_grid_reduced, 1, calculate_problem_space)
word_grid_reduced$score <- parallel::parApply(cl = cl, word_grid_reduced, 1, calculate_problem_space)
Sys.time() - start
# word_grid$score <- parallel::parApply(cl = cl, word_grid, 1, calculate_problem_space) # Loop through answers
stopCluster(cl)
beepr::beep(5)

# Calculate average problem space per word
word_scores <- word_grid_reduced %>% 
  group_by(guess) %>% 
  summarise(avg = mean(score),
            n = n(),
            sd = sd(score)) %>% 
  ungroup() %>% 
  mutate(se = sd / sqrt(n),
         upper_ci = avg + se * 2.575,
         lower_ci = avg - se * 2.575)

# Filter for guesses that are feasible candidates
candidates <- word_scores$guess[word_scores$lower_ci <= min(word_scores$upper_ci)]

word_grid_candidates <- word_grid %>% 
  filter(guess %in% candidates)

# Now just loop through candidate features
nCores <- detectCores() - 1
cl <- makeCluster(nCores, outfile = "")
clusterExport(cl, varlist = c('df_wide'))
start <- Sys.time()
word_grid_candidates$score <- parallel::parApply(cl = cl, word_grid_candidates, 1, calculate_problem_space)
Sys.time() - start
stopCluster(cl)
beepr::beep(5)

# Calculate final scores
final_word_scores <- word_grid_candidates %>% 
  group_by(guess) %>% 
  summarise(score = mean(score)) %>% 
  ungroup()

# Optimal word is the one with the smallest average problem space
final_word_scores$guess[final_word_scores$score == min(final_word_scores$score)]