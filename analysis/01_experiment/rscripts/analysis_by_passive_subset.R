# load packages
library(stringi) # for string manipulation
library(purrr) # for map()
library(tidyverse) # for data wrangling and visualization
source("helpers.R")

# set working directory
this.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

### DATA WRANGLING

# prepare data for manual annotation

# load data
df = read_csv("../data/example-trials.csv")

# wrangle data
df = df %>%
  filter(is.na(slide_number_in_experiment), # filter out bot checks
         trial_type == "target") %>% # filter out fillers
  mutate(
    response = str_remove_all(response, "[\\[\\]\\']"),
    # remove square brackets
    response = str_split(response, ", "),
    # split `response`
    typed_sentence = map(.x = response,
                         .f = ~ .x[[1]]),
    # first response to `typed_sentence`
    typed_sentence = str_remove(typed_sentence, "\\."),
    # remove final period from sentence
    typed_sentence = str_remove(typed_sentence, "\"\""),
    # remove quotation marks from typed_sentece
    sentence = str_remove(sentence, "\\."),
    # remove final period from sentence
    seen_sentence = map(.x = response,
                        .f = ~ .x[[2]]),
    # second response to `seen_sentence`
    picture_description = map(.x = response,
                              .f = ~ .x[[3]]),
    # third response to `picture_description`
    seen_picture = map(.x = response,
                       .f = ~ .x[[4]]),
    # fourth response to `seen_picture`
    correctly_typed = ifelse(tolower(sentence)  == tolower(typed_sentence),
                             1, 0)
  ) %>% # check if participants typed sentence correctly (for exclusion purposes)
  select(-c(slide_number_in_experiment,
            stim,
            response)) %>%
  select(
    workerid,
    exp_condition = Answer.condition,
    sent_condition = condition,
    sentence,
    typed_sentence,
    correctly_typed,
    seen_sentence,
    picture,
    picture_description,
    seen_picture,
    everything()
  ) %>%
  mutate_if(.predicate = "is.list",
            .funs = "unlist")

# exclude participants who typed the priming sentence incorrectly in more than 90% of the cases after manual correction of sentences which were coded as incorrectly typed because of minor deviations (information taken from analysis_by_passive.R)
part_to_exclude = c(21, 61, 74, 82)

`%notin%` = Negate(`%in%`)

df = df %>%
  filter(workerid %notin% part_to_exclude)

# check if there are still participants who typed some sentences incorrectly
length(which(df$correctly_typed == 0))
rows_deviations = which(df$correctly_typed == 0)

# check which participants show minor deviations in the typing of the priming sentence
df_deviations = df %>%
  group_by(workerid) %>%
  summarize(prop_correct = mean(correctly_typed)) %>%
  mutate(part_deviations = ifelse(prop_correct < 1, 1, 0))

# all participants show minor deviations

# data frame with the deviations
df_part_deviations = df[rows_deviations, ]
