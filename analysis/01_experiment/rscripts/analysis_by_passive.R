# load packages
#library(stringi) # for string manipulation
#library(purrr) # for map()
library(lme4) # for glmer()
library(brms) # for bayesian models
library(tidyverse) # for data wrangling and visualization
source("helpers.R")

# set default theme
theme_set(theme_classic() + #set the theme
            theme(text = element_text(size = 12))) #set the default text size

# set working directory
this.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

### DATA WRANGLING

# prepare data for manual annotation

# load data
#df = read_csv("../data/example-trials.csv")

# wrangle data
#df = df %>%
#  filter(is.na(slide_number_in_experiment), # filter out bot checks
#         trial_type == "target") %>% # filter out fillers
# mutate(
#    response = str_remove_all(response, "[\\[\\]\\']"),
# remove square brackets
#    response = str_split(response, ", "),
# split `response`
#    typed_sentence = map(.x = response,
#                         .f = ~ .x[[1]]),
# first response to `typed_sentence`
#    typed_sentence = str_remove(typed_sentence, "\\."),
# remove final period from sentence
#    typed_sentence = str_remove(typed_sentence, "\"\""),
# remove quotation marks from typed_sentece
#    sentence = str_remove(sentence, "\\."),
# remove final period from sentence
#    seen_sentence = map(.x = response,
#                       .f = ~ .x[[2]]),
# second response to `seen_sentence`
#    picture_description = map(.x = response,
#                              .f = ~ .x[[3]]),
# third response to `picture_description`
#    seen_picture = map(.x = response,
#                       .f = ~ .x[[4]]),
# fourth response to `seen_picture`
#    correctly_typed = ifelse(tolower(sentence)  == tolower(typed_sentence),
#                             1, 0)
#    ) %>% # check if participants typed sentence correctly (for exclusion purposes)
#  select(-c(slide_number_in_experiment,
#            stim,
#            response)) %>%
#  select(
#    workerid,
#    exp_condition = Answer.condition,
#    sent_condition = condition,
#    sentence,
#    typed_sentence,
#    correctly_typed,
#    seen_sentence,
#    picture,
#    picture_description,
#    seen_picture,
#    everything()
#  ) %>%
#  mutate_if(.predicate = "is.list",
#            .funs = "unlist")

# check if participants typed some sentences incorrectly
#length(which(df$correctly_typed == 0))
#which(df$correctly_typed == 0)

# write df to csv
#write.csv(df, "../data/df_manual2.csv")

# after manual annotation, load data
df = read_csv("../data/df_manual.csv")

#check for exclusion of participants
df_incorrect = df %>%
  group_by(workerid) %>%
  summarize(prop_correct = mean(correctly_typed)) %>%
  mutate(exclude = ifelse(prop_correct < 0.9, 1, 0))

part_to_exclude = df_incorrect$workerid[df_incorrect$exclude == 1]

# wrangle data
`%notin%` = Negate(`%in%`)

df = df %>%
  filter(workerid %notin% part_to_exclude) %>%
  mutate(
    by_passive_YesNo = case_when(
      bpassive_active_other == "BP" ~ 1,
      bpassive_active_other == "A" ~ 0,
      TRUE ~ NA_real_
    ),
    # create dependent variable (described picture with by-passive?)
    prime_type = case_when(
      sent_condition %in% c("passive",
                            "by-locative",
                            "non-by-locative") ~ "non-active",
      sent_condition == "active" ~ "active"
    ) # prime_type (collapse passive, by-locative, and non-by-locative as non-active)
  ) %>%
  rename(condition = exp_condition)

### PROPORTIONS

# proportion of by-passive, active, and other
prop.table(table(df$bpassive_active_other))

# proportion of passives
df %>%
  summarize(prop = mean(by_passive_YesNo %>%
                          as.character() %>%
                          as.numeric(),
                        na.rm = T))

# proportion of passives by `prime_type`
prop_by_prime_type = df %>%
  group_by(prime_type) %>%
  summarize(prop = mean(by_passive_YesNo,
                        na.rm = T))

# proportion of passives by condition
prop_by_condition = df %>%
  group_by(condition) %>%
  summarize(prop = mean(by_passive_YesNo,
                        na.rm = T))

# proportion of passives by `prime_type` and `condition`
prop_by_prime_type_condition = df %>%
  group_by(prime_type, condition) %>%
  summarize(prop = mean(by_passive_YesNo,
                        na.rm = T))

# difference in proportion of passives in `prime_type` grouped by `condition`
diff_prop = prop_by_prime_type_condition %>%
  group_by(condition) %>%
  summarize(diff_prop = prop[prime_type == "non-active"] -
              prop[prime_type == "active"])

### VISUALIZATION

df.sum = df %>%
  group_by(prime_type,
           condition) %>%
  summarize(
    prop_by_passive = mean(by_passive_YesNo,
                           na.rm = T),
    ci_low = ci.low(by_passive_YesNo,
                    na.rm = T),
    ci_high = ci.high(by_passive_YesNo,
                      na.rm = T)
  ) %>%
  ungroup() %>%
  mutate(
    y_min = prop_by_passive - ci_low,
    y_max = prop_by_passive + ci_high,
    prime_type = case_when(
      prime_type == "active" ~ "active",
      condition == "passive" ~ "passive",
      condition == "by-locative" ~ "by-loc",
      TRUE  ~ "non-by-loc"
    )
  )

df.sum$prime_type = factor(df.sum$prime_type,
                           levels = c("passive",
                                      "by-loc",
                                      "non-by-loc",
                                      "active"))

df.sum$condition = factor(df.sum$condition,
                          levels = c("passive",
                                     "by-locative",
                                     "non-by-locative"))

df.sum_subj = df %>%
  group_by(workerid,
           prime_type,
           condition) %>%
  summarize(prop_by_passive = mean(by_passive_YesNo,
                                   na.rm = T)) %>%
  ungroup() %>%
  drop_na(prop_by_passive) %>%
  mutate(
    prime_type = case_when(
      prime_type == "active" ~ "active",
      condition == "passive" ~ "passive",
      condition == "by-locative" ~ "by-loc",
      TRUE  ~ "non-by-loc"
    )
  )

df.sum_subj$prime_type = factor(df.sum_subj$prime_type,
                                levels = c("passive",
                                           "by-loc",
                                           "non-by-loc",
                                           "active"))

df.sum_subj$condition = factor(df.sum_subj$condition,
                               levels = c("passive",
                                          "by-locative",
                                          "non-by-locative"))

p = df.sum  %>%
  ggplot(aes(x = prime_type,
             y = prop_by_passive,
             fill = prime_type)) +
  geom_bar(
    position = position_dodge(),
    stat = "identity",
    color = "black",
    fill = c(rep(c(
      "#E41A1C", "#377EB8"
    ),
    3))
  ) +
  geom_dotplot(
    data = df.sum_subj,
    aes(x = prime_type,
        y = prop_by_passive),
    binaxis = 'y',
    stackdir = 'center',
    binwidth = 1 / 30,
    dotsize = 0.8,
    fill = "black",
    alpha = 0.5
  ) +
  geom_errorbar(aes(ymin = y_min,
                    ymax = y_max),
                width = 0.25) +
  facet_grid(cols = vars(condition),
             scale = "free_x") +
  labs(x = "Prime type",
       y = "Proportion of by-passive responses") +
  theme(legend.position = "none")

p

ggsave(
  filename = "../graphs/by_passive_plot.pdf",
  plot = p,
  width = 5,
  height = 3
)

### DATA ANALYSIS

# transform variables to factors
df$prime_type = factor(df$prime_type,
                       levels = c("non-active",
                                  "active"))
contrasts(df$prime_type) = contr.sum(2)

df$condition = factor(df$condition,
                      levels = c("passive",
                                 "by-locative",
                                 "non-by-locative"))
contrasts(df$condition) = contr.sum(3)

df$by_passive_YesNo = factor(df$by_passive_YesNo)

df = df %>%
  mutate_at(c("workerid", "picture"),
            as.character) %>%
  mutate_at(c("workerid", "picture"),
            as.factor)

#1. Maximal model analyses

# maximal model
fit_a = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 + prime_type || workerid) +
    (1 + condition || picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit_a)

# fit bayesian maximal model
fit.brm = brm(
  formula = by_passive_YesNo ~ prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  data = df,
  family = bernoulli(link = "logit"),
  iter = 4000,
  file = "brm",
  seed = 1
)
summary(fit.brm)

## FORWARD MODEL COMPARISONS

#model 1
fit1 = glmer(
  by_passive_YesNo ~
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit1)
#AIC 1549

#model 2
fit2 = glmer(
  by_passive_YesNo ~ prime_type +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit2)
#AIC 1551

# model 3
fit3 = glmer(
  by_passive_YesNo ~ prime_type + condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit3)
#AIC 1551

# model 4
fit4 = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit4)
# AIC 1554

anova(fit1, fit2)[2, 2] - anova(fit1, fit2)[1, 2]
anova(fit2, fit3)[2, 2] - anova(fit2, fit3)[1, 2]
anova(fit3, fit4)[2, 2] - anova(fit3, fit4)[1, 2]

## SIMPLE EFFECTS ANALYSIS
# dummy code `condition`
contrasts(df$condition) = NULL
df$condition = relevel(df$condition, "non-by-locative")

# `prime_type` is sum coded as above

fit_s_eff = glmer(
  by_passive_YesNo ~ prime_type * condition - prime_type +
    (1 + prime_type || workerid) +
    (1 + condition || picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit_s_eff)

## PAIRWISES

# pass-bloc
pass_bloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "non-by-locative")
pass_bloc$condition = factor(pass_bloc$condition,
                             levels = c("passive",
                                        "by-locative"))
contrasts(pass_bloc$condition) = contr.sum(2)
fit_pass_bloc = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_bloc
)
summary(fit_pass_bloc)

# pass-nloc
pass_nloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "by-locative")
pass_nloc$condition = factor(pass_nloc$condition,
                             levels = c("passive",
                                        "non-by-locative"))
contrasts(pass_nloc$condition) = contr.sum(2)
fit_pass_nloc = glmer(
  by_passive_YesNo ~
    prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_nloc
)
summary(fit_pass_nloc)

#bloc-nloc
bloc_nloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "passive")
bloc_nloc$condition = factor(bloc_nloc$condition,
                             levels = c("by-locative",
                                        "non-by-locative"))
contrasts(bloc_nloc$condition) = contr.sum(2)
fit_bloc_nloc = glmer(
  by_passive_YesNo ~
    prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_nloc
)
summary(fit_bloc_nloc)

## BAYES FACTOR

#pass
pass_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "passive")

fit0_pass_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_BF
)

fit1_pass_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_BF
)

pass_BIC0 = anova(fit0_pass_BF, fit1_pass_BF)$BIC[1]
pass_BIC1 = anova(fit0_pass_BF, fit1_pass_BF)$BIC[2]
pass_deltaBIC10 = pass_BIC1 - pass_BIC0
pass_BF01 = exp(1) ^ (pass_deltaBIC10 / 2)
pass_BF10 = 1 / pass_BF01
pass_BF10

# bloc
bloc_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "by-locative")

fit0_bloc_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_BF
)

fit1_bloc_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_BF
)

bloc_BIC0 = anova(fit0_bloc_BF, fit1_bloc_BF)$BIC[1]
bloc_BIC1 = anova(fit0_bloc_BF, fit1_bloc_BF)$BIC[2]
bloc_deltaBIC10 = bloc_BIC1 - bloc_BIC0
bloc_BF01 = exp(1) ^ (bloc_deltaBIC10 / 2)
bloc_BF10 = 1 / bloc_BF01
bloc_BF10

# nloc
nloc_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "non-by-locative")

fit0_nloc_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = nloc_BF
)

fit1_nloc_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = nloc_BF
)

nloc_BIC0 = anova(fit0_nloc_BF, fit1_nloc_BF)$BIC[1]
nloc_BIC1 = anova(fit0_nloc_BF, fit1_nloc_BF)$BIC[2]
nloc_deltaBIC10 = nloc_BIC1 - nloc_BIC0
nloc_BF01 = exp(1) ^ (nloc_deltaBIC10 / 2)
nloc_BF10 = 1 / nloc_BF01
nloc_BF10

#2. Analyses with model with simpler random effects structure

# simple model
fit = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit)

## FORWARD MODEL COMPARISONS

#model 1
fit1 = glmer(
  by_passive_YesNo ~
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit1)

#model 2
fit2 = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit2)

# model 3
fit3 = glmer(
  by_passive_YesNo ~ prime_type + condition +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit3)

# model 4
fit4 = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit4)

anova(fit1, fit2)[2, 2] - anova(fit1, fit2)[1, 2]
anova(fit2, fit3)[2, 2] - anova(fit2, fit3)[1, 2]
anova(fit3, fit4)[2, 2] - anova(fit3, fit4)[1, 2]

## PAIRWISES

# pass-bloc
pass_bloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "non-by-locative")
pass_bloc$condition = factor(pass_bloc$condition,
                             levels = c("passive",
                                        "by-locative"))
contrasts(pass_bloc$condition) = contr.sum(2)
fit_pass_bloc = glmer(
  by_passive_YesNo ~ prime_type * condition +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_bloc
)
summary(fit_pass_bloc)

# pass-nloc
pass_nloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "by-locative")
pass_nloc$condition = factor(pass_nloc$condition,
                             levels = c("passive",
                                        "non-by-locative"))
contrasts(pass_nloc$condition) = contr.sum(2)
fit_pass_nloc = glmer(
  by_passive_YesNo ~
    prime_type * condition +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_nloc
)
summary(fit_pass_nloc)

#bloc-nloc
bloc_nloc = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition != "passive")
bloc_nloc$condition = factor(bloc_nloc$condition,
                             levels = c("by-locative",
                                        "non-by-locative"))
contrasts(bloc_nloc$condition) = contr.sum(2)
fit_bloc_nloc = glmer(
  by_passive_YesNo ~
    prime_type * condition +
    (1 | workerid) +
    (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_nloc
)
summary(fit_bloc_nloc)

## BAYES FACTOR

#pass
pass_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "passive")

fit0_pass_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_BF
)

fit1_pass_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_BF
)

pass_BIC0 = anova(fit0_pass_BF, fit1_pass_BF)$BIC[1]
pass_BIC1 = anova(fit0_pass_BF, fit1_pass_BF)$BIC[2]
pass_deltaBIC10 = pass_BIC1 - pass_BIC0
pass_BF01 = exp(1) ^ (pass_deltaBIC10 / 2)
pass_BF10 = 1 / pass_BF01
pass_BF10

# bloc
bloc_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "by-locative")

fit0_bloc_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_BF
)

fit1_bloc_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_BF
)

bloc_BIC0 = anova(fit0_bloc_BF, fit1_bloc_BF)$BIC[1]
bloc_BIC1 = anova(fit0_bloc_BF, fit1_bloc_BF)$BIC[2]
bloc_deltaBIC10 = bloc_BIC1 - bloc_BIC0
bloc_BF01 = exp(1) ^ (bloc_deltaBIC10 / 2)
bloc_BF10 = 1 / bloc_BF01
bloc_BF10

# nloc
nloc_BF = df %>%
  mutate(condition = as.character(condition)) %>%
  filter(condition == "non-by-locative")

fit0_nloc_BF = glmer(
  by_passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = nloc_BF
)

fit1_nloc_BF = glmer(
  by_passive_YesNo ~ prime_type +
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = nloc_BF
)

nloc_BIC0 = anova(fit0_nloc_BF, fit1_nloc_BF)$BIC[1]
nloc_BIC1 = anova(fit0_nloc_BF, fit1_nloc_BF)$BIC[2]
nloc_deltaBIC10 = nloc_BIC1 - nloc_BIC0
nloc_BF01 = exp(1) ^ (nloc_deltaBIC10 / 2)
nloc_BF10 = 1 / nloc_BF01
nloc_BF10
