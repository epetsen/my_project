# load packages
library(lme4) # for glmer()
library(brms) # for bayesian models
library(tidyverse) # for data wrangling and visualization
source("helpers.R")

# set working directory
this.dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(this.dir)

### DATA WRANGLING

# load data
df = read_csv("../data/df_manual2.csv")

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
    passive_YesNo = case_when(
      passive_active_other %in% c("BP", "P") ~ 1,
      passive_active_other == "A" ~ 0,
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
prop.table(table(df$passive_active_other))

# proportion of passives
df %>%
  summarize(prop = mean(passive_YesNo %>%
                          as.character() %>%
                          as.numeric(),
                        na.rm = T))

# proportion of passives by `prime_type`
prop_by_prime_type = df %>%
  group_by(prime_type) %>%
  summarize(prop = mean(passive_YesNo,
                        na.rm = T))

# proportion of passives by condition
prop_by_condition = df %>%
  group_by(condition) %>%
  summarize(prop = mean(passive_YesNo,
                        na.rm = T))

# proportion of passives by `prime_type` and `condition`
prop_by_prime_type_condition = df %>%
  group_by(prime_type, condition) %>%
  summarize(prop = mean(passive_YesNo,
                        na.rm = T))

# difference in proportion of passives in `prime_type` grouped by `condition`
diff_prop = prop_by_prime_type_condition %>%
  group_by(condition) %>%
  summarize(diff_prop = prop[prime_type == "non-active"] -
              prop[prime_type == "active"])

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

df$passive_YesNo = factor(df$passive_YesNo)

df = df %>%
  mutate_at(c("workerid", "picture"),
            as.character) %>%
  mutate_at(c("workerid", "picture"),
            as.factor)

# maximal model
fit_a = glmer(
  passive_YesNo ~ prime_type * condition +
    (1 + prime_type || workerid) +
    (1 + condition || picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit_a)

## FORWARD MODEL COMPARISONS

#model 1
fit1 = glmer(
  passive_YesNo ~
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit1)

#model 2
fit2 = glmer(
  passive_YesNo ~ prime_type +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit2)

# model 3
fit3 = glmer(
  passive_YesNo ~ prime_type + condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = df
)
summary(fit3)

# model 4
fit4 = glmer(
  passive_YesNo ~ prime_type * condition +
    (1 + prime_type | workerid) +
    (1 + condition | picture),
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
  passive_YesNo ~ prime_type * condition +
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
  passive_YesNo ~
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
  passive_YesNo ~
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
  passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = pass_BF
)

fit1_pass_BF = glmer(
  passive_YesNo ~ prime_type +
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
  passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = bloc_BF
)

fit1_bloc_BF = glmer(
  passive_YesNo ~ prime_type +
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
  passive_YesNo ~
    (1 | workerid) + (1 | picture),
  family = binomial,
  control = glmerControl(optimizer = "bobyqa"),
  data = nloc_BF
)

fit1_nloc_BF = glmer(
  passive_YesNo ~ prime_type +
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
