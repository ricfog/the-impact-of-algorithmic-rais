
# Load libraries and data ----

library(tidyverse)
library(here)
library(vroom)
library(xtable)
library(tidymodels)

df <- vroom(here("Data", "Predictions.csv"))
perc <- vroom(here("Data", "Perception.csv")) %>% inner_join(df %>% distinct(id, condition, setting), by = "id")
def <- vroom(here("Data", "Defendants_data.csv"))

# join defendants race and predictions
df <- df %>% inner_join(def %>% select(index, race), by = "index")

# process data: get all answers (both h and hr) in the same column
# but keep qp and qb in the same row
df_long_full <- df %>%
  select(qp_hr, qb_hr, qp_r, qb_r, id, race, setting, condition, survey_part, outcome, order_question, index) %>%
  rename(qp = qp_hr, qb = qb_hr) %>%
  mutate(type_answer = "hr") %>%
  bind_rows(
    df %>%
      select(qp_h, qb_h, qp_r, qb_r, id, race, setting, condition, survey_part, outcome, order_question, index) %>%
      rename(qp = qp_h, qb = qb_h) %>%
      mutate(type_answer = "h")
  ) %>%
  filter(!is.na(qp))

## group and transform answers in perception questions
perc <- perc %>%
  mutate(humanaccuracy_2 = ifelse(grepl("25", humanaccuracy_2), NA, humanaccuracy_2)) %>%
  rowwise() %>%
  mutate_at(paste0("use_binary_", 2:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1]) / as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(paste0("use_likelihood_", 2:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1]) / as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(paste0("humanaccuracy_", 1:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1]) / as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(c(paste0("confidence_", 1:3)), function(x) {
    case_when(
      x == "Moderately confident" | x == "Extremely confident" ~ "Confident",
      x == "Not confident at all" | x == "Slightly confident" ~ "Not confident",
      TRUE ~ "Somewhat confident"
    )
  }) %>%
  mutate_at(c(paste0("humanaccuracy_comparison_", 1:3)), function(x) {
    case_when(
      x == "among the lowest accuracies (0-20%)" | x == "lower than most accuracies (21-40%)" ~ "Lower",
      x == "approximately equal to the median accuracy (41-60%)" ~ "Median",
      TRUE ~ "Higher"
    )
  })

# transform dataframe into long format
perc1 <- perc %>% select(matches("1"), id)
colnames(perc1) <- str_replace(colnames(perc1), "_1", "")
perc2 <- perc %>% select(matches("2"), id)
colnames(perc2) <- str_replace(colnames(perc2), "_2", "")
perc3 <- perc %>% select(matches("3"), id)
colnames(perc3) <- str_replace(colnames(perc3), "_3", "")
perc <- perc1 %>%
  mutate(set_q = 1) %>%
  bind_rows(perc2 %>% mutate(set_q = 2)) %>%
  bind_rows(perc3 %>% mutate(set_q = 3))

# check randomization
df %>%
  distinct(id, setting, condition) %>%
  group_by(setting, condition) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), 3))




# Analysis of racial disparities ----

# use only data from human+RAI from the second and third parts of the survey
df_long <- df_long_full %>% filter(type_answer == "hr")

# performance
# base rate
base <- df_long %>%
  group_by(race) %>%
  summarise(value = mean(outcome))
# % predicted positives
pos <- df_long %>%
  group_by(race) %>%
  summarise(value = mean(qb), se = sd(qb) / sqrt(n()))
# accuracy
acc <- df_long %>%
  group_by(race) %>%
  summarise(value = mean(qb == outcome), se = sd(qb == outcome) / sqrt(n()))
t.test(
  df_long[df_long$race == "Black", ]$outcome == df_long[df_long$race == "Black", ]$qb,
  df_long[df_long$race == "White", ]$outcome == df_long[df_long$race == "White", ]$qb
)$p.value
# false positive rate
fpr <- df_long %>%
  filter(outcome == 0) %>%
  group_by(race) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_long[df_long$race == "Black" & df_long$outcome == 0, ]$qb,
  df_long[df_long$race == "White" & df_long$outcome == 0, ]$qb
)$p.value
# false negative rate
fnr <- df_long %>%
  filter(outcome == 1) %>%
  group_by(race) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_long[df_long$race == "Black" & df_long$outcome == 1, ]$qb,
  df_long[df_long$race == "White" & df_long$outcome == 1, ]$qb
)$p.value
# positive predicted values
ppv <- df_long %>%
  filter(qb == 1) %>%
  group_by(race) %>%
  summarise(value = mean(outcome), se = sd(outcome) / sqrt(n()))
t.test(
  df_long[df_long$race == "Black" & df_long$qb == 1, ]$outcome,
  df_long[df_long$race == "White" & df_long$qb == 1, ]$outcome
)$p.value
# auc
auc <- as.list(1:300) %>%
  purrr::map(
    ~ df_long %>%
      group_by(race) %>%
      sample_n(nrow(.), replace = T) %>%
      # sampling with replacement
      group_by(race) %>%
      group_modify(~ roc_auc(., outcome %>% as.factor(), qp)) # compute AUC
  ) %>%
  bind_rows() %>%
  select(-.metric, -.estimator) %>%
  rename(value = .estimate) %>%
  group_by(race) %>%
  summarise(lb = quantile(value, probs = 0.025), ub = quantile(value, probs = 0.975), value = mean(value))

# gather all metrics together
metrics <- pos %>%
  mutate(metric = "Fraction of predicted positives") %>%
  bind_rows(acc %>% mutate(metric = "Accuracy")) %>%
  bind_rows(fpr %>% mutate(metric = "False positive rate (FPR)")) %>%
  bind_rows(fnr %>% mutate(metric = "False negative rate (FNR)")) %>%
  bind_rows(ppv %>% mutate(metric = "Positive predicted value (PPV)")) %>%
  mutate(lb = value - 1.96 * se, ub = value + 1.96 * se) %>%
  select(-se) %>%
  bind_rows(auc %>% mutate(metric = "Area under the curve (AUC)"))
metrics %>%
  bind_rows(base %>% mutate(metric = "Base rate")) %>%
  select(metric, value, race) %>%
  pivot_wider(names_from = race, values_from = value)

# calibration testing via logisitc regression
tidy(glm(outcome ~ qp + race, df_long, family = binomial()))
tidy(glm(outcome ~ qp * race, df_long, family = binomial()))
# calibration testing via chi-squared
df_long %>%
  group_by(floor(pmin(qp, 0.99) * 10) / 10) %>%
  summarise(chisq.test(outcome, race)$p.value, n = n())
# plot calibration
df_long %>%
  mutate(qp_binned = floor(pmin(qp, 0.99) * 10) / 10) %>%
  group_by(qp_binned, race) %>%
  summarise(mean_outcome = mean(outcome), se = sd(outcome) / sqrt(n())) %>%
  ggplot(aes(qp_binned, mean_outcome, fill = race)) +
  geom_col(position = position_dodge2()) +
  theme_bw() +
  geom_errorbar(aes(ymin = mean_outcome - 1.96 * se, ymax = mean_outcome + 1.96 * se), position = position_dodge2(width = 0.9, padding = .6))


# influence in non-anchoring
df_na <- df %>% filter(setting == "No anchor" & survey_part == 2)
df_na <- df_na %>% mutate(influence = ifelse(abs(qp_r - qp_h) >= 0.05, (qp_hr - qp_h) / (qp_r - qp_h), NA))
# influence by race
df_na %>%
  group_by(race) %>%
  summarise(mean_influence = mean(influence, na.rm = T), median_influence = median(influence, na.rm = T))
wilcox.test(df_na$influence ~ df_na$race)
# when the RAI risk estimate is higher than the human risk estimate
df_na %>%
  group_by(race, qp_h < qp_r) %>%
  summarise(mean_influence = mean(influence, na.rm = T), median_influence = median(influence, na.rm = T))
wilcox.test(df_na[df_na$qp_h < df_na$qp_r, ]$influence ~ df_na[df_na$qp_h < df_na$qp_r, ]$race)

# influence overall
# get all offenders for which we have at least three predictions of each type (H and H+RAI)
dfi <- df_long_full %>%
  group_by(index, type_answer) %>%
  mutate(n = n()) %>%
  filter(n >= 3) %>%
  ungroup() %>%
  group_by(index) %>%
  filter(length(unique(type_answer)) == 2)
length(unique(dfi$index))
# average the predictions
dfi <- dfi %>%
  group_by(index, type_answer, race) %>%
  summarise(qp = mean(qp), qp_r = qp_r[1]) %>%
  pivot_wider(names_from = type_answer, values_from = qp)
dfi <- dfi %>% mutate(influence = ifelse(abs(qp_r - h) >= 0.05, (hr - h) / (qp_r - h), NA))
# influence by race
dfi %>%
  group_by(race) %>%
  summarise(mean_influence = mean(influence, na.rm = T), median_influence = median(influence, na.rm = T))
wilcox.test(dfi$influence ~ dfi$race)
# when the RAI risk estimate is higher than the human risk estimate
dfi %>%
  group_by(race, h < qp_r) %>%
  summarise(mean_influence = mean(influence, na.rm = T), median_influence = median(influence, na.rm = T), n = n())
wilcox.test(dfi[dfi$h < dfi$qp_r, ]$influence ~ dfi[dfi$h < dfi$qp_r, ]$race)

