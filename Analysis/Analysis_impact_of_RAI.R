
# Load libraries and data ----

library(tidyverse)
library(here)
library(vroom)
library(xtable)
library(tidymodels)
library(ggridges)
source(here("Analysis", "utils.R"))

df <- vroom(here("Data", "Predictions.csv"))
perc <- vroom(here("Data", "Perception.csv")) %>% inner_join(df %>% distinct(id, condition, setting), by = "id")

## additional analysis
# drop predictions that took < 5 seconds or 10 seconds
# time_ths <- 5
# df <- df %>% filter(time_h >= time_ths & !is.na(time_h) |
#                       (time_hr >= time_ths & is.na(time_h)))
# min(df$time_h, na.rm = T); min(df[df$setting=='Anchor',]$time_hr, na.rm = T)
# nrow(df)
# OR
# demo <- vroom(here('Data', 'Demographics.csv'))
# df <- df %>% inner_join(demo %>% filter(ml_familiarity == 'No'))

# process data: get all answers (both h and hr) in the same column
# but keep qp and qb in the same row
df_long <- df %>%
  select(qp_hr, qb_hr, qp_r, qb_r, id, setting, condition, survey_part, outcome, order_question) %>%
  rename(qp = qp_hr, qb = qb_hr) %>%
  mutate(type_answer = "hr") %>%
  bind_rows(
    df %>%
      select(qp_h, qb_h, qp_r, qb_r, id, setting, condition, survey_part, outcome, order_question) %>%
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
  distinct(id, setting) %>%
  count(setting) %>%
  mutate(prop = round(n / sum(n), 3))
df %>%
  distinct(id, condition) %>%
  count(condition) %>%
  mutate(prop = round(n / sum(n), 3))
df %>%
  distinct(id, setting, condition) %>%
  count(setting, condition) %>%
  mutate(prop = round(n / sum(n), 3)) %>%
  mutate(text = paste0(prop * 100, "% (", n, ")")) %>%
  select(-n, -prop) %>%
  pivot_wider(names_from = setting, values_from = text) %>%
  xtable()


# Analysis of anchoring vs non-anchoring ----

# subset observations in the anchoring vs non-anchoring
df_a <- df_long %>%
  filter(type_answer == "hr" & survey_part == 2)

# performance
# base rate
base <- df_a %>%
  group_by(setting) %>%
  summarise(value = mean(outcome))
# % predicted positives
pos <- df_a %>%
  group_by(setting) %>%
  summarise(value = mean(qb), se = sd(qb) / sqrt(n()))
# accuracy
acc <- df_a %>%
  group_by(setting) %>%
  summarise(value = mean(qb == outcome), se = sd(qb == outcome) / sqrt(n()))
t.test(
  df_a[df_a$setting == "Anchor", ]$outcome == df_a[df_a$setting == "Anchor", ]$qb,
  df_a[df_a$setting == "No anchor", ]$outcome == df_a[df_a$setting == "No anchor", ]$qb
)$p.value
# false positive rate
fpr <- df_a %>%
  filter(outcome == 0) %>%
  group_by(setting) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_a[df_a$setting == "Anchor" & df_a$outcome == 0, ]$qb,
  df_a[df_a$setting == "No anchor" & df_a$outcome == 0, ]$qb
)$p.value
# false negative rate
fnr <- df_a %>%
  filter(outcome == 1) %>%
  group_by(setting) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_a[df_a$setting == "Anchor" & df_a$outcome == 1, ]$qb,
  df_a[df_a$setting == "No anchor" & df_a$outcome == 1, ]$qb
)$p.value
# positive predicted values
ppv <- df_a %>%
  filter(qb == 1) %>%
  group_by(setting) %>%
  summarise(value = mean(outcome), se = sd(outcome) / sqrt(n()))
t.test(
  df_a[df_a$setting == "Anchor" & df_a$qb == 1, ]$outcome,
  df_a[df_a$setting == "No anchor" & df_a$qb == 1, ]$outcome
)$p.value
# auc CI via empirical bootstrap
auc <- as.list(1:300) %>%
  purrr::map(
    ~ df_a %>%
      group_by(setting) %>%
      sample_n(nrow(.), replace = T) %>% # sampling with replacement
      group_by(setting) %>%
      group_modify(~ roc_auc(., outcome %>% as.factor(), qp, event_level = "second")) # compute AUC
  ) %>%
  bind_rows() %>%
  select(-.metric, -.estimator) %>%
  rename(value = .estimate) %>%
  group_by(setting) %>%
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
  bind_rows(base %>% mutate(metric = "Base rate"))


# difference between the RAI and human risk estimates, overall
df_a %>%
  group_by(setting) %>%
  summarise(mean_abs_diff = mean(abs(qp - qp_r)), 
            median_diff = median(abs(qp - qp_r)))
t.test(
  abs(df_a[df_a$setting == "Anchor", ]$qp - df_a[df_a$setting == "Anchor", ]$qp_r),
  abs(df_a[df_a$setting == "No anchor", ]$qp - df_a[df_a$setting == "No anchor", ]$qp_r)
)
wilcox.test(
  abs(df_a[df_a$setting == "Anchor", ]$qp - df_a[df_a$setting == "Anchor", ]$qp_r),
  abs(df_a[df_a$setting == "No anchor", ]$qp - df_a[df_a$setting == "No anchor", ]$qp_r)
)
# absolute difference between the RAI and human risk estimates, by RAI risk estimate
df_a %>%
  mutate(qp_r_bin = floor(qp_r * 5) / 5) %>%
  group_by(qp_r_bin, setting) %>%
  summarise(value = mean(abs(qp - qp_r)), se = sd(abs(qp - qp_r)) / sqrt(n())) %>%
  ggplot(aes(qp_r_bin, value, col = setting)) +
  geom_point() +
  geom_errorbar(aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se))
# boxplot to visualize the same target
df_a %>%
  mutate(qp_r_bin = as.factor(floor(qp_r * 5) / 5)) %>%
  mutate(diff_hr_r = qp - qp_r) %>%
  ggplot(aes(qp_r_bin, diff_hr_r, fill = setting)) +
  geom_point() +
  geom_boxplot()
# wilcox for each level of RAI's predictions
for (binned_score in 0:4) {
  (df_a %>% mutate(qp_r_bin = floor(qp_r * 5) / 5) %>%
    filter(qp_r_bin == binned_score / 5) %>%
    wilcox.test(data = ., abs(qp - qp_r) ~ setting))$p.value %>% print()
}
# difference between the RAI and human risk estimates, by condition
df_a %>%
  group_by(condition, setting) %>%
  summarise(mean_abs_diff = mean(abs(qp - qp_r)))
for (sel_condition in unique(df_a$condition)) {
  (df_a %>% filter(condition == sel_condition) %>% wilcox.test(data = ., abs(qp_r - qp) ~ setting))$p.value %>% print()
}
# compare with the pre-registered risk estimates in the non-anchoring setting
df %>%
  filter(survey_part == 2 & setting == "No anchor") %>%
  summarise(mean_abs_diff = mean(abs(qp_h - qp_r)))
# additional check: condition on the RAI's accuracy
df_a %>%
  group_by(id) %>%
  summarise(acc_r = mean(qb_r == outcome)) %>%
  inner_join(df_a) %>%
  group_by(condition, acc_r, setting) %>%
  summarise(mean_abs_diff = mean(abs(qp_r - qp)), n = n()) %>%
  filter(n > 200) %>%
  print(n = 500)

# disagreement between RAI and human binary predictions
df_a %>%
  group_by(setting) %>%
  summarise(value = mean(qb != qb_r), se = sd(qb != qb_r) / sqrt(n()))
# compare to the disagreement with the pre-registered binary predictions in the non-anchoring setting
df %>%
  filter(survey_part == 2 & setting == "No anchor") %>%
  summarise(value = mean(qb_h != qb_r))
# check if it varies across the conditions
df_a %>%
  group_by(condition, setting) %>%
  summarise(value = mean(qb != qb_r), se = sd(qb != qb_r) / sqrt(n()))
# additional check: condition on the RAI's accuracy
df_a %>%
  group_by(id) %>%
  summarise(acc_r = mean(qb_r == outcome)) %>%
  inner_join(df_a) %>%
  group_by(condition, acc_r, setting) %>%
  summarise(value = mean(qb_r == qb), n = n()) %>%
  filter(n > 200) %>%
  print(n = 500)


# plot agreement with model for probability predictions
labels_plot <- paste0(seq(0, 90, by = 10), "-", seq(0, 90, by = 10) + 9, "%")
labels_plot[10] <- "90-100%"
p <- df_a %>%
  mutate(qp_r = as.character(floor(qp_r * 10) / 10)) %>%
  filter(type_answer == "hr") %>%
  ggplot(aes(x = qp, y = qp_r, fill = setting)) +
  geom_density_ridges(alpha = 0.3) +
  scale_y_discrete(bquote("RAI risk estimate" ~ Q[RAI]^p), labels = labels_plot) +
  scale_x_continuous(bquote(Q^p), breaks = seq(0, 1, by = 0.1), labels = paste0(seq(0, 100, by = 10), "%"), limits = c(0, 1)) +
  scale_fill_manual(name = "Risk estimate", values = c("blue", "#E69F00")) +
  # theme_bw() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 10)) +
  coord_flip()
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, filename = here("Plots", "Density_risk_estimates_anchoring_vs_nonanc.pdf"), device = cairo_pdf)
# plot agreement with model for binary predictions
p <- df_a %>%
  mutate(qp_r = as.character(floor(qp_r * 10) / 10)) %>%
  filter(type_answer == "hr") %>%
  group_by(setting) %>%
  summarise(agreement = mean(qb_r == qb)) %>%
  ggplot(aes(setting, agreement, fill = setting)) +
  geom_col() +
  scale_fill_manual(name = "Risk estimate", values = c("blue", "#E69F00")) +
  scale_x_discrete(name = "", breaks = c("", "")) +
  scale_y_continuous(name = bquote("%" ~ Q[P + RAI]^b ~ "=" ~ Q[P + RAI]^b), limits = c(0, 1))
p %>% set_theme_ggplot()
ggsave(width = 4, height = 4, filename = here("Plots", "Binary_predictions_anchoring_vs_nonanc.pdf"), device = cairo_pdf)

# perceived use of the RAI
# create data
dp <- perc %>%
  filter(set_q == 3) %>%
  inner_join(df_a %>% distinct(setting, condition, id), by = "id")
# self-reported revision of binary predictions and risk estimates
dp %>%
  group_by(setting) %>%
  summarise(use_binary = mean(use_binary), use_likelihood = mean(use_likelihood))
wilcox.test(dp$use_binary ~ dp$setting)
wilcox.test(dp$use_likelihood ~ dp$setting)
# perceived trust in the tool
dp %>%
  group_by(setting) %>%
  summarise(trust = mean(trust == "Yes"))
t.test(ifelse(dp$trust == "Yes", 1, 0) ~ dp$setting)
# perceived confidence
dp %>%
  group_by(setting) %>%
  group_by(setting, confidence) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), 3))
# perceived accuracy
dp %>%
  group_by(setting) %>%
  group_by(setting) %>%
  summarise(value = mean(humanaccuracy))



# Analysis of impact of the RAI in the non-anchoring setting ----

# subset data of interest
df_na <- df %>% filter(survey_part == 2 & setting == "No anchor")
# create also a long version of the data
df_na_long <- df_na %>%
  select(qp_hr, qb_hr, qp_r, qb_r, id, setting, condition, survey_part, outcome) %>%
  rename(qp = qp_hr, qb = qb_hr) %>%
  mutate(type_answer = "hr") %>%
  bind_rows(df_na %>%
    select(qp_h, qb_h, qp_r, qb_r, id, setting, condition, survey_part, outcome) %>%
    rename(qp = qp_h, qb = qb_h) %>%
    mutate(type_answer = "h")) %>%
  filter(!is.na(qp))


# performance
# % predicted positives
pos <- df_na_long %>%
  group_by(type_answer) %>%
  summarise(value = mean(qb), se = sd(qb) / sqrt(n()))
# accuracy
acc <- df_na_long %>%
  group_by(type_answer) %>%
  summarise(value = mean(qb == outcome), se = sd(qb == outcome) / sqrt(n()))
t.test(
  df_na_long[df_na_long$type_answer == "h", ]$outcome == df_na_long[df_na_long$type_answer == "h", ]$qb,
  df_na_long[df_na_long$type_answer == "hr", ]$outcome == df_na_long[df_na_long$type_answer == "hr", ]$qb
)$p.value
# false positive rate
fpr <- df_na_long %>%
  filter(outcome == 0) %>%
  group_by(type_answer) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_na_long[df_na_long$type_answer == "h" & df_na_long$outcome == 0, ]$qb,
  df_na_long[df_na_long$type_answer == "hr" & df_na_long$outcome == 0, ]$qb
)$p.value
# false negative rate
fnr <- df_na_long %>%
  filter(outcome == 1) %>%
  group_by(type_answer) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  df_na_long[df_na_long$type_answer == "h" & df_na_long$outcome == 1, ]$qb,
  df_na_long[df_na_long$type_answer == "hr" & df_na_long$outcome == 1, ]$qb
)$p.value
# positive predicted values
ppv <- df_na_long %>%
  filter(qb == 1) %>%
  group_by(type_answer) %>%
  summarise(value = mean(outcome), se = sd(outcome) / sqrt(n()))
t.test(
  df_na_long[df_na_long$type_answer == "h" & df_na_long$qb == 1, ]$outcome,
  df_na_long[df_na_long$type_answer == "hr" & df_na_long$qb == 1, ]$outcome
)$p.value
# auc
auc <- as.list(1:300) %>%
  purrr::map(
    ~ df_na_long %>%
      group_by(type_answer) %>%
      sample_n(nrow(.), replace = T) %>% # sampling with replacement
      group_by(type_answer) %>%
      group_modify(~ roc_auc(., outcome %>% as.factor(), qp, event_level = "second")) # compute AUC via percentile
  ) %>%
  bind_rows() %>%
  select(-.metric, -.estimator) %>%
  rename(value = .estimate) %>%
  group_by(type_answer) %>%
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
  select(metric, value, type_answer) %>%
  pivot_wider(names_from = type_answer, values_from = value)

# accuracy of the RAI
df_na %>% summarise(value = mean(outcome == qb_r))
# AUC for the RAI
roc_auc(df_na, as.factor(outcome), qp_r, event_level = "second")

# average influence by initial difference
df_na <- df_na %>% mutate(influence = ifelse(abs(qp_h - qp_r) > 0.05, (qp_hr - qp_h) / (qp_r - qp_h), NA))
df_na %>% summarise(mean_inf = mean(influence, na.rm = T), median_inf = median(influence, na.rm = T))
labels <- paste0(ifelse(seq(-6, 2) * 10 < 0, paste0("(", seq(-6, 2) * 10, ")"), seq(-6, 2) * 10), "-", ifelse(seq(-5, 3) * 10 < 0, paste0("(", seq(-5, 3) * 10, "%", ")"), paste0(seq(-5, 3) * 10, "%")))
p <- df_na %>%
  mutate(initial_risk_diff = floor((qp_r - qp_h) * 10) / 10) %>%
  filter(initial_risk_diff >= -0.5 & initial_risk_diff <= 0.3) %>%
  ggplot(aes(initial_risk_diff, influence, group = initial_risk_diff)) +
  geom_boxplot(notch = T, fill = "#E69F00") +
  theme_bw() +
  xlab(bquote(Q[RAI]^p ~ "-" ~ Q[P]^p)) +
  scale_x_continuous(breaks = seq(-0.5, 0.3, by = 0.1), labels = labels) +
  scale_y_continuous(name = "Influence", limits = c(-1.6, 1.6), breaks = seq(-1.5, 1.5, by = 0.5), labels = paste0(round(seq(-1.5, 1.5, by = 0.5) * 100), "%"))
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, here("Plots", "Scores_no_anchoring.pdf"), device = cairo_pdf)
# influence is larger in one direction?
df_na %>%
  group_by(qp_r - qp_h > 0) %>%
  summarise(mean_influence = mean(influence, na.rm = T), mean_revision = mean(abs(qp_hr - qp_h)), size_initial_diff = mean(qp_r - qp_h), n = n())
wilcox.test(df_na$influence ~ ifelse((df_na$qp_r - df_na$qp_h) > 0, 1, 0))
t.test(abs(df_na$qp_hr - df_na$qp_h) ~ ifelse((df_na$qp_r - df_na$qp_h) > 0, 1, 0))
# correlation between revision and influence
cor.test(df_na$influence, df_na$qp_r - df_na$qp_h)
cor.test(df_na[(df_na$qp_r - df_na$qp_h) > 0, ]$influence, df_na[(df_na$qp_r - df_na$qp_h) > 0, ]$qp_r - df_na[(df_na$qp_r - df_na$qp_h) > 0, ]$qp_h, method = "spearman")
cor.test(df_na[(df_na$qp_r - df_na$qp_h) < 0, ]$influence, df_na[(df_na$qp_r - df_na$qp_h) < 0, ]$qp_r - df_na[(df_na$qp_r - df_na$qp_h) < 0, ]$qp_h, method = "spearman")

# average deviation
df_na_long %>%
  group_by(type_answer) %>%
  summarise(mean_dev = mean(abs(qp - qp_r), na.rm = T), median_dev = median(abs(qp - qp_r), na.rm = T))

# distance of risk estimates from the RAI before and after seeing the RAI's prediction
p <- df_na %>%
  mutate(initial_diff = abs(qp_h - qp_r), final_diff = abs(qp_hr - qp_r)) %>%
  pivot_longer(cols = c("initial_diff", "final_diff"), names_to = "type", values_to = "value") %>%
  ggplot(aes(value, y = ..density.., fill = type)) +
  geom_density(alpha = 0.4) +
  theme_bw() +
  scale_fill_manual(name = "Risk estimate", labels = c(bquote("|" ~ Q[P + RAI] - Q[RAI] ~ "|"), bquote("|" ~ Q[P] - Q[RAI] ~ "|")), values = c("#E69F00", "#999999")) +
  xlab("Value") +
  ylab("Density")
p %>% set_theme_ggplot()
ggsave(width = 6, height = 4, here("Plots", "Scores_no_anchoring_update_risk_estimates.pdf"), device = cairo_pdf)

# mean influence by participant
inf_part <- df_na %>%
  group_by(id) %>%
  summarise(mean_influence = mean(influence, na.rm = T), mean_final_diff = mean(abs(qp_r - qp_hr))) %>%
  arrange(mean_influence) %>%
  print(n = 500)
p <- inf_part %>%
  mutate(id = as.factor(id)) %>%
  ggplot(aes(mean_influence)) +
  geom_density(fill = "#E69F00") +
  xlab("Mean influence per participant") +
  ylab("Density") +
  theme_bw() +
  xlim(-0.5, 1)
p %>% set_theme_ggplot()
ggsave(width = 6, height = 4, here("Plots", "Scores_no_anchoring_influence_by_part.pdf"), device = cairo_pdf)
df_na %>%
  mutate(id = as.factor(id)) %>%
  ggplot(aes(id, influence)) +
  geom_boxplot() +
  ylim(-1, 2)
# test all
kruskal.test(df_na$influence ~ df_na$id)
# how many participants always matched the RAI's prediction? 
# (final prediction is always less than 10% far from the RAI)
df_na %>%
  group_by(id) %>%
  summarise(matched = mean(abs(qp_hr - qp_r) <= 0.1)) %>%
  filter(matched == 1) %>%
  nrow()
# how many participants never revised
df_na %>%
  group_by(id) %>%
  summarise(matched = mean(abs(qp_hr - qp_h) <= 0.1)) %>%
  filter(matched == 1) %>%
  nrow()
# how many participants always averaged the predictions
df_na %>%
  group_by(id) %>%
  summarise(matched = mean(abs(abs(qp_r + qp_h) / 2 - qp_hr) <= 0.1)) %>%
  filter(matched == 1) %>%
  nrow()
# get total number of participants
length(unique(df_na$id))

# influence from the RAI across questions
df_na %>%
  group_by(order_question) %>%
  summarise(mean_influence = mean(influence, na.rm = T))
kruskal.test(df_na$influence ~ df_na$order_question)

# how often binary predictions are revised
mean(df_na$qb_hr != df_na$qb_h)
sd(df_na$qb_hr != df_na$qb_h) / sqrt(length(df_na$qb_hr))
# quick check: out of cases initial prediction does not agree, % final 
# predictions agree
mean(df_na[df_na$qb_h!=df_na$qb_r,]$qb_hr == df_na[df_na$qb_h!=df_na$qb_r,]$qb_r)
# how often and how do participants switch
df_na %>%
  count(qb_h, qb_hr) %>%
  mutate(prop = n / sum(n))
# how much agreement with the RAI's predictions
mean(df_na$qb_h == df_na$qb_r)
mean(df_na$qb_hr == df_na$qb_r)
t.test(df_na$qb_h == df_na$qb_r, df_na$qb_hr == df_na$qb_r)
# when participants switch pred., do they do that to match the RAI's prediction?
mean(df_na[df_na$qb_h != df_na$qb_hr, ]$qb_hr == df_na[df_na$qb_h != df_na$qb_hr, ]$qb_r)
# how often did their initial estimate match the RAI's but their final did not
mean(df_na[df_na$qb_h == df_na$qb_r, ]$qb_hr != df_na[df_na$qb_h == df_na$qb_r, ]$qb_r)
# then, do they switch because the risk is higher than their estimate?
df_na %>%
  filter(qb_h != qb_hr) %>%
  group_by(qb_hr, qp_h < qp_r) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))
# agreement when the RAI's prediction is no re-arrest
df_na %>%
  filter(qb_r == 0) %>%
  summarise(mean_initial = mean(qb_h == qb_r), mean_final = mean(qb_hr == qb_r))
# agreement when the RAI's prediction is re-arrest
df_na %>%
  filter(qb_r == 1) %>%
  summarise(mean_initial = mean(qb_h == qb_r), mean_final = mean(qb_hr == qb_r))
# did the likelihood of adherence vary with the RAI's recommendation?
# when no re-arrest and did not agree: likelihood of switching
mean((df_na %>% filter(qb_r == 0 & qb_h != qb_r))$qb_hr == (df_na %>% filter(qb_r == 0 & qb_h != qb_r))$qb_r)
mean((df_na %>% filter(qb_r == 1 & qb_h != qb_r))$qb_hr == (df_na %>% filter(qb_r == 1 & qb_h != qb_r))$qb_r)
t.test(
  (df_na %>% filter(qb_r == 0 & qb_h != qb_r))$qb_hr == (df_na %>% filter(qb_r == 0 & qb_h != qb_r))$qb_r,
  (df_na %>% filter(qb_r == 1 & qb_h != qb_r))$qb_hr == (df_na %>% filter(qb_r == 1 & qb_h != qb_r))$qb_r
)

# plot final binary prediction ~ RAI prediction and initial prediction
p <- df_na %>%
  group_by(qb_h, qb_r, qb_hr) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(qb_h) %>%
  mutate(prop = round(n / sum(n), 3) * 100) %>%
  mutate_at(c("qb_h", "qb_hr", "qb_r"), function(x) ifelse(x == 0, "No re-arrest", "Re-arrest")) %>%
  ggplot(aes(qb_hr, qb_r, fill = prop, label = paste0(prop, "%"))) +
  geom_tile() +
  facet_grid(~qb_h) +
  geom_text(color = "white") +
  theme_bw() +
  scale_fill_gradient(name = "Share", low = "#009E73", high = "#0072B2") +
  ylab(bquote(Q[RAI]^b)) +
  xlab(bquote(Q[P + RAI]^b))
p %>% set_theme_ggplot()
ggsave(width = 6, height = 4, here("Plots", "Revision_matrix.pdf"), device = cairo_pdf)
# how many of the initial predictions were re-arrests?
mean(df_na$qb_h)


# plot updates by RAI's prediction
p <- df_na_long %>%
  mutate(qp_r_binned = floor(qp_r * 10) / 10) %>%
  group_by(qp_r_binned, type_answer) %>%
  summarise(value = mean(qb), se = sd(qb) / sqrt(n())) %>%
  ggplot(aes(qp_r_binned, value, fill = type_answer)) +
  geom_col(position = position_dodge()) +
  geom_errorbar(aes(ymin = value - 1.96 * se, ymax = value + 1.96 * se), width = 0.02, position = position_dodge(width = 0.1)) +
  scale_x_continuous(bquote("RAI risk estimate" ~ Q[RAI]^p), breaks = seq(0, 0.9, length = 10), labels = labels_plot) +
  scale_fill_manual(name = "Prediction", values = c("#999999", "#E69F00"), labels = c(bquote(Q[P]^b), bquote(Q[P + RAI]^b))) +
  scale_y_continuous(bquote("%" ~ Q^b ~ '="re-arrest"'), breaks = seq(0, 1, by = 0.1), labels = paste0(seq(0, 100, by = 10), "%"))
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, here("Plots", "Binary_predictions_no_anchoring.pdf"), device = cairo_pdf)
# does the switching depend on the risk estimate of the RAI
df_na %>%
  filter(qb_h != qb_r) %>%
  group_by(floor(qp_r * 10) / 10) %>%
  summarise(value = mean(qb_hr == qb_r), n = n())
cor.test(df_na %>% filter(qb_h != qb_r & qp_r < 0.5) %>% mutate(changed = ifelse(qb_hr == 0, 1, 0)) %>% pull(changed),
  (df_na %>% filter(qb_h != qb_r & qp_r < 0.5))$qp_r,
  method = "spearman"
)

# perceived use of likelihood
nap <- df_na %>% distinct(id) %>% 
  inner_join(perc %>% filter(set_q == 3), by = 'id')
nap %>% summarise(mean(use_likelihood), median(use_likelihood))
nap %>% summarise(mean(use_binary), median(use_binary))

# perceived and real use of likelihood in perception questions
nap <- df_na %>% inner_join(perc %>% filter(set_q == 3), by = "id")
use <- nap %>%
  group_by(id) %>%
  summarise(
    mean_influence = mean(influence, na.rm = T),
    use_likelihood = use_likelihood[1],
    mean_initial_diff = mean(abs(qp_r - qp_h)),
    mean_final_diff = mean(abs(qp_r - qp_hr)),
    trust = ifelse(trust[1] == "Yes", 1, 0)
  )
cor.test(use$use_likelihood, use$mean_influence, method = "spearman")
# how many participants trusted the tool
nap %>%
  distinct(id, set_q, trust) %>%
  count(trust) %>%
  mutate(prop = n / sum(n))
# perceived trust in the tool and distance from the RAI's predictions
use %>%
  group_by(trust) %>%
  summarise(
    mean_influence = mean(mean_influence),
    mean_initial_diff = mean(mean_initial_diff),
    mean_final_diff = mean(mean_final_diff)
  )
wilcox.test(use$mean_influence ~ use$trust)


# switching: is the prediction more correct?
df_na %>%
  filter(qb_h != qb_hr) %>%
  summarise(acc_qb_h = mean(outcome == qb_h), acc_qb_r = mean(outcome == qb_hr))

# density estimates before and after update (two or one plot)
labels_plot <- paste0(seq(0, 90, by = 10), "-", seq(0, 90, by = 10) + 9, "%")
labels_plot[10] <- "90-100%"
# p_h <- df_na %>% mutate(qp_r = as.character(floor(qp_r*10)/10)) %>%
#   ggplot(aes(x = qp_h, y = qp_r)) + geom_density_ridges(fill = "#999999") +
#   theme_bw() + ylab(bquote(Q[RAI]^P)) +
#   scale_y_discrete(bquote('RAI risk estimate'~Q[RAI]), labels = labels_plot) +
#   scale_x_continuous(bquote(Q[P]^P), breaks = seq(0,1, by=0.1), labels = paste0(seq(0,100,by=10), '%'), limits = c(0,1))
# p_hr <- df_na %>% mutate(qp_r = as.character(floor(qp_r*10)/10)) %>%
#   ggplot(aes(x = qp_hr, y = qp_r)) + geom_density_ridges(fill = "#E69F00") +
#   theme_bw() + ylab(bquote(Q[RAI]^P)) +
#   scale_y_discrete('', labels = NULL) +
#   scale_x_continuous(bquote(Q[P+RAI]^P), breaks = seq(0,1, by=0.1), labels = paste0(seq(0,100,by=10), '%'), limits = c(0,1))
# grid.arrange(p_h, p_hr, ncol = 2) %>%
#   ggsave(width = 8, height = 4, filename = here('Plots', 'Density_risk_estimates_no_anchoring.pdf'), device = cairo_pdf)
p <- df_na %>%
  mutate(qp_r = as.character(floor(qp_r * 10) / 10)) %>%
  select(qp_r, qp_h, qp_hr) %>%
  pivot_longer(cols = c("qp_h", "qp_hr"), names_to = "type", values_to = "value") %>%
  ggplot(aes(x = value, y = qp_r, fill = type)) +
  geom_density_ridges(alpha = 0.3) +
  scale_y_discrete(bquote("RAI risk estimate" ~ Q[RAI]^p), labels = labels_plot) +
  scale_x_continuous(bquote(Q^p), breaks = seq(0, 1, by = 0.1), labels = paste0(seq(0, 100, by = 10), "%"), limits = c(0, 1)) +
  scale_fill_manual(name = "Risk estimate", values = c("#999999", "#E69F00"), labels = c(bquote(Q[P]^p), bquote(Q[P + RAI]^p))) +
  # theme_bw() + theme(axis.title = element_text(size = 18), axis.text = element_text(size = 10)) +
  coord_flip()
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, filename = here("Plots", "Density_risk_estimates_no_anchoring.pdf"), device = cairo_pdf)


# Analysis of the two conditions (controlled and decreasing) ----

# subset data of interest
dfc <- df_long %>%
  filter(survey_part == 2 & condition != "None") %>%
  filter(type_answer != "h")
# check accuracy of the tool
dfc %>%
  group_by(condition) %>%
  summarise(acc = mean(qb_r == outcome))
# check number of questions per participant
mean((dfc %>% group_by(id) %>% summarise(n = n()))$n)
# recheck randomization
dfc %>%
  distinct(id, setting, condition) %>%
  group_by(condition, setting) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))


# performance
# base rate
base <- dfc %>%
  group_by(condition) %>%
  summarise(value = mean(outcome))
# % predicted positives
pos <- dfc %>%
  group_by(condition) %>%
  summarise(value = mean(qb), se = sd(qb) / sqrt(n()))
# accuracy
acc <- dfc %>%
  group_by(condition) %>%
  summarise(value = mean(qb == outcome), se = sd(qb == outcome) / sqrt(n()))
t.test(
  dfc[dfc$condition == "Decreasing", ]$outcome == dfc[dfc$condition == "Decreasing", ]$qb,
  dfc[dfc$condition == "Controlled", ]$outcome == dfc[dfc$condition == "Controlled", ]$qb
)$p.value
# false positive rate
fpr <- dfc %>%
  filter(outcome == 0) %>%
  group_by(condition) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  dfc[dfc$condition == "Decreasing" & dfc$outcome == 0, ]$qb,
  dfc[dfc$condition == "Controlled" & dfc$outcome == 0, ]$qb
)$p.value
# false negative rate
fnr <- dfc %>%
  filter(outcome == 1) %>%
  group_by(condition) %>%
  summarise(value = mean(qb != outcome), se = sd(qb != outcome) / sqrt(n()))
t.test(
  dfc[dfc$condition == "Decreasing" & dfc$outcome == 1, ]$qb,
  dfc[dfc$condition == "Controlled" & dfc$outcome == 1, ]$qb
)$p.value
# positive predicted values
ppv <- dfc %>%
  filter(qb == 1) %>%
  group_by(condition) %>%
  summarise(value = mean(outcome), se = sd(outcome) / sqrt(n()))
t.test(
  dfc[dfc$condition == "Decreasing" & dfc$qb == 1, ]$outcome,
  dfc[dfc$condition == "Controlled" & dfc$qb == 1, ]$outcome
)$p.value
# auc
auc <- as.list(1:300) %>%
  purrr::map(
    ~ dfc %>%
      group_by(condition) %>%
      sample_n(nrow(.), replace = T) %>% # sampling with replacement
      group_by(condition) %>%
      group_modify(~ roc_auc(., outcome %>% as.factor(), qp)) # compute AUC
  ) %>%
  bind_rows() %>%
  select(-.metric, -.estimator) %>%
  rename(value = .estimate) %>%
  group_by(condition) %>%
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
  select(metric, value, condition) %>%
  pivot_wider(names_from = condition, values_from = value)


# agreement with the RAI in terms of risk estimates and binary predictions
dfc %>%
  group_by(condition) %>%
  summarise(mean_diff = mean(abs(qp_r - qp)), mean_agreement = mean(qb_r == qb))
wilcox.test(abs(dfc$qp_r - dfc$qp) ~ dfc$condition)$p.value
# difference when the RAI is right or wrong
wilcox.test(abs(dfc[dfc$outcome == dfc$qb_r, ]$qp_r - dfc[dfc$outcome == dfc$qb_r, ]$qp) ~ dfc[dfc$outcome == dfc$qb_r, ]$condition)$p.value
wilcox.test(abs(dfc[dfc$outcome != dfc$qb_r, ]$qp_r - dfc[dfc$outcome != dfc$qb_r, ]$qp) ~ dfc[dfc$outcome != dfc$qb_r, ]$condition)$p.value
# agreement with order
dfc %>%
  group_by(condition, order_question) %>%
  summarise(mean_diff = mean(abs(qp_r - qp))) %>%
  ggplot(aes(order_question, mean_diff, col = condition)) +
  geom_point()

# perceived use and trust in the the tool
cp <- dfc %>%
  distinct(id, condition, setting) %>%
  inner_join(perc %>% filter(set_q != 1), by = "id")
cp %>%
  group_by(set_q, condition) %>%
  summarise(
    mean_use = mean(use_likelihood),
    mean_binary = mean(use_binary),
    mean_trust = mean(trust == "Yes")
  )
# confidence in their predictions
cp %>%
  group_by(set_q, condition, confidence) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n) %>%
  pivot_wider(names_from = condition, values_from = prop)

