
# Load libraries and data -----

library(tidyverse)
library(here)
library(vroom)
library(xtable)
library(tidymodels)
source(here("Analysis", "utils.R"))

df <- vroom(here("Data", "Predictions.csv")) # %>%
# filter(condition != 'Decreasing')

## additional analysis
# drop predictions that took < 5 seconds or 10 seconds
# time_ths <- 10
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
  select(qp_hr, qb_hr, id, setting, survey_part, outcome, condition, time_hr) %>%
  rename(qp = qp_hr, qb = qb_hr, time = time_hr) %>%
  mutate(type_answer = "hr") %>%
  bind_rows(
    df %>%
      select(qp_h, qb_h, id, setting, survey_part, outcome, condition, time_h) %>%
      rename(qp = qp_h, qb = qb_h, time = time_h) %>%
      mutate(type_answer = "h")
  ) %>%
  filter(!is.na(qp))

df_long_2hr <- df_long %>%
  filter(survey_part == 2) %>%
  filter(type_answer == "hr")



# Analysis of risk estimates -> binary predictions ----

# plot fraction of predicted re-arrests for each
labels_plot <- paste0(seq(0, 90, by = 10), "-", seq(0, 90, by = 10) + 9, "%")
labels_plot[10] <- "90-100%"
p <- df_long_2hr %>%
  mutate(qp_binned = floor(pmin(qp, 0.99) * 10) / 10) %>%
  filter(type_answer == "hr") %>%
  group_by(qp_binned) %>%
  summarise(mean_qb = mean(qb), n = n(), .groups = "keep") %>%
  mutate(se = sqrt(mean_qb * (1 - mean_qb) / n)) %>%
  ggplot(aes(qp_binned, mean_qb)) +
  geom_col(fill = "#E69F00") +
  geom_errorbar(aes(ymin = mean_qb - 1.96 * se, ymax = mean_qb + 1.96 * se), width = 0.02) +
  scale_x_continuous(bquote("Participant's binned risk estimate" ~ Q[P + RAI]^p),
    breaks = seq(0, 0.9, length = 10),
    labels = labels_plot
  ) +
  scale_y_continuous(bquote("% participant's" ~ Q[P + RAI]^b ~ "=re-arrest"),
    breaks = seq(0, 1, by = 0.2), labels = paste0(seq(0, 1, by = 0.2) * 100, "%")
  )
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, here("Plots", "Mapping_prob_dec.pdf"), device = cairo_pdf)


# predicted re-arrest for risk estimate < 50%
df_long_2hr %>%
  filter(qp < 0.5) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# predicted no re-arrest for risk estimate > 50%
df_long_2hr %>%
  filter(qp > 0.5) %>%
  summarise(mean = 1 - mean(qb), se = round(sd(qb) / sqrt(n()), 3))

# predicted re-arrest for each risk estimate
df_long_2hr %>%
  group_by(qp) %>%
  summarise(qb = mean(qb)) %>%
  ggplot(aes(qp, qb)) +
  geom_point()
# predicted re-arrest for risk estimate >=40% and <50%
df_long_2hr %>%
  filter(qp < 0.5 & qp >= 0.4) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# predicted re-arrest for risk estimate - 50%
df_long_2hr %>%
  filter(qp == 0.5) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# predicted no re-arrest for risk estimate >50% and <=60%
df_long_2hr %>%
  filter(qp > 0.5 & qp <= 0.6) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# test all differences
t.test((df_long_2hr %>% filter(qp < 0.5 & qp >= 0.4))$qb, (df_long_2hr %>% filter(qp == 0.5))$qb)$p.value
t.test((df_long_2hr %>% filter(qp < 0.5 & qp >= 0.4))$qb, (df_long_2hr %>% filter(qp <= 0.6 & qp > 0.5))$qb)$p.value
t.test((df_long_2hr %>% filter(qp <= 0.6 & qp > 0.5))$qb, (df_long_2hr %>% filter(qp == 0.5))$qb)$p.value


# plot fraction of predicted re-arrests for each risk estimate and worker
# sort ids by lowest risk estimate with qb positive
ids_sorted <- df_long_2hr %>%
  group_by(id) %>%
  filter(qb == 1) %>%
  summarise(min_qp = min(qp)) %>%
  arrange(min_qp) %>%
  pull(., id)
p <- df_long_2hr %>%
  mutate(binned_qp = floor(pmin(qp, 0.99) * 10) / 10) %>%
  group_by(id, binned_qp) %>%
  summarise(mean_qb = mean(qb)) %>%
  arrange(binned_qp, mean_qb) %>%
  ggplot(aes(factor(id, levels = ids_sorted), binned_qp, fill = mean_qb)) +
  geom_tile() +
  xlab("Participant") +
  scale_fill_gradientn(name = bquote(atop("% predictions", "re-arrest" ~ Q[P + RAI]^b)), colours = c("yellow", "red", "black")) +
  # scale_fill_gradient(bquote('Fraction of\npredicted\nre-arrests'*Q[P+RAI]^b), colours = c('white', 'yellow', 'red', 'black')) +
  scale_y_continuous(bquote("Participant's binned risk estimate" ~ Q[P + RAI]^p), breaks = seq(0, 0.9, length = 10), labels = labels_plot) +
  coord_flip()
p %>% set_theme_ggplot() +
  theme(
    axis.text.y = element_blank(), axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid = element_line(colour = "white")
  )
ggsave(width = 8, height = 4, here("Plots", "Mapping_prob_dec_by_id.pdf"), device = cairo_pdf)

# % of participants that adopt profit-maximizing strategy
df_long_2hr %>%
  filter(qp != 0.5) %>%
  mutate(is_optimal = ifelse(ifelse(qp > 0.5, 1, 0) == qb, 1, 0)) %>%
  group_by(id) %>%
  summarise(is_always_optimal = mean(is_optimal)) %>%
  ungroup() %>%
  # group_by(is_always_optimal == 1) %>% summarise(n = n())
  group_by(is_always_optimal > 0.9) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))


# % binary predictions are re-arrest
mean(df_long_2hr$qb)
# % risk estimates > 50% are re-arrest
mean(df_long_2hr[df_long_2hr$qp > 0.5, ]$qp)
# % risk estimates >= 50% are re-arrest
mean(df_long_2hr[df_long_2hr$qp >= 0.5, ]$qp)
# test differences
t.test(ifelse(df_long_2hr$qp > 0.5, 1, 0), df_long_2hr$qb, matched = T)$p.value
t.test(ifelse(df_long_2hr$qp >= 0.5, 1, 0), df_long_2hr$qb, matched = T)$p.value


# plot % binary predictions != risk estimate at 50% threshold vs time
df_long_2hr %>%
  mutate(time = round(time)) %>%
  group_by(time) %>%
  summarise(mean_inc = mean(qb != ifelse(qp > 0.5, 1, 0)), se_inc = sd(qb) / sqrt(n())) %>%
  ggplot(aes(time, mean_inc)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean_inc - 1.96 * se_inc, ymax = mean_inc + 1.96 * se_inc), alpha = 0.3) +
  xlim(3, 20) +
  ylim(0, 0.5) +
  theme_bw() +
  ylab("")



# Analysis of performance -----

# subset risk estimates != 50%
df_long5 <- df_long_2hr %>%
  filter(qp != 0.5) %>%
  filter(!(survey_part == 2 & type_answer == "h")) %>%
  mutate(qp_b = ifelse(qp > 0.5, 1, 0))


# accuracy of risk estimates vs binary predictions
df_long5 %>% summarise(mean(outcome == qp_b))
df_long5 %>% summarise(mean(outcome == qb))
# test difference
t.test(ifelse(df_long5$outcome == df_long5$qp_b, 1, 0), ifelse(df_long5$outcome == df_long5$qb, 1, 0))


# false positive rate
df_long5 %>%
  filter(outcome == 0) %>%
  summarise(mean(outcome != qp_b))
df_long5 %>%
  filter(outcome == 0) %>%
  summarise(mean(outcome != qb))
t.test(
  ifelse(df_long5[df_long5$outcome == 0, ]$qp_b != df_long5[df_long5$outcome == 0, ]$outcome, 1, 0),
  ifelse(df_long5[df_long5$outcome == 0, ]$qb != df_long5[df_long5$outcome == 0, ]$outcome, 1, 0)
)$p.value

# false negative rate
df_long5 %>%
  filter(outcome == 1) %>%
  summarise(mean(outcome != qp_b))
df_long5 %>%
  filter(outcome == 1) %>%
  summarise(mean(outcome != qb))
t.test(
  ifelse(df_long5[df_long5$outcome == 0, ]$qp_b != df_long5[df_long5$outcome == 0, ]$outcome, 1, 0),
  ifelse(df_long5[df_long5$outcome == 0, ]$qb != df_long5[df_long5$outcome == 0, ]$outcome, 1, 0)
)$p.value

# positive predictive values
df_long5 %>%
  filter(qp_b == 1) %>%
  summarise(mean(outcome))
df_long5 %>%
  filter(qb == 1) %>%
  summarise(mean(outcome))
t.test((df_long5 %>% filter(qp_b == 1))$outcome, (df_long5 %>% filter(qb == 1))$outcome)




# Analysis of risk estimates -> binary predictions in first part of the survey ----

# get data from first part of the survey
df_long_1 <- df_long %>% filter(type_answer == "h" & survey_part == 1)
# get data from second part of the survey, but only non-anchoring and without RAI
df_long_2h <- df_long %>% filter(type_answer == "h" & survey_part == 2)

#
# predicted re-arrest for each risk estimate
df_long_1 %>%
  group_by(qp) %>%
  summarise(qb = mean(qb)) %>%
  ggplot(aes(qp, qb)) +
  geom_point()
df_long_2h %>%
  group_by(qp) %>%
  summarise(qb = mean(qb)) %>%
  ggplot(aes(qp, qb)) +
  geom_point()
# predicted re-arrest for risk estimate >=40% and <50%
df_long_1 %>%
  filter(type_answer == "h") %>%
  filter(qp < 0.5 & qp >= 0.4) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
df_long_2h %>%
  filter(type_answer == "h") %>%
  filter(qp < 0.5 & qp >= 0.4) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# predicted re-arrest for risk estimate - 50%
df_long_1 %>%
  filter(type_answer == "h") %>%
  filter(qp == 0.5) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
df_long_2h %>%
  filter(type_answer == "h") %>%
  filter(qp == 0.5) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
# predicted no re-arrest for risk estimate >50% and <=60%
df_long_1 %>%
  filter(type_answer == "h") %>%
  filter(qp > 0.5 & qp <= 0.6) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))
df_long_2h %>%
  filter(type_answer == "h") %>%
  filter(qp > 0.5 & qp <= 0.6) %>%
  summarise(mean = mean(qb), se = round(sd(qb) / sqrt(n()), 3))


