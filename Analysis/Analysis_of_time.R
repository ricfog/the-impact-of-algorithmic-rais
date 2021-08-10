
# Load libraries and data ----

library(tidyverse)
library(here)
library(vroom)
library(xtable)
library(tidymodels)
source(here("Analysis", "utils.R"))

df <- vroom(here("Data", "Predictions.csv"))
time <- vroom(here("Data", "Time_and_payment.csv"))
desc <- vroom(here("Data", "Defendants_descriptions.csv"))

# additional analysis
#demo <- vroom(here('Data', 'Demographics.csv'))
#df <- df %>% inner_join(demo %>% filter(ml_familiarity == 'Yes'))

# process data: get all answers (both h and hr) in the same column
# but keep qp and qb in the same row
df_long <- df %>%
  select(qp_hr, qb_hr, time_hr, qp_r, qb_r, id, 
         setting, condition, survey_part, outcome, order_question, index) %>%
  rename(qp = qp_hr, qb = qb_hr, time = time_hr) %>%
  mutate(type_answer = "hr") %>%
  bind_rows(
    df %>%
      select(qp_h, qb_h, time_h, qp_r, qb_r, id, 
             setting, condition, survey_part, outcome, order_question, index) %>%
      rename(qp = qp_h, qb = qb_h, time = time_h) %>%
      mutate(type_answer = "h")
  ) %>%
  filter(!is.na(qp))

# join descriptions with df long
desc <- desc %>% inner_join(df_long, by = "index")


# Analysis of time -------

# time spent mturk vs time spent on platform
time %>%
  mutate(survey_time = survey_time / 60, mturk_time = mturk_time / 60) %>%
  summarise(
    mean_survey = mean(survey_time), 
    mean_mturk = mean(mturk_time, na.rm = T), 
    sd_survey = sd(survey_time, na.rm = T), sd_mturk = sd(mturk_time, na.rm = T),
    ratio_per_participant = mean(mturk_time / survey_time, na.rm = T), 
    ratio_all = mean(mturk_time[!is.na(mturk_time)])/mean(survey_time[!is.na(mturk_time)]),
    median_survey = median(survey_time), 
    median_mturk = median(mturk_time, na.rm = T)
  )

# how much more time needed to complete the HIT
# 5 minutes
time %>%
  filter(!is.na(mturk_time)) %>%
  count(abs(mturk_time / 60 - survey_time / 60) < 5) %>%
  mutate(prop = round(n / sum(n), 3)) # 5
# 10 more minutes
time %>%
  filter(!is.na(mturk_time)) %>%
  count(abs(mturk_time / 60 - survey_time / 60) < 10) %>%
  mutate(prop = round(n / sum(n), 3)) # 10
# 20 more minutes
time %>%
  filter(!is.na(mturk_time)) %>%
  count(abs(mturk_time / 60 - survey_time / 60) < 20) %>%
  mutate(prop = round(n / sum(n), 3)) # 20

# plot the two times
p <- time %>%
  mutate(survey_time = survey_time / 60, mturk_time = mturk_time / 60) %>%
  ggplot(aes(survey_time, mturk_time)) +
  geom_jitter() +
  geom_abline(col = "red", linetype = "dashed") +
  geom_hline(col = "blue", linetype = "dotted", aes(yintercept = 90)) +
  theme_bw() +
  xlim(0, 90) +
  ylim(0, 90) +
  xlab("Time to complete survey (minutes)") +
  ylab("Time to complete HIT (minutes)")
p %>% set_theme_ggplot()
ggsave(width = 8, height = 4, here("Plots", "Time_mturk_vs_survey.pdf"), 
       device = cairo_pdf)


# time spent on each prediction
df_long %>% summarise(mean_time = mean(time),
                      median_time = median(time), 
                      sd_time = sd(time), n = n())
df_long %>%
  filter(!(survey_part == 2 & setting == "No anchor")) %>%
  summarise(mean_time = mean(time), 
            median_time = median(time), 
            sd_time = sd(time), n = n())
df_long %>%
  filter(survey_part == 2 & setting == "No anchor" & type_answer == "hr") %>%
  summarise(mean_time = mean(time), 
            median_time = median(time), 
            sd_time = sd(time), n = n())


# time spent on each question (cumulative): only for predictions in anchoring setting
cum_time <- df_long %>%
  #filter(survey_part == 2 & setting == "Anchor") %>%
  filter(setting == 'Anchor') %>%
  arrange(id, desc(time)) %>%
  group_by(id) %>%
  mutate(n = 1:n(), time_cum = cumsum(time) / sum(time)) %>%
  ungroup()
cum_time %>%
  filter(n == 1) %>%
  summarise(value = mean(time_cum), sd = sd(time_cum), 
            median = median(time_cum), n = n())
cum_time %>%
  filter(n <= 4) %>%
  summarise(value = mean(time_cum), sd = sd(time_cum), 
            median = median(time_cum), n = n())
# decrease in average time overall
cum_time %>%
  ungroup() %>%
  filter(n > 4) %>%
  summarise(value = mean(time), sd = sd(time), se = sd(time) / sqrt(n()))
# average decrease in average time per participant
cum_time %>%
  group_by(id) %>%
  mutate(mean_time = mean(time)) %>%
  filter(n > 4) %>%
  summarise(mean_time = mean_time[1], mean_time_sub = mean(time)) %>%
  ungroup() %>%
  summarise(mean_time_decrease = 1 - mean(mean_time_sub / mean_time), 
            sd = sd(mean_time_sub / mean_time))
# do they all split their time unevenly?
cum_time %>%
  group_by(id) %>%
  mutate(mean_time = mean(time)) %>%
  filter(n > 4) %>%
  summarise(mean_time_decrease = mean(mean_time) / mean(time)) %>%
  ggplot(aes(mean_time_decrease)) +
  geom_histogram()
cum_time[cum_time$id %in% sample(1:531, 100), ] %>%
  group_by(id) %>%
  ggplot(aes(id, time_cum)) +
  geom_point() +
  coord_flip()

# time spent on each question by order
# first part of the survey: first question vs other questions
df_long %>%
  filter(survey_part == 1) %>%
  group_by(order_question == 2) %>%
  summarise(mean_time = mean(time), 
            median_time = median(time), 
            se_time = sd(time) / sqrt(n()))
wilcox.test(df_long[df_long$survey_part == 1, ]$time ~ 
              ifelse(df_long[df_long$survey_part == 1, ]$order_question == 2, 1, 0))
# second part of the survey: first question vs other questions
df_long %>%
  filter(!(survey_part == 2 & setting == "No anchor")) %>%
  group_by(order_question == 17) %>%
  summarise(mean_time = mean(time), 
            median_time = median(time), 
            se_time = sd(time) / sqrt(n()))
wilcox.test(df_long[df_long$survey_part == 2, ]$time ~ 
              ifelse(df_long[df_long$survey_part == 2, ]$order_question == 17, 1, 0))


# allocation of time across sections
cum_time %>%
  filter(setting == "Anchor") %>%
  group_by(id) %>%
  mutate(total_time = sum(time / 60)) %>%
  group_by(id, survey_part) %>%
  summarise(time_part = sum(time / 60) / mean(total_time)) %>%
  ungroup() %>%
  group_by(survey_part) %>%
  summarise(mean_time_part = mean(time_part), 
            median_time_part = median(time_part), 
            sd = sd(time_part))


# Analysis of time vs. performance ----

# subset data of interest
tp <- df_long %>%
  filter(!(survey_part == 2 & setting == "No anchor")) %>%
  group_by(id) %>%
  mutate(mean_time_id = mean(time))
tp <- tp %>%
  arrange(id, time) %>%
  group_by(id) %>%
  mutate(rank = 1:n())
# correlation between time and accuracy on the survey
at <- tp %>%
  filter(setting == "Anchor") %>%
  group_by(id) %>%
  summarise(acc = mean(qb == outcome), total_time = sum(time / 60))
at %>% ggplot(aes(total_time, acc)) +
  geom_jitter()
cor.test(at$acc, at$total_time, method = "spearman")
# time spent on each assessment
tp %>%
  group_by(qb == outcome) %>%
  summarise(mean_time = mean(time), median_time = median(time))
tp %>%
  mutate(accurate = as.factor(ifelse(qb == outcome, 1, 0))) %>%
  ggplot(aes(time, fill = accurate)) +
  geom_density(alpha = 0.3) +
  xlim(0, 60)
t.test(tp[tp$qb == tp$outcome, ]$time, tp[tp$qb != tp$outcome, ]$time)
wilcox.test(tp[tp$qb == tp$outcome, ]$time, tp[tp$qb != tp$outcome, ]$time)
# ranking: if participants were to spend more time on the 
# questions for which they make accurate predictions, then the
# ranking of the accurate predictions should be higher (time was sorted 
# in increasing order above)
tp %>%
  group_by(qb == outcome) %>%
  summarise(mean_rank = mean(rank), median_rank = median(rank))


# get data of interest
desc <- desc %>%
  filter(setting == "Anchor") %>%
  mutate(n_char = nchar(description))
cor.test(desc$time, desc$n_char, method = "spearman")
# outcome and description
wilcox.test(desc$n_char ~ desc$outcome)
# accuracy and description
wilcox.test(desc$n_char ~ ifelse(desc$qb == desc$outcome, 1, 0))
# binned description
desc <- desc %>% mutate(n_char_bin = floor(n_char / 50))
# time for short descriptions
desc %>%
  group_by(n_char_bin) %>%
  mutate(n = n()) %>%
  filter(n > 200) %>%
  ggplot(aes(time, fill = factor(n_char_bin))) +
  geom_density(alpha = 0.3) +
  xlim(0, 60)


# plot the time spent on each assessment by workers
df_sorted <- df_long[df_long$id %in% sample(unique(df_long$id), 100), ] %>%
  #filter(survey_part == 2 & setting == "Anchor") %>%
  filter(setting == 'Anchor') %>%
  group_by(id) %>%
  mutate(median_time = median(time)) %>%
  arrange(median_time) %>%
  mutate(id = as.factor(id))
p <- df_sorted %>%
  ggplot(aes(factor(id, levels = unique(id)), time)) +
  geom_boxplot() +
  theme_bw() +
  ylim(0, 60) +
  xlab("Participant") +
  ylab("Time spent on assessment (seconds)")
p %>% set_theme_ggplot() + theme(axis.text.x = element_blank(), 
                                 axis.title.y = element_text(size = 15))
ggsave(width = 8, height = 4, here("Plots", "Time_per_assessment.pdf"), 
       device = cairo_pdf)
