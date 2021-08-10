
# Load libraries and data -----

## libraries
library(tidyverse)
library(here)
library(vroom)


## laod data
pred <- vroom(here("Data", "Predictions.csv"))
def <- vroom(here("Data", "Defendants_data.csv")) %>%
  inner_join(pred %>% distinct(index), by = "index")
tp <- vroom(here("Data", "Time_and_payment.csv"))
demo <- vroom(here("Data", "Demographics.csv"))


# Time and payments ----

# unique participants
length(unique(pred$id))

# total predictions
sum(!is.na(pred$qp_hr)) + sum(!is.na(pred$qp_h))

# total time spent on the survey
tp %>%
  summarise(
    mean = mean(survey_time / 60),
    sd = sd(survey_time / 60),
    median = median(survey_time / 60)
  )
# bonus
tp %>%
  summarise(
    mean = mean(bonus),
    sd = sd(bonus),
    median = median(bonus)
  )
# salary (hourly)
tp %>%
  mutate(salary = (bonus + 1.5) * 60^2 / survey_time) %>%
  summarise(
    mean = mean(salary),
    sd = sd(salary),
    median = median(salary)
  )

# time spent on each prediction
time_long <- pred %>%
  select(condition, setting, id, time_hr, time_h) %>%
  pivot_longer(cols = c("time_hr", "time_h"), names_to = "time", values_to = "value") %>%
  filter(!is.na(value))

time_long %>%
  summarise(
    mean = mean(value),
    sd = sd(value),
    median = median(value)
  )

# time spent in total for each setting
time_long %>%
  group_by(setting, id) %>%
  summarise(total_time = sum(value)) %>%
  group_by(setting) %>%
  summarise(
    mean = mean(total_time) / 60,
    sd = sd(total_time) / 60,
    median = median(total_time) / 60
  )


# Demographics -----

# age and binned age
demo %>% ggplot(aes(age)) +
  geom_histogram()
demo %>% summarise(mean = mean(age), sd = sd(age), median = median(age))
age_bin <- function(x) {
  if (x < 18) x <- "<18"
  if (x >= 18 & x <= 24) x <- "18-24"
  if (x >= 25 & x <= 34) x <- "25-34"
  if (x >= 35 & x <= 59) x <- "35-59"
  if (x >= 60 & x <= 78) x <- "60-78"
  if (x > 78) x <- ">79"
  x
}
demo %>%
  mutate(age_bin = case_when(
    age < 18 ~ "<18",
    age >= 18 & age <= 24 ~ "18-24",
    age >= 25 & age <= 34 ~ "25-34",
    age >= 35 & age <= 59 ~ "35-59",
    age >= 60 & age <= 78 ~ "60-78",
    TRUE ~ "79+"
  )) %>%
  group_by(age_bin) %>%
  summarise(n = n()) %>%
  mutate(prop = round(n / sum(n), 2))

# gender and race/ethnicity
demo %>%
  count(gender) %>%
  mutate(prop = round(n / sum(n), 2))
demo %>%
  count(race) %>%
  mutate(prop = round(n / sum(n), 2))

# process education
demo %>%
  filter(age >= 25) %>%
  count(education) %>%
  mutate(prop = round(n / sum(n), 2))
demo %>%
  filter(age >= 25) %>%
  mutate(college_degree_or_more = ifelse(grepl("Associate|Bachelor|Graduate", education), 1, 0)) %>%
  summarise(
    mean = mean(college_degree_or_more),
    n = sum(college_degree_or_more),
    total = n()
  )

# distribution by US region
northeast <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont", "New York", "New Jersey", "Pennsylvania")
midwest <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin", "Iowa", "Kansas", "Minnesota", "Missouri", "Nebraska", "North Dakota", "South Dakota")
west <- c("Arizona", "Colorado", "Idaho", "Montana", "Nevada", "New Mexico", "Utah", "Wyoming", "Alaska", "California", "Hawaii", "Oregon", "Washington")
sum(demo$location %in% northeast)
sum(demo$location %in% west)
sum(demo$location %in% midwest)
sum(demo$location %in% (setdiff(demo$location, c(northeast, midwest, west))))

# familiarity with use of tools
demo %>%
  count(ml_familiarity) %>%
  mutate(prop = round(n / sum(n), 2))



## General stats regarding predictions ----

# total number of predictions
sum(!is.na(pred$qb_h)) + sum(!is.na(pred$qb_hr))

# unique defendants
length(unique(def$index))

# anchoring vs non anchoring
pred %>%
  distinct(id, setting) %>%
  count(setting) %>%
  mutate(prop = round(n / sum(n), 3))

# time spent on each prediction
time_long <- pred %>%
  select(setting, id, time_h, time_hr) %>%
  pivot_longer(cols = c("time_h", "time_hr"), names_to = "type", values_to = "value") %>%
  filter(!is.na(value))
time_long %>% summarise(mean = mean(value), sd = sd(value), median = median(value))

# total time spent on the 40 predictions
time_long %>%
  group_by(setting, id) %>%
  summarise(total = sum(value) / 60) %>%
  group_by(setting) %>%
  summarise(mean = mean(total), sd = sd(total), median = median(total))

# base rate in sample
round(mean(def$y), 3)

# stats on predictions
pred_long <- pred %>%
  select(setting, id, qp_hr, qp_h, qp_r, qb_h, qb_hr) %>%
  pivot_longer(cols = c("qp_hr", "qp_h", "qp_r", "qb_h", "qb_hr"), names_to = "type", values_to = "value") %>%
  filter(!is.na(value))
# human vs RAI risk estimate
pred_long %>%
  filter(type == "qp_hr" | type == "qp_h") %>%
  summarise(mean = mean(value), median = median(value))
pred_long %>%
  filter(type == "qp_r") %>%
  summarise(mean = mean(value), median = median(value))
# human vs RAI binary prediction
pred_long %>%
  filter(type == "qb_hr" | type == "qb_h") %>%
  summarise(mean = mean(value))
pred_long %>%
  filter(type == "qp_r") %>%
  summarise(mean = mean(value > 0.5))
