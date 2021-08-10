
# Load libraries and data -----

library(tidyverse)
library(here)
library(vroom)
library(tidymodels)
library(sandwich)
source(here("Analysis", "utils.R"))


df <- vroom(here("Data", "Predictions.csv"))
def <- vroom(here("Data", "Defendants_data_full.csv")) %>%
  mutate(prs = ifelse(is.na(prs), "RFEL", prs))


# get all answers (h, hr, and r) in the same column but keep qp and qb in the same row
df_all <- df %>%
  select(qp_h, qp_hr, qp_r, setting, condition, outcome, order_question, survey_part, id, index) %>%
  pivot_longer(cols = c("qp_h", "qp_hr", "qp_r"), names_to = "type", values_to = "qp") %>%
  mutate(type = case_when(type == "qp_h" ~ "h", type == "qp_hr" ~ "hr", TRUE ~ "r")) %>%
  bind_cols(
    df %>% select(qb_h, qb_hr, qb_r) %>%
      pivot_longer(cols = c("qb_h", "qb_hr", "qb_r"), names_to = "type_answer", values_to = "qb") %>%
      select(-type_answer)
  ) %>%
  drop_na()

# create outcome
def <- def %>%
  mutate(incar = ifelse(is.na(incar), 0, 1))

# join the datasets
df_all <- df_all %>%
  inner_join(def, by = "index")


## Analysis of judicial and participants' predictions ----

# prediction matrix for humans
pm <- df_all %>%
  mutate(type = ifelse(grepl("h", type), "h", type)) %>%
  filter(type != "r") %>%
  group_by(type, ogs, prs) %>%
  summarise(mean_qb = round(mean(qb), 2)) %>%
  arrange(desc(ogs)) %>%
  mutate(type = "Participants")
# plot it also with judicial decisions
p <- pm %>%
  bind_rows(vroom(here("Data", "Other", "Decision_matrix_full_RAI.csv")) %>% mutate(type = "RAI")) %>%
  bind_rows(vroom(here("Data", "Other", "Decision_matrix_full_decision.csv")) %>% mutate(type = "Judges") %>% rename(mean_qb = incar)) %>%
  filter(ogs <= 10) %>%
  ggplot(aes(prs, ogs, fill = mean_qb)) +
  geom_tile() +
  facet_grid(~ factor(type, levels = c("Judges", "RAI", "Participants"))) +
  theme_bw() +
  scale_y_continuous("Offense Gravity Score (OGS)", breaks = seq(0, 10), labels = paste0(seq(0, 10))) +
  xlab("Prior Record Score (PRS)") +
  scale_fill_gradientn(name = "Share of offenders \n incarcerated or \n with predictions\n of re-arrest", colours = c("white", "yellow", "red", "black"), limits = c(0, 1))
p %>% set_theme_ggplot()
ggsave(width = 12, height = 5, here("Plots", "Pred_vs_dec_matrix.pdf"), device = cairo_pdf)


# regression
# create data
df_reg <- df_all %>%
  mutate(type = ifelse(grepl("h", type), "h", type)) %>%
  mutate(prs = as.numeric(ifelse(prs == "RFEL", 6, prs)))
# first change RAI's predictions to match the rate of predicted positives
pos <- mean((df_all %>% distinct(index, incar))$incar)
# find threshold for the tool
seq_th <- seq(0, 1, by = 0.01)
th <- seq_th[which(as.list(seq_th) %>% purrr::map(~ mean((df_all %>% filter(type == "r"))$qp > .x[[1]])) %>% unlist() >= pos) %>% max()]
df_reg <- df_reg %>% mutate(qb = ifelse(type == "r", ifelse(qp > th, 1, 0), qb))
# fit models
# recidivism - human
mod_fit <- glm(qb ~ prs + ogs, df_reg %>% filter(type == "h"), family = binomial())
sand <- sandwich(mod_fit)
mh <- tidy(mod_fit) %>% 
  select(term, estimate) %>%
  inner_join(tibble(term = colnames(sand), std.error = sqrt(diag(sand)))) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(estimate / std.error)))) %>%
  mutate(type = "Human")
# recidivism - rai
mod_fit <- glm(qb ~ prs + ogs, df_reg %>% filter(type == "r"), family = binomial())
mr <- tidy(mod_fit) %>% 
  select(term, estimate) %>%
  inner_join(tibble(term = colnames(sand), std.error = sqrt(diag(sand)))) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(estimate / std.error)))) %>%
  mutate(type = "RAI")
# incarceration - judges
mod_fit <- glm(incar ~ prs + ogs, df_reg %>% distinct(index, incar, prs, ogs), family = binomial())
mi <- tidy(mod_fit) %>% 
  select(term, estimate) %>%
  inner_join(tibble(term = colnames(sand), std.error = sqrt(diag(sand)))) %>%
  mutate(p.value = 2 * (1 - pnorm(abs(estimate / std.error)))) %>%
  mutate(type = "Judge")
coefs <- mh %>%
  bind_rows(mr) %>%
  bind_rows(mi)
coefs %>%
  select(-type) %>%
  xtable() %>%
  print(include.rownames = FALSE)

# type of charge and mean
df_reg %>%
  group_by(crimecat) %>%
  distinct(index, outcome, incar) %>%
  summarise(mean_outcome = mean(outcome), mean_incar = mean(incar), n = n())
df_reg %>%
  filter(type == "h") %>%
  group_by(crimecat) %>%
  summarise(mean_qb = mean(qb))
df_reg %>%
  filter(type == "r") %>%
  group_by(crimecat) %>%
  summarise(mean_qb = mean(qb))
