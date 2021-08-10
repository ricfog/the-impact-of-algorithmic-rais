
# Load libraries and data -----

library(tidyverse)
library(here)
library(vroom)
library(xtable)
library(tidymodels)

df <- vroom(here('Data', 'Predictions.csv'))
perc <- vroom(here('Data', 'Perception.csv')) %>% inner_join(df %>% distinct(id, condition, setting), by = 'id')

# get all answers (h, hr, and r) in the same column but keep qp and qb in the same row
df_all <- df %>%
  select(qp_h, qp_hr, qp_r, setting, condition, outcome, order_question, survey_part, id) %>%
  pivot_longer(cols=c('qp_h', 'qp_hr', 'qp_r'), names_to = 'type', values_to = 'qp') %>%
  mutate(type = case_when(type == 'qp_h' ~ 'h', type == 'qp_hr' ~ 'hr', TRUE ~ 'r')) %>%
  bind_cols(
    df %>% select(qb_h, qb_hr, qb_r) %>%
      pivot_longer(cols=c('qb_h', 'qb_hr', 'qb_r'), names_to = 'type_answer', values_to = 'qb') %>%
      select(-type_answer)
  ) %>%
  drop_na()

## group and transform answers in perception questions
perc <- perc %>%
  mutate(humanaccuracy_2 = ifelse(grepl('25', humanaccuracy_2), NA, humanaccuracy_2)) %>%
  rowwise() %>%
  mutate_at(paste0('use_binary_', 2:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1])/as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(paste0('use_likelihood_', 2:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1])/as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(paste0('humanaccuracy_', 1:3), function(x) as.numeric(strsplit(x, "\\/")[[1]][1])/as.numeric(strsplit(x, "\\/")[[1]][2])) %>%
  mutate_at(c(paste0('confidence_', 1:3)), function(x) case_when(
              x == 'Moderately confident' | x == 'Extremely confident' ~ 'Confident',
              x == 'Not confident at all' | x == 'Slightly confident' ~ 'Not confident',
              TRUE ~ 'Somewhat confident')) %>%
  mutate_at(c(paste0('humanaccuracy_comparison_', 1:3)), function(x) case_when(
              x == "among the lowest accuracies (0-20%)" | x == "lower than most accuracies (21-40%)" ~ 'Lower',
              x == "approximately equal to the median accuracy (41-60%)" ~ 'Median',
              TRUE ~ 'Higher'))
  
# transform dataframe into long format
perc1 <- perc %>% select(matches('1'), id)
colnames(perc1) <- str_replace(colnames(perc1), '_1', '')
perc2 <- perc %>% select(matches('2'), id)
colnames(perc2) <- str_replace(colnames(perc2), '_2', '')
perc3 <- perc %>% select(matches('3'), id)
colnames(perc3) <- str_replace(colnames(perc3), '_3', '')
perc <- perc1 %>% mutate(set_q = 1) %>%
  bind_rows(perc2 %>% mutate(set_q = 2)) %>%
  bind_rows(perc3 %>% mutate(set_q = 3))


# Analysis of overall performance ----

# get classifier for which % of predicted positives = % of predicted positives by human
# for the % predicted positives by human choose the predictions of human+RAI
pos <- round(mean(df_all[(df_all$type=='hr' | df_all$type=='h'),]$qb), 3)
# find threshold for the tool
seq_th <- seq(0,1,by=0.01)
th <- seq_th[which(as.list(seq_th) %>% purrr::map(~ mean(df_all[df_all$type=='r',]$qp>.x[[1]])) %>%
  unlist()>=pos) %>% max()]
# create new binary predictions and attach to df_all
df_all_eq <- df_all %>%
  bind_rows(df_all %>% filter(type == 'r') %>% mutate(type = 're') %>% mutate(qb = ifelse(qp>=th, 1, 0)))
  

# get performance metrics
# % predicted positives
pos <- df_all_eq %>% group_by(type) %>% summarise(value = mean(qb), se = sd(qb)/sqrt(n()))
t.test(df_all_eq[df_all_eq$type=='hr',]$qb, df_all_eq[df_all_eq$type=='r',]$qb)

# accuracy
acc <- df_all_eq %>% group_by(type) %>% summarise(value = mean(qb == outcome), se = sd(qb == outcome)/sqrt(n()))
# H+RAI vs RAI
t.test(ifelse(df_all_eq[df_all_eq$type=='hr',]$outcome==df_all_eq[df_all_eq$type=='hr',]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='r',]$outcome==df_all_eq[df_all_eq$type=='r',]$qb, 1, 0))$p.value
# H+RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='hr',]$outcome==df_all_eq[df_all_eq$type=='hr',]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re',]$outcome==df_all_eq[df_all_eq$type=='re',]$qb, 1, 0))$p.value
# RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='r',]$outcome==df_all_eq[df_all_eq$type=='r',]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re',]$outcome==df_all_eq[df_all_eq$type=='re',]$qb, 1, 0))$p.value


# false positive rate
fpr <- df_all_eq %>% filter(outcome == 0) %>% group_by(type) %>% summarise(value = mean(qb != outcome), se = sd(qb != outcome)/sqrt(n()))
# RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='r' & df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='r' & df_all_eq$outcome==0,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re'& df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='re'  & df_all_eq$outcome==0,]$qb, 1, 0))$p.value
# H+RAI vs RAI
t.test(ifelse(df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==0,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='r'& df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='r'  & df_all_eq$outcome==0,]$qb, 1, 0))$p.value
# H+RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==0,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re'& df_all_eq$outcome==0,]$outcome!=df_all_eq[df_all_eq$type=='re'  & df_all_eq$outcome==0,]$qb, 1, 0))$p.value


# false negative rate
fnr <- df_all_eq %>% filter(outcome == 1) %>% group_by(type) %>% summarise(value = mean(qb != outcome), se = sd(qb != outcome)/sqrt(n()))
# RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='r' & df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='r' & df_all_eq$outcome==1,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re'& df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='re'  & df_all_eq$outcome==1,]$qb, 1, 0))$p.value
# P+RAI vs RAI
t.test(ifelse(df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==1,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='r'& df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='r'  & df_all_eq$outcome==1,]$qb, 1, 0))$p.value
# P+RAI vs RAIeq
t.test(ifelse(df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='hr' & df_all_eq$outcome==1,]$qb, 1, 0),
       ifelse(df_all_eq[df_all_eq$type=='re'& df_all_eq$outcome==1,]$outcome!=df_all_eq[df_all_eq$type=='re'  & df_all_eq$outcome==1,]$qb, 1, 0))$p.value


# positive predicted values
ppv <- df_all_eq %>% filter(qb == 1) %>% group_by(type) %>% summarise(value = mean(outcome), se = sd(outcome)/sqrt(n()))
# RAI vs RAIeq
t.test(df_all_eq[df_all_eq$type=='r' & df_all_eq$qb==1,]$outcome,
       df_all_eq[df_all_eq$type=='re' & df_all_eq$qb==1,]$outcome)$p.value
# P+RAI vs RAI
t.test(df_all_eq[df_all_eq$type=='hr' & df_all_eq$qb==1,]$outcome,
       df_all_eq[df_all_eq$type=='r' & df_all_eq$qb==1,]$outcome)$p.value
# H+RAI vs RAIeq
t.test(df_all_eq[df_all_eq$type=='hr' & df_all_eq$qb==1,]$outcome,
       df_all_eq[df_all_eq$type=='re' & df_all_eq$qb==1,]$outcome)$p.value


# bootstrap (CI)
auc <- as.list(1:300) %>% purrr::map(~ df_all_eq %>% group_by(type) %>% 
                                     sample_n(nrow(.), replace=T) %>%  # sampling with replacement
                                     group_by(type) %>% 
                                     group_modify(~roc_auc(., outcome %>% as.factor(), qp)) # compute AUC
                                     ) %>%
  bind_rows() %>% select(-.metric, -.estimator) %>% rename(value = .estimate) %>%
  group_by(type) %>% summarise(lb = quantile(value, probs=0.025), ub = quantile(value, probs=0.975), value = mean(value))

# gather all metrics together
metrics <- pos %>% mutate(metric = 'Fraction of predicted positives') %>%
  bind_rows(acc %>% mutate(metric = 'Accuracy')) %>%
  bind_rows(fpr %>% mutate(metric = 'False positive rate (FPR)')) %>%
  bind_rows(fnr %>% mutate(metric = 'False negative rate (FNR)')) %>%
  bind_rows(ppv %>% mutate(metric = 'Positive predicted value (PPV)')) %>%
  mutate(lb = value-1.96*se, ub = value+1.96*se) %>% select(-se) %>%
  bind_rows(auc %>% mutate(metric = 'Area under the curve (AUC)'))


# plot metrics
order_metrics <- unique(metrics$metric)
metrics %>%
  ggplot(aes(factor(metric, 
                    levels = order_metrics), 
             value, fill = factor(type, levels = c('h', 'hr', 're', 'r')))) + 
  geom_col(position = position_dodge2()) +
  geom_errorbar(aes(ymin = lb, ymax = ub), position = position_dodge2(width = 0.9, padding = .6)) +
   theme_bw() + xlab('Metric') +
  scale_y_continuous('Value (%)', limits = c(0,1), breaks = seq(0,1,by=0.25), labels = paste0(seq(0,1,by=0.25)*100, '%')) +
  scale_fill_manual(name = 'Prediction', labels = c(bquote(Q[P]), bquote(Q[P+RAI]), bquote(Q['RAI,eq']), bquote(Q[RAI])),
                    values = c("#999999", "#E69F00", "#56B4E9", "#009E73"))
ggsave(width = 12, height = 3, here('Plots', 'Performance.pdf'), device = cairo_pdf)


# % participants that performs better than the RAI and RAIeq overall
acc_each <- df_all_eq %>% filter(!(type == 'h' & survey_part == 2)) %>%
  filter(survey_part != 1) %>% # comment this line to compute accuracy overall
  mutate(type = ifelse(grepl('h', type), 'h', type)) %>% # merge h and hr
  group_by(id, type) %>% summarise(acc = mean(qb == outcome), n = n()) %>%
  pivot_wider(names_from = 'type', values_from = 'acc')
acc_each
# % better than RAI
mean(acc_each$h>=acc_each$r); sum(acc_each$h>=acc_each$r)
# better than RAIeq
mean(acc_each$h>=acc_each$re)


## Variation in accuracy throughout the survey ----

# % predicted positives and accuracy by order of question in first part
df_all %>% filter(order_question < 17 & type == 'h') %>%
  group_by(order_question<9) %>% 
  summarise(mean_qb = mean(qb),
            mean_qp = mean(qp),
            mean_outcome = mean(outcome), 
            n_questions = length(unique(order_question)), accuracy = mean(qb == outcome))
# % predicted positives
t.test((df_all %>% filter(order_question < 17 & type == 'h' & order_question < 9))$qb,
       (df_all %>% filter(order_question < 17 & type == 'h' & order_question >= 9))$qb)$p.value
# accuracy
t.test((df_all %>% filter(order_question < 17 & type == 'h' & order_question < 9) %>% mutate(acc = ifelse(qb==outcome, 1, 0)))$acc,
(df_all %>% filter(order_question < 17 & type == 'h' & order_question >= 9) %>% mutate(acc = ifelse(qb==outcome, 1, 0)))$acc)$p.value

# accuracy between first and second part
df_all %>% filter(type == 'h') %>% filter(setting == 'No anchor') %>% filter(condition != 'Decreasing') %>%
  group_by(survey_part) %>% summarise(accuracy = mean(qb == outcome), risk_estimate = mean(qp))


# Perceived vs real performance ----

# get accuracy for each part of the survey and participants
acc <- 
  # for 1st & 2nd set of perception questions 
  df_all %>% filter(type != 'r' & !(type == 'h' & survey_part == 2) & survey_part != 3) %>%
  filter(order_question < 31) %>%
  group_by(id, survey_part) %>% summarise(acc = mean(outcome == qb)) %>% mutate(set_q = ifelse(survey_part == 1, 1, 2)) %>%
  # for 3rd set of perception questions
  bind_rows(df_all %>% filter(type != 'r' & !(type == 'h' & survey_part == 2) & survey_part == 2) %>%
  group_by(id) %>% summarise(acc = mean(outcome == qb)) %>% mutate(set_q = 3)) %>% arrange(id) %>%
  select(-survey_part)
# create ranking
acc <- acc %>% group_by(set_q) %>% arrange(acc) %>%
  mutate(rank = 1-1:n()/n())

# merge accuracy and perception questions
ap <- perc %>% inner_join(acc, by = c('id', 'set_q'))

# correlation of real performance and perceived performance
ap %>% group_by(set_q) %>%
  summarise(
    mean_acc = mean(acc),
    mean_humanacc = mean(humanaccuracy),
    estimate_cor = cor.test(humanaccuracy, acc, method='spearman')$estimate,
    p.val_cor = cor.test(humanaccuracy, acc, method='spearman')$p.value)
# how far from truth?
ap %>% group_by(set_q) %>% summarise(perc_diff = mean(abs(acc-humanaccuracy)), diff_n_question = perc_diff*ifelse(set_q[1] == 1, 14, 25))
# is there consistent under/overestimation?
mean(ap[ap$set_q==1,]$humanaccuracy > ap[ap$set_q==1,]$acc)
mean(ap[ap$set_q==3,]$humanaccuracy > ap[ap$set_q==3,]$acc)


# perceived accuracy is higher is self-confidence is higher
ap %>% filter(set_q != 2) %>% group_by(set_q, confidence) %>% summarise(mean_hacc = mean(humanaccuracy), n = n()) %>% mutate(prop_n = round(n/sum(n),3))
ap %>% group_by(set_q) %>% summarise(p_val = (kruskal.test(humanaccuracy ~ as.factor(confidence))$p.value))
ap %>% filter(confidence != 'Somewhat confident' & set_q != 2) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(humanaccuracy ~ confidence)$p.value)
ap %>% filter(confidence != 'Confident' & set_q != 2) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(humanaccuracy ~ confidence)$p.value)
ap %>% filter(confidence != 'Not confident' & set_q != 2) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(humanaccuracy ~ confidence)$p.value)


# perceived ranking
ap %>% group_by(set_q, humanaccuracy_comparison) %>% summarise(n = n(), rank = mean(rank)) %>% mutate(prop_n = round(n/sum(n),3))
# correlation of perceived and real ranking - first part
ap %>% group_by(set_q) %>% summarise(p_val = (kruskal.test(rank ~ as.factor(humanaccuracy_comparison))$p.value))
ap %>% filter(humanaccuracy_comparison != 'Higher' & set_q == 1) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)
ap %>% filter(humanaccuracy_comparison != 'Lower'  & set_q == 1) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)
ap %>% filter(humanaccuracy_comparison != 'Median'  & set_q == 1) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)
# second part
ap %>% filter(humanaccuracy_comparison != 'Higher' & set_q == 3) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)
ap %>% filter(humanaccuracy_comparison != 'Lower'  & set_q == 3) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)
ap %>% filter(humanaccuracy_comparison != 'Median'  & set_q == 3) %>% group_by(set_q) %>% summarise(p_val = wilcox.test(rank ~ as.factor(humanaccuracy_comparison))$p.value)

