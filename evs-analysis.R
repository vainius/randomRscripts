library(tidyverse)
library(foreign)
library(ggrepel)
factorWithLabels <- function(x){
  labels <- attr(x, "value.labels")
  if (!is.null(labels)) {
    factor(x, levels = labels, labels = str_trim(names(labels)))
  } else {
    x
  }
}

# subset

evs_raw_dt <- read.spss("ZA7503_v3-0-0.sav", 
                        to.data.frame = TRUE, 
                        use.value.labels = FALSE)

evs_sub <- evs_raw_dt %>%
  select(A046:A049, 
         A173,
         B002:B008,
         D018:D026_05,
         D081,
         E032:E046,
         E059,
         E110:E111_01,
         E114:E119,
         E224:E233B,
         F118:F126,
         G007_01,
         G063:G257,
         H009:H011,
         S002EVS,
         S003)

# E110:E111_01 satisfaction with the system
# G007_01 trust other people in country
# G063:G257 how close do you feel
# S001, etc. - basic

write_rds(evs_sub, 'evs_sub.RDS')
evs_sub <- read_rds('evs_sub.RDS')

# replace category codes with their labels

recode_cols <- c('S002EVS', 'S003')
evs_sub <- evs_sub %>% mutate(across(all_of(recode_cols), factorWithLabels))

variable_labels <- attr(evs_sub, "variable.labels")
nice_names_df <- data.frame(
  original_col = names(variable_labels),
  question_full = variable_labels
) %>%
  mutate(question_full = gsub(": ", "\n", question_full)) %>%
  mutate(question_full = paste(original_col, question_full, sep = ': '))

q_vect <- names(evs_sub) %>% unique()
res_list <- list()
for (q in q_vect) {
  col_attr <- attr(evs_sub[[q]], "value.labels")
  if (!is.null(col_attr) && length(names(col_attr)) > 0) {
    res_list[[q]] <- data.frame(
      question = q,
      value_label = names(col_attr),
      value = col_attr
    )
  }
}
code_label_df <- do.call(rbind, res_list)

# check if non-missing

non_miss_dt <- evs_sub %>%
  group_by(S002EVS) %>%
  summarize(across(A046:H011, ~ mean(!is.na(.x)), .names = "{.col}"))

# keep only multi-year

non_miss_year_cnt <- non_miss_dt %>%
  select(-S002EVS) %>%
  summarize(across(everything(), ~ sum(.x > 0, na.rm = TRUE), .names = "{.col}"))

cols_to_drop <- names(non_miss_year_cnt)[sapply(non_miss_year_cnt, function(x) x <= 1)]

filtered_df <- evs_sub #%>%
  #select(-all_of(cols_to_drop))

# range

range_f <- function(x, ...){max(x, ...) - min(x, ...) + 1}

range_df <- filtered_df %>%
  summarise(across(A046:G007_01, range_f, na.rm = T))
cols_3cat <- names(range_df)[sapply(range_df, function(x) x == 3)]

code_label_df %>%
  filter(question %in% cols_3cat)

# recode 3-category cols

filtered_df <- filtered_df %>%
  mutate_at(c('B008', 'D024', 'E032'), function(x){if_else(x == 3, 1.5, x)}) %>%
  mutate_at(c('D023'), function(x){if_else(x == 2, 0.5, x)})

code_label_df <- code_label_df %>%
  mutate(value = if_else(question %in% c('B008', 'D024', 'E032') & value == 3, 1.5, value)) %>%
  mutate(value = if_else(question %in% c('D023') & value == 2, 0.5, value))

value_range_df <- code_label_df %>%
  group_by(question) %>%
  summarize(max_value = max(value, na.rm = T),
            min_value = min(value, na.rm = T))

# check mean and sd over-time

invert_scale_q <- c('D020', 'D026_05', 'D026', 'E039', 'E110', 'E111', 'E114', 'E115', 'E116', 'E117', 'G007_01', 'H009', 'H010', 'H011')

summary_dt <- filtered_df %>%
  group_by(S002EVS, S003) %>%
  summarise(across(everything(), 
                   list(val_mean = ~mean(.x, na.rm = TRUE),
                        val_sd = ~sd(.x, na.rm = TRUE)),
                   .names = "{.col}__{.fn}"),
            .groups = 'drop') %>%
  pivot_longer(cols = -c(S002EVS, S003), 
               names_to = "variable", 
               values_to = "value") %>%
  separate(col = "variable",
           into = c("question", "statistic"),
           sep = "__") %>%
  filter(!is.na(value) & is.finite(value)) %>%
  inner_join(value_range_df, by = join_by(question)) %>%
  mutate(value = (value - min_value)/(max_value - min_value)) %>%
  select(-max_value, -min_value) %>%
  mutate(value = if_else(question %in% invert_scale_q, 1 - value, value))

code_label_df <- code_label_df %>%
  group_by(question) %>%
  mutate(value_norm = (value - min(value))/(max(value) - min(value))) %>%
  mutate(value_norm = if_else(question %in% invert_scale_q, 1 - value_norm, value_norm))
  
summary_sub <- summary_dt %>%
  mutate(S002EVS = as.numeric(str_extract(S002EVS, "^[0-9]+"))) %>%
  inner_join(nice_names_df, by = join_by(question == original_col)) %>%
  filter(S002EVS >= 1990) %>%
  pivot_wider(names_from = statistic,
              values_from = value)
  
# 

muti_year_q <- summary_sub %>% 
  filter(S003 == 'Lithuania') %>% 
  group_by(question) %>%
  summarize(n_cnt = n()) %>%
  filter(n_cnt > 1) %>%
  .[['question']]

plot_sub <- summary_sub %>%
  filter(question %in% setdiff(muti_year_q, c('B002', 'B003'))) %>%
  mutate(question_full = ifelse(str_detect(question_full, "\n"),
                                str_split(question_full, "\n", simplify = TRUE)[, 2],
                                question_full)) %>%
  mutate(question_full = ifelse(str_detect(question_full, ": "),
                                str_split(question_full, ": ", simplify = TRUE)[, 2],
                                question_full))

quest_labels <- plot_sub %>%
  distinct(question, question_full) %>%
  inner_join(code_label_df, by = join_by(question)) %>%
  filter(!(value_label %in% c(1:9)))
quest_labels$S002EVS <- 1990

western_europe <- c('Austria', 'Belgium', 'Denmark', 'Finland', 'France', 'Germany', 'Great Britain', 
                    'Iceland', 'Ireland', 'Italy', 'Luxembourg', 'Malta', 'Netherlands', 'Northern Ireland', 
                    'Norway', 'Portugal', 'Spain', 'Sweden', 'Switzerland')
eastern_europe <- c('Albania', 'Armenia', 'Azerbaijan', 'Belarus', 'Bosnia and Herzegovina', 'Bulgaria', 
                    'Croatia', 'Czech Republic', 'Estonia', 'Georgia', 'Hungary', 'Kosovo', 
                    'Latvia', 'Lithuania', 'Moldova', 'Montenegro', 'North Macedonia', 'Poland', 
                    'Romania', 'Russia', 'Serbia', 'Slovakia', 'Slovenia', 'Turkey', 'Ukraine')

quest_avg <- plot_sub %>%
  mutate(region = case_when(S003 %in% western_europe ~ 'Western Europe',
                            S003 %in% eastern_europe ~ 'Eastern Europe')) %>%
  filter(!is.na(region)) %>%
  group_by(region, S002EVS, question, question_full) %>%
  summarize(overall_avg = median(val_mean, na.rm = T), .groups = 'drop')

plot_sub %>%
  filter(S002EVS >= 1990) %>%
  filter(S003 %in% c('Lithuania')) %>%
  filter(question %in% muti_year_q) %>%
  ggplot(aes(x = S002EVS, y = val_mean)) +
  geom_point(aes(shape = S003)) +
  geom_path(aes(group = S003, linetype = S003)) +
  geom_point(data = quest_avg, aes(shape = region, y = overall_avg)) +
  geom_path(data = quest_avg, aes(group = region, linetype = region, y = overall_avg), colour = 'blue') +
  geom_text(data = quest_labels, aes(label = value_label, y = value_norm), hjust = 0, size = 3) +
  facet_wrap(~question_full, scales = 'fixed') +
  theme(legend.position = 'bottom')

# Cross-country comparison for H009:H011

quest_sub_vect <- c('H009', 'H010', 'H011')

quest_labels_2 <- summary_sub %>%
  filter(question %in% quest_sub_vect) %>%
  distinct(question, question_full) %>%
  inner_join(code_label_df, by = join_by(question)) %>%
  filter(!(value_label %in% c(1:9)))

summary_sub %>%
  filter(S002EVS == 2017) %>%
  filter(question %in% quest_sub_vect) %>%
  ggplot(aes(x = 0, y = val_mean)) +
  geom_point() +
  facet_wrap(~question_full, ncol = 3) +
  geom_text(data = quest_labels_2, 
            aes(label = value_label, y = value_norm), hjust = 1, size = 3) +
  geom_text_repel(aes(label = S003), max.overlaps = 20)

label_dt <- summary_sub %>%
  filter(S002EVS == 2017) %>%
  filter(question %in% quest_sub_vect[3])
label_dt_2 <- summary_sub %>%
  filter(S002EVS == 2017) %>%
  filter(question %in% quest_sub_vect[1]) %>%
  filter(val_mean > 0.75 | val_mean < 0.35 | S003 == 'Lithuania')

plot_sub_2 <- summary_sub %>%
  filter(S002EVS == 2017) %>%
  filter(question %in% quest_sub_vect)

plot_sub_2 %>%
  ggplot(aes(x = question_full, y = val_mean)) +
  geom_point(aes(group = S003), size = 0.1) + 
  geom_line(aes(group = S003), size = 0.1) + 
  geom_line(data = filter(plot_sub_2, S003 == 'Lithuania'), aes(group = S003), size = 0.5, linetype = 'dashed') + 
  geom_text_repel(data = label_dt, aes(label = S003), max.overlaps = 20, size = 3.5) +  
  geom_text_repel(data = label_dt_2, aes(label = S003), max.overlaps = 20, size = 3.5) +  
  geom_text(data = filter(quest_labels_2, question == 'H010' & value_norm < 1), 
            aes(label = value_label, y = value_norm), hjust = 0.5, size = 3) +
  theme_minimal() +
  labs(x = "Question", y = "Survey response") +
  geom_hline(yintercept = 0.5, linetype = 'dotted') +
  ggtitle("European Values Study 2017-2021: Approval of Government Surveillance Measures") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
        legend.position = 'none')
