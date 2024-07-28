# Data source:
# https://www.scribd.com/document/754066723/Topline-ABC-Ipsos-Poll-July-28-2024
# GPT query: "Combine these into a table with: candidate, poll date, favorable, unfavorable"

library(tidyverse)
library(ggrepel)
library(glue)
library(scales)

poll_dt <- read_csv("Candidate_Poll_Data.csv")

poll_prep <- poll_dt %>%
  rename(poll_date = `Poll Date`) %>%
  mutate(poll_date = gsub("-[^,]*,", "", poll_date)) %>%
  mutate(poll_date = strptime(poll_date, format = "%B %d %Y")) %>%
  mutate(poll_date = as.Date(poll_date)) %>% 
  pivot_longer(cols = c(Favorable, Unfavorable), names_to = "rating_type", values_to = "value")

leftmost_points <- poll_prep %>%
  group_by(Candidate, rating_type) %>%
  filter(poll_date == min(poll_date))

plot_var <- 'Favorable'
sampl_error <- 3

unique_dates <- range(unique(poll_prep$poll_date))

event_dt <- data.frame(
  event = c('debate', 'Trump shot', 'Biden out'),
  date = as.Date(c('2024-06-27', '2024-07-13', '2024-07-21')),
  value = 50
)

poll_prep %>%
  filter(rating_type == plot_var) %>%
  ggplot(aes(x = poll_date, y = value, color = Candidate)) +
  geom_line() +
  geom_point() +
  labs(x = "Poll Date",
       y = glue("{plot_var} Rating")) +
  theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "none") +
  geom_text_repel(data = filter(leftmost_points, rating_type == plot_var), aes(label = Candidate), hjust = 0) +
  scale_x_date(breaks = unique_dates,
               minor_breaks = unique_dates, 
               date_labels = "%m-%d") +
  ggtitle(glue("{plot_var} ratings\nABC News/Ipsos poll\nSurvey of the American general population. Error: ±3 p.p.")) +
  scale_y_continuous(minor_breaks = seq(0, 100, by = 1)) +
  #geom_errorbar(aes(ymin = value - sampl_error, ymax = value + sampl_error), 
  #              width = 2, alpha = 0.5, linetype = "dashed") +
  geom_vline(data = event_dt, aes(xintercept = date), linetype = "dotted", color = "black") +
  geom_text(data = event_dt, aes(x = date, y = value, label = event), angle = 90, vjust = -0.5, hjust = 1, size = 3, color = "black")
