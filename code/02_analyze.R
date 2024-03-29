# Author: Jerry Hong
# Disclaimer: Some or all the code below is through the assistance of AI generated sources like GitHub Copilot as well as outside internet resources

# load libraries
library(tidyverse)
library(estimatr)

# load data
penn <- read.table("https://psantanna.com/Econ520/files/penn_jae.dat", header=TRUE)

# merge treatment groups 4 and 6 into one group
penn_merge <- penn %>%
  mutate(tg = ifelse(tg %in% c(4, 6), 4, tg))

# subset data including the control group 0 and treatment group 4
penn_subset <- penn_merge %>%
  filter(tg %in% c(0, 4))

# get count of the number of observations in each group
penn_subset %>%
  group_by(tg) %>%
  summarize(n = n())
# there are 3354 observations in the control group and 3030 observations in the treatment group

# check for missing data
penn_subset %>%
  summarize_all(~sum(is.na(.)))
# we see there are no missing data

# creating the log_duration variable 
penn_subset <- penn_subset %>%
  mutate(log_duration = log(inuidur1))

# summary stats
penn_subset %>%
  group_by(tg) %>%
  summarize(count=n(),
            mean_inuidur1 = mean(inuidur1, na.rm = TRUE),
            mean_log_duration = mean(log_duration, na.rm = TRUE),
            median_inuidur1 = median(inuidur1, na.rm = TRUE),
            median_log_duration = median(log_duration, na.rm = TRUE),
            sd_inuidur1 = sd(inuidur1, na.rm = TRUE),
            sd_log_duration = sd(log_duration, na.rm = TRUE)) %>%
  write.csv('tables/tg_summary.csv')

# count table
penn_subset %>%
  group_by(tg) %>%
  summarize(female_count = sum(female),
            black_count = sum(black),
            othrace_count = sum(othrace),
            agelt35_count = sum(agelt35),
            agegt54_count = sum(agegt54)) %>%
  write.csv('tables/tg_count.csv')

# prop table for female
prop_female <- with(penn_subset, prop.table(table(tg, female), margin =2))

# prop table for black
prop_black <- with(penn_subset, prop.table(table(tg, black), margin =2))

# prop table for othrace
prop_othrace <- with(penn_subset, prop.table(table(tg, othrace), margin =2))

# prop table for agelt35
prop_agelt35 <- with(penn_subset, prop.table(table(tg, agelt35), margin =2))

# prop table for agegt54
prop_agegt54 <- with(penn_subset, prop.table(table(tg, agegt54), margin =2))

prop_df <- cbind(prop_female, prop_black, prop_othrace, prop_agelt35, prop_agegt54)

colnames(prop_df) <- c("Male", "Female",
                                    "Not Black", "Black",
                                    "Not Other Race", "Other Race",
                                    "Not Younger 35", "Younger 35",
                                    "Not Older 54", "Older 54")
row.names(prop_df) <- c("Control", "Treatment")

write.csv(prop_df, 'tables/tg_prop.csv')

# with each proportion table, the control has a slight majority in 
# all categories except being other race (othrace) where the 57% are in the treatment group

# plotting the two unemployment stints and save the plot
penn_subset %>%
  ggplot(aes(x = log_duration, fill = factor(tg))) +
  geom_density(alpha = 0.5) +
  labs(title = "Density plot of log duration by treatment group",
       x = "Log duration (weeks)",
       y = "Density") +
  scale_fill_discrete(name="Treatment Group",
                       labels=c("Control", "Treatment")) +
  theme_minimal() +
  ggsave('plots/log_duration_density.png', width = 8, height = 6)

# estimating average treatment effect on log_duration
summary(estimatr::lm_robust(log_duration ~ tg, data = penn_subset))

# point estimate: -0.07954
# with a p-value of 0.008627, it is statistically significant
# 95% CI: (-0.1389, -0.02019)

# create 'white' race variable
penn_subset <- penn_subset %>%
  mutate(white = ifelse(black == 0 & othrace == 0 & hispanic == 0, 1, 0))

# create 'btw35_54' age variable
penn_subset <- penn_subset %>%
  mutate(btw35_54 = ifelse(agelt35 == 0 & agegt54 == 0, 1, 0))

# ATE for the three race groups
summary(estimatr::lm_robust(log_duration ~ tg * white, data = penn_subset))
summary(estimatr::lm_robust(log_duration ~ tg * black, data = penn_subset))
summary(estimatr::lm_robust(log_duration ~ tg * othrace, data = penn_subset))

# ATE variation for race, setting othrace as reference
summary(estimatr::lm_robust(log_duration ~ tg*(white+black), data = penn_subset))

# ATE for the three age groups
summary(estimatr::lm_robust(log_duration ~ tg * agelt35, data = penn_subset))
summary(estimatr::lm_robust(log_duration ~ tg * agegt54, data = penn_subset))
summary(estimatr::lm_robust(log_duration ~ tg * btw35_54, data = penn_subset))

# ATE variation for age, setting btw35_54 as reference
summary(estimatr::lm_robust(log_duration ~ tg*(agelt35+agegt54), data = penn_subset))

# ATE variation for gender and race, setting othrace as reference
summary(estimatr::lm_robust(log_duration ~ tg + female*(white+black), data = penn_subset))
