library("ggplot2")
library("here")
library("dplyr")
library("tidyr")
library("socialmixr")
library("scales")

df <- readRDS(here::here("data", "cis_age.rds")) %>%
  mutate(age_group = limits_to_agegroups(lower_age_limit),
         date = start_date + (end_date - start_date) / 2) %>%
  pivot_longer(starts_with("proportion")) %>%
  group_by(date, age_group, name) %>%
  summarise(value = median(value), .groups = "drop") %>%
  pivot_wider()

p <- ggplot(df, aes(x = date, y = proportion_pos,
               ymin = proportion_pos_low_95,
               ymax = proportion_pos_high_95,
               colour = age_group,
               fill = age_group)) +
  geom_line() +
  geom_ribbon(colour = NA, alpha = 0.35) +
  xlab("") +
  scale_y_continuous("Proportion positive",
                     labels = percent_format(accuracy = 1L)) +
  theme_minimal() +
  scale_colour_brewer("Age group", palette = "Dark2") +
  scale_fill_brewer("Age group", palette = "Dark2")
ggsave(here::here("figure", "cis_age.pdf"), p)

cis <- readRDS(here::here("data", "cis.rds"))

cis_eng <- cis %>%
  filter(level == "national",
         geography == "England") %>%
  pivot_longer(starts_with("proportion")) %>%
  group_by(date, name) %>%
  summarise(value = median(value), .groups = "drop") %>%
  pivot_wider()
ggplot(cis_eng, aes(x = date,
                    y = proportion_pos,
                    ymin = proportion_pos_low_95,
                    ymax = proportion_pos_high_95)) +
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  xlab("") +
  ylab("Proportion positive") +
  theme_minimal()

cis_regions <- cis %>%
  filter(level == "regional") %>%
  pivot_longer(starts_with("proportion")) %>%
  group_by(date, geography, name) %>%
  summarise(value = median(value), .groups = "drop") %>%
  pivot_wider()

ggplot(cis_regions, aes(x = date, y = proportion_pos,
                        ymin = proportion_pos_low_95,
                        ymax = proportion_pos_high_95)) +
  geom_line() +
  geom_ribbon(alpha = 0.25) +
  xlab("") +
  ylab("Proportion positive") +
  theme_minimal() +
  facet_wrap(~ geography)
