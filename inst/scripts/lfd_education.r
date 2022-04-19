library("readxl")
library("here")
library("dplyr")
library("janitor")
library("tidyr")
library("ggplot2")
library("scales")
library("binom")
library("lubridate")
library("rvest")
library("purrr")

url <- paste0("https://www.gov.uk/government/collections/",
              "nhs-test-and-trace-statistics-england-weekly-reports")
session <- session(url)

weekly_url <- session %>%
  html_nodes(xpath = "//div/ul/li/a") %>%
  html_attr("href") %>%
  grep("weekly-statistics", ., value = TRUE) %>%
  pluck(1)

latest <- session %>%
  session_jump_to(weekly_url)

url <- latest %>%
  html_nodes(xpath = "//div/h3/a") %>%
  html_attr("href") %>%
  grep(pattern = "tests.reported", value = TRUE, ignore.case = TRUE)

filename <- sub("^.*/([^/]+)$", "\\1", url)

dir <- tempdir()
download.file(url, file.path(dir, filename))

ed_settings <- read_excel(file.path(dir, filename), sheet = "Table_7")
header_row <- which(ed_settings[, 1] == "LFD testing in education")

ed_settings <- ed_settings %>%
  row_to_names(header_row) %>%
  clean_names() %>%
  slice(1:20) %>%
  rename(name = lfd_testing_in_education) %>%
  select(-total) %>%
  mutate_if(is.numeric, as.character) %>%
  mutate(test = sub("Total number of (positive|negative) LFD tests", "\\1",
                    name)) %>%
  filter(grepl("(positive|negative)", test)) %>%
  mutate(school = c(rep("Nurseries and primary schools", 2),
                    rep("Secondary / College registered", 2),
                    rep("Secondary / College unregistered", 2),
                    rep("Unidentified", 2),
                    rep("Higher Education", 2))) %>%
  select(-name) %>%
  filter(school != "Unidentified") %>%
  pivot_longer(names_to = "date", starts_with("x")) %>%
  mutate(value = as.integer(value)) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(sub("^.+([0-9]{2})_([0-9]{2})_([0-9]{2})$",
                            "20\\3-\\2-\\1", date))) %>%
  pivot_wider(names_from = "test") %>%
  mutate(total = positive + negative)

schools <- read_excel(file.path(dir, filename), sheet = "Table_8") %>%
  row_to_names(header_row) %>%
  clean_names() %>%
  rename(name = lfd_testing_in_education_by_role) %>%
  select(-total) %>%
  mutate_if(is.numeric, as.character) %>%
  mutate(test = sub("Total number of (positive|negative) LFD tests", "\\1",
                    name)) %>%
  filter(grepl("(positive|negative)", test)) %>%
  mutate(school = c(rep("Primary school staff", 2),
                    rep("Primary school household", 2),
                    rep("Primary school bubble", 2),
                    rep("Secondary school students", 2),
                    rep("Secondary school staff", 2),
                    rep("Secondary school household", 2),
                    rep("Secondary school bubble", 2))) %>%
  select(-name) %>%
  pivot_longer(names_to = "date", starts_with("x")) %>%
  mutate(value = as.integer(value)) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(sub("^.+([0-9]{2})_([0-9]{2})_([0-9]{2})$",
                            "20\\3-\\2-\\1", date))) %>%
  pivot_wider(names_from = "test") %>%
  mutate(total = positive + negative)

df_all <- ed_settings %>%
  filter(school == "Higher Education") %>%
  bind_rows(schools) %>%
  filter(!is.na(total))

uncert <- binom.confint(df_all$positive, df_all$total, method = "exact") %>%
  select(mean, lower, upper)

dfb <- df_all %>%
  cbind(uncert) %>%
  filter(date != "2020-12-24", date >= "2021-02-01") %>%
  filter(!grepl("Higher|bubble", school))

p_testing <- ggplot(dfb,
                    aes(x = date, y = mean, colour = school,
                        ymin = lower, ymax = upper, fill = school)) +
 geom_vline(xintercept = as.Date("2021-03-08"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-03-31"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-04-19"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-05-29"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-06-06"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-07-23"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-09-01"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-10-23"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-10-31"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-12-18"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2022-01-04"), linetype = "dashed") +
  geom_rect(xmin = min(dfb$date), xmax = as.Date("2021-03-08"),
            ymin = 0, ymax = max(dfb$upper), fill = alpha("grey", 0.01),
            colour = NA) +
  geom_rect(xmin = as.Date("2021-03-31"), xmax = as.Date("2021-04-19"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.01, fill = "grey",
            colour = NA) +
  geom_rect(xmin = as.Date("2021-05-29"), xmax = as.Date("2021-06-06"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.01, fill = "grey",
            colour = NA) +
  geom_rect(xmin = as.Date("2021-07-23"), xmax = as.Date("2021-09-01"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.01, fill = "grey",
            colour = NA) +
  geom_rect(xmin = as.Date("2021-10-23"), xmax = as.Date("2021-10-31"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.01, fill = "grey",
            colour = NA) +
  geom_rect(xmin = as.Date("2021-12-18"), xmax = as.Date("2022-01-05"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.01, fill = "grey",
            colour = NA) +
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.35) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_fill_brewer("", palette = "Dark2") +
  theme_bw() +
  expand_limits(y = 0) +
  scale_y_continuous("Proportion positive", labels = scales::percent) +
  xlab("Final Wednesday of week of data") +
  theme(legend.position = "bottom")

suppressWarnings(dir.create(here::here("figure")))
ggsave(here::here("figure", "lfd_testing.svg"), p_testing, width = 10, height = 5)
