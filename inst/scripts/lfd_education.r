library("readODS")
library("here")
library("dplyr")
library("janitor")
library("tidyr")
library("ggplot2")
library("scales")
library("binom")
library("lubridate")
library("covidregionaldata")
library("rvest")
library("covid19.lfd.education")

url <- paste0("https://www.gov.uk/government/collections/",
              "nhs-test-and-trace-statistics-england-weekly-reports")
session <- session(url)

weekly_url <- session %>%
  html_nodes(xpath = "//div/ul/li/a") %>%
  purrr::pluck(1) %>%
  html_attr("href")

latest <- session %>%
  session_jump_to(weekly_url)

url <- latest %>%
  html_nodes(xpath = "//div/h3/a") %>%
  html_attr("href") %>%
  grep(pattern = "tests_conducted", value = TRUE)

filename <- sub("^.*/([^/]+)$", "\\1", url)

dir <- tempdir()
download.file(url, file.path(dir, filename))

ed_settings <- read_ods(file.path(dir, filename),
                        sheet = "Table_7", skip = 2) %>%
  clean_names() %>%
  slice(1:20) %>%
  rename(name = x) %>%
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

schools <- read_ods(file.path(dir, filename),
                        sheet = "Table_8", skip = 2) %>%
  clean_names() %>%
  rename(name = x) %>%
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
  bind_rows(schools)

uncert <- binom.confint(df_all$positive, df_all$total, method = "exact") %>%
  select(mean, lower, upper)

dfb <- df_all %>%
  cbind(uncert) %>%
  filter(date != "2020-12-24", date >= "2021-02-01") %>%
  filter(!grepl("Higher|bubble", school))

p_testing <- ggplot(dfb,
                    aes(x = date, y = mean, colour = school,
                        ymin = lower, ymax = upper, fill = school)) +
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.35) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_fill_brewer("", palette = "Dark2") +
  theme_minimal() +
  expand_limits(y = 0) +
  scale_y_continuous("Proportion positive", labels = scales::percent) +
  xlab("Final Wednesday of week of data") +
  theme(legend.position = "bottom") +
  geom_vline(xintercept = as.Date("2021-03-08"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-03-31"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-04-19"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-05-29"), linetype = "dashed") +
  geom_vline(xintercept = as.Date("2021-06-06"), linetype = "dashed") +
  geom_rect(xmin = min(dfb$date), xmax = as.Date("2021-03-08"),
            ymin = 0, ymax = max(dfb$upper), fill = alpha("black", 0.008),
            colour = NA) +
  geom_rect(xmin = as.Date("2021-03-31"), xmax = as.Date("2021-04-19"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.008, fill = "black",
            colour = NA) +
  geom_rect(xmin = as.Date("2021-05-29"), xmax = as.Date("2021-06-06"),
            ymin = 0, ymax = max(dfb$upper), alpha = 0.008, fill = "black",
            colour = NA)



suppressWarnings(dir.create(here::here("figure")))
ggsave(here::here("figure", "lfd_testing.png"), p_testing, width = 10, height = 5)
