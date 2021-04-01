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

dir <- tempdir()
filename <- "tests_conducted_2021_04_01.ods"

url <- paste0("https://assets.publishing.service.gov.uk/government/uploads/",
              "system/uploads/attachment_data/file/975450/",
              filename)
download.file(url, file.path(dir, filename))

ed_settings <- read_ods(file.path(dir, filename),
                        sheet = "Table_6", skip = 2) %>%
  clean_names() %>%
  slice(1:20) %>%
  rename(name = na) %>%
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
  mutate(date = as.Date(sub("^x([0-9]{2})_([0-9]{2})_([0-9]{2}).*$",
                            "20\\3-\\2-\\1", date))) %>%
  pivot_wider(names_from = "test") %>%
  mutate(total = positive + negative)

schools <- read_ods(file.path(dir, filename),
                        sheet = "Table_7", skip = 2) %>%
  clean_names() %>%
  rename(name = na) %>%
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
  mutate(date = as.Date(sub("^x([0-9]{2})_([0-9]{2})_([0-9]{2}).*$",
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
  filter(date != "2020-12-24", date >= "2021-02-01")

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
  xlab("") +
  theme(legend.position = "bottom")

res <- estimate_min_specificity(dfb$positive, dfb$total,  samples = 10000)

dfe <- covidregionaldata::get_regional_data("UK") %>%
  mutate(date = floor_date(date, "week", 4)) %>%
  filter(region == "England", date %in% dfb$date) %>%
  group_by(date) %>%
  summarise(cases = sum(newCasesBySpecimenDate), .groups = "drop")

p_cases <- ggplot(dfe, aes(x = date, y = cases)) +
  geom_point() +
  geom_line() +
  xlab("") +
  scale_y_continuous("Number of cases", labels = scales::comma) +
  theme_minimal() +
  expand_limits(y = 0)

sums <- dfb %>%
  summarise_at(vars(positive, negative, total), sum)
