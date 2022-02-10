library("readODS")
library("here")
library("dplyr")
library("janitor")
library("tidyr")
library("ggplot2")
library("scales")
library("binom")
library("lubridate")
library("rvest")
library("ggrepel")
library("viridis")
library("purrr")
library("sf")

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
  grep(pattern = "tests.conducted", value = TRUE, ignore.case = TRUE)

filename <- sub("^.*/([^/]+)$", "\\1", url)

dir <- tempdir()
download.file(url, file.path(dir, filename))

ltlas <- read_ods(file.path(dir, filename), sheet = "Table_5")
header_row <- which(ltlas[, 1] == "LTLA")

ltlas <- ltlas %>%
  row_to_names(header_row) %>%
  as_tibble() %>%
  clean_names() %>%
  select(-total) %>%
  mutate(test_result = sub("Total number of (positive|negative) LFD tests",
                           "\\1", test_result)) %>%
  filter(grepl("(positive|negative)", test_result)) %>%
  pivot_longer(names_to = "date", starts_with("x")) %>%
  mutate(value = as.integer(value)) %>%
  filter(!is.na(value)) %>%
  mutate(date = as.Date(sub("^.+([0-9]{2})_([0-9]{2})_([0-9]{2})$",
                            "20\\3-\\2-\\1", date))) %>%
  pivot_wider(names_from = "test_result") %>%
  mutate(total = positive + negative) %>%
  filter(!is.na(total))

uncert <- binom.confint(ltlas$positive, ltlas$total, method = "exact") %>%
  select(mean, lower, upper)

df <- ltlas %>%
  bind_cols(uncert) %>%
  filter(date != "2020-12-24", date >= "2021-02-01")

wr_latest <- df %>%
  mutate(rel_error = (abs(lower - mean) + abs(upper - mean)) / (2 * mean)) %>%
  select(date, ltla, estimate = mean, rel_error)

wr_previous <- wr_latest %>%
  mutate(date = date + 7)

wr <- wr_latest %>%
  inner_join(wr_previous, by = c("date", "ltla"),
             suffix = c("", "_previous")) %>%
  mutate(ratio = estimate / estimate_previous,
         rel_error = rel_error + rel_error_previous,
         lower_ratio = pmax(0, ratio - rel_error * ratio),
         upper_ratio = ratio + rel_error * ratio) %>%
  select(date, ltla, ends_with("ratio"))

inc_r <- df %>%
  inner_join(wr, by = c("ltla", "date")) %>%
  mutate(label = if_else(ltla_name %in% c("Bolton", "Blackburn with Darwen",
                                          "Liverpool", "Tower Hamlets"),
                         ltla_name, NA_character_))

latest <- inc_r %>%
  filter(date > max(date) - 7) %>%
  mutate(label = if_else(ratio > 2 | mean > 0.01,
                         ltla_name, NA_character_))

p <- ggplot(latest, aes(y = mean, x = ratio, colour = region_name)) +
  geom_point() +
  geom_errorbarh(alpha = 0.15, aes(xmin = lower_ratio, xmax = upper_ratio)) +
  geom_errorbar(alpha = 0.15, aes(ymin = lower, ymax = upper)) +
  geom_text_repel(aes(label = label), show.legend = FALSE) +
  scale_y_continuous("LFD positive prevalence", labels = scales::percent_format(.5)) +
  xlab("Weekly relative growth") +
  theme_bw() +
  coord_cartesian(xlim = c(0, ceiling(max(latest$ratio)) + 1)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  scale_color_brewer("Region", palette = "Paired")

suppressWarnings(dir.create(here::here("figure")))
ggsave(here::here("figure", "lfd_prev_growth.svg"), p, width = 11, height = 7)

labels <- inc_r %>%
  filter(date == max(date)) %>%
  arrange(desc(mean)) %>%
  head(n = 20) %>%
  mutate(date = date + 2)

last_10_weeks <- inc_r %>%
  filter(date > max(date) - weeks(10)) %>%
  mutate(label = if_else(ltla %in% labels$ltla & date == max(date),
                         ltla_name, NA_character_))

p <- ggplot(last_10_weeks, aes(x = date, y = mean,
                               colour = region_name,
                               group = ltla)) +
  geom_point() +
  geom_line(alpha = 0.2) +
  scale_colour_brewer("", palette = "Set1") +
  theme_bw() +
  xlab("") +
  expand_limits(x = max(last_10_weeks$date + 7), y = 0) +
  scale_y_continuous("LFD prevalence", labels = scales::label_percent()) +
  theme(legend.position = "bottom") +
  geom_text_repel(aes(label = label), show.legend = FALSE)

ggsave(here::here("figure", "lfd_last_10_weeks.svg"), p, width = 10, height = 6)

p <- ggplot(last_10_weeks, aes(x = date, y = mean,
                               colour = region_name,
                               group = ltla)) +
  geom_point() +
  geom_line(colour = "black", alpha = 0.2) +
  scale_colour_brewer("", palette = "Set1") +
  theme_bw() +
  xlab("") +
  expand_limits(x = max(last_10_weeks$date + 7), y = 0) +
  scale_y_continuous("LFD prevalence", labels = scales::label_percent()) +
  theme(legend.position = "bottom") +
  geom_text_repel(aes(label = label), show.legend = FALSE) +
  facet_wrap(~ region_name)

ggsave(here::here("figure", "lfd_last_10_weeks_regions.svg"), p, width = 12, height = 10)

england_ltla_shape <- st_read(here::here("data", "Local_Authority_Districts_(December_2021)_UK_BUC")) %>%
  rename(geo_code = LAD21CD) %>%
  filter(grepl("^E", geo_code))

all_ltlas_dates <-
  expand_grid(ltla = unique(england_ltla_shape$geo_code),
              date = unique(last_10_weeks$date))

last_10_weeks_all <- last_10_weeks %>%
  right_join(all_ltlas_dates, by = c("ltla", "date")) %>%
  replace_na(list(mean = 0))

map <- england_ltla_shape %>%
  inner_join(last_10_weeks_all %>%
            rename(geo_code = ltla), by = "geo_code")

p <- ggplot(map, aes(x = LAT, y = LONG, fill = mean)) +
  geom_sf(colour = NA) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
	plot.background = element_rect(fill = "white", colour = "white")) + 
  scale_fill_viridis("LFD prevalence", labels = scales::label_percent()) +
  facet_wrap( ~ date, nrow = 1)

ggsave(here::here("figure", "lfd_last_10_weeks_maps.svg"), p, width = 12, height = 4)

p_testing <- ggplot(df, aes(x = date, y = mean,
                        ymin = lower, ymax = upper)) +
  geom_point() +
  geom_line() +
  geom_ribbon(alpha = 0.35) +
  scale_colour_brewer("", palette = "Dark2") +
  scale_fill_brewer("", palette = "Dark2") +
  theme_bw() +
  expand_limits(y = 0) +
  scale_y_continuous("Proportion positive", labels = scales::percent) +
  facet_wrap(~ltla_name) +
  xlab("Final Wednesday of week of data")

ggsave(here::here("figure", "lfd_ltla.svg"), p_testing, width = 25, height = 25)
