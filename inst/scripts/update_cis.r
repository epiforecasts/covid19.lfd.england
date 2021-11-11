library("here")
library("readxl")
library("rvest")
library("tidyr")
library("dplyr")
library("lubridate")
library("ggplot2")
library("purrr")
library("janitor")
library("lubridate")

## create directory for CIS if it doesn't exist
cis_dir <- here::here("data", "cis")
dir.create(cis_dir, showWarnings = FALSE, recursive = TRUE)

## creata URLs that list spreadsheets
years <- c(2020 + seq(0, 1))
urls <- paste0("https://www.ons.gov.uk/peoplepopulationandcommunity/",
              "healthandsocialcare/conditionsanddiseases/datasets/",
              "coronaviruscovid19infectionsurveydata/", years)

## get URLs of the spreadsheets, scraped from the web pages
file_urls <- lapply(urls, function(url) {
  session <- session(url)
  file_url <- session %>%
    html_nodes(xpath = paste0(
                 "//a[contains(concat(' ', ",
                 "normalize-space(@class),' '),' btn--primary ')]"
               )) %>%
    html_attr("href")
  return(file_url)
}) %>%
  unlist() %>%
  grep("\\.xlsx?$", value = TRUE, .)

## construct tibble with files to download
df_dl <- tibble(file_url = file_urls) %>%
  mutate(file_name = sub("^.*/([^/]+)$", "\\1", file_url),
         file_path = file.path(cis_dir, file_name),
         full_url = paste0("https://www.ons.gov.uk", file_url)) %>%
  filter(!file.exists(file_path))

## if no new URLs there is nothing to do
if (nrow(df_dl) > 0) {
  df_dl %>%
    rowwise() %>%
    mutate(ret = download.file(full_url, file_path))
  if (any(df_dl$ret != 0)) warning("Some downloads failed")
}

## define levels to extract
levels <- c("national", "regional", "local")

## define geography codes not in data
geography_codes <- c(England = "E92000001",
                     `North East` = "E12000001",
                     `North West` = "E12000002",
                     `Yorkshire and The Humber` = "E12000003",
                     `East Midlands` = "E12000004",
                     `West Midlands` = "E12000005",
                     `East of England` = "E12000006",
                     `London` = "E12000007",
                     `South East` = "E12000008",
                     `South West` = "E12000009")

## define table structure
columns <- c(national = 4, regional = 5, local = 6)

## list all files
files <- list.files(here::here("data", "cis"), full.names = TRUE)
list_file <- here::here("data", "cis_files.rds")

if (file.exists(list_file) && setequal(files, readRDS(list_file))) {
  stop("Nothing new to extract")
}

## manual override for problematic spreadsheet (wrong table label in contents)
override <- list(`covid19infectionsurveydatasets20210305v2.xlsx` =
                   list(`1k` = "1l"))

## construct list of data frames with positivity
positivity <- list()
for (level in levels) {
  positivity[[level]] <- lapply(files, function(x) {
    ## first,  get table of contents sheet to work out which sheet we want
    contents_sheet <- read_excel(x, sheet = "Contents") %>%
      clean_names()
    if (level == "national") {
      contents_sheet <- contents_sheet %>%
        filter(grepl("^Table", contents)) %>%
        head(n = 1)
    } else if (level == "regional") {
      contents_sheet <- contents_sheet %>%
        filter(grepl(" [Rr]egions?$", contents),
               !grepl("daily", contents),
               !grepl("14", contents))
    } else if (level == "local") {
      contents_sheet <- contents_sheet %>%
        filter(grepl("CIS sub-region", contents)) %>%
        head(n = 1)
    } else {
      stop("Unknown level: ", level)
    }
    ## extract table number
    sheet <- sub("^Table (.+) - .*$", "\\1", contents_sheet$contents)
    ## manual override
    if (basename(x) %in% names(override) &&
        sheet %in% names(override[[basename(x)]])) {
      sheet <- override[[x]][[sheet]]
    }
    if (length(sheet) == 1) {
      ## we found the sheet, now we get a preview so we can work out where in
      ## the sheet the actual table is
      preview <- read_excel(x, sheet = sheet) %>%
        clean_names()
      if (level %in% c("national", "regional")) {
        skip <- min(which(!is.na(preview$x2)))
      } else {
        skip <- which(preview$contents == "Geography Code")
      }
      ## work out the date of the survey
      if (level == "regional") {
        date_raw <- preview$contents[skip - 1]
        if (grepl("^[0-9]+$", date_raw)) {
          date <- as.Date(as.integer(date_raw), origin = "1899-12-30")
        } else {
          date <- dmy(date_raw)
        }
      } else {
        date_end <- dmy(sub("^.* to ", "", preview$contents[3]))
      }
      ## having figured out where the table is and extracted the date, read the table
      data <- read_excel(x, sheet = sheet, skip = skip) %>%
        clean_names()
      if (level == "national") {
        if (colnames(data)[1] != "time_period") return(NULL) ## old estimates,  not needed
        data <- data %>%
          slice(-1) %>%
          mutate(region = "England")
      } else if (level == "regional") {
        ## rename column
        data <- data %>%
          rename(region = 1) %>%
          filter(!is.na(region))
      } else {
        data <- data %>%
          filter(!is.na(geography_code),
                 !is.na(region))
      }
      data <- data %>%
        select(1:columns[level]) %>%
        rename(percentage_pos = columns[[level]] - 2,
               percentage_pos_low_95 = columns[[level]] - 1,
               percentage_pos_high_95 = columns[[level]]) %>%
        mutate_at(vars(starts_with("percentage")), as.numeric) %>%
        filter(!is.na(percentage_pos))
      if (level == c("national")) {
        data <- data %>%
          mutate(week_end = dmy(sub("^.* to ", "", time_period)),
                 region = "England") %>%
          select(-time_period)
      } else if (level == "regional") {
        data <- data %>%
          mutate(week_end = floor_date(date, "week", 1) + 6)
      } else if (level == "local"){
       data <- data %>%
         mutate(week_end = date_end)
       }
      return(data %>%
             mutate(file_name = x))
    } else {
      return(NULL)
    }
  })
  positivity[[level]] <- positivity[[level]] %>%
    bind_rows() %>%
    distinct() %>% ## avoid duplicate rows
    mutate(level = level)
}

## get latest national data
positivity$national <- positivity$national %>%
  filter(!is.na(week_end)) %>%
  group_by(file_name) %>%
  mutate(max_date = max(week_end)) %>%
  ungroup() %>%
  filter(max_date == max(max_date)) %>%
  select(-max_date)

## combine it all into one data frame
combined <- positivity %>%
  bind_rows( ) %>%
  select(-file_name) %>%
  mutate(geography_code =
           if_else(is.na(geography_code), geography_codes[region],
                   geography_code), 
         region = sub("the Humber", "The Humber", region),
         geography = if_else(level == "local", local_authority_areas, region),
         region = if_else(level == "national", NA_character_, region)) %>%
  select(level, geography, geography_code, region, week_end, starts_with("percentage_"))

## save
saveRDS(combined, here::here("data", "cis.rds"))

## save area mapping, correcting for LAD21 changes
areas <- combined %>%
  filter(level == "local") %>%
  select(geography_code, region, geography) %>%
  distinct() %>%
  mutate(lad = strsplit(geography, "; ")) %>%
  select(-geography) %>%
  unnest(lad) %>%
  mutate(lad = if_else(lad %in% c("Suffolk Coastal", "Waveney"),
                       "East Suffolk",
                       lad),
         lad = if_else(lad %in% c("Forest Heath", "St Edmundsbury"),
                       "West Suffolk",
                       lad),
         lad = if_else(lad %in% c("Taunton Deane", "West Somerset"),
                       "Somerset West and Taunton",
                       lad),
         lad = if_else(lad %in% c("Bournemouth", "Poole", "Christchurch"),
                       "Bournemouth, Christchurch and Poole",
                       lad),
         lad = if_else(lad %in% c("East Dorset", "North Dorset", "West Dorset", "Purbeck", "Weymouth and Portland"),
                       "Dorset",
                       lad),
         lad = if_else(lad %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"),
                       "Buckinghamshire",
                       lad),
         lad = if_else(lad %in% c("Corby", "East Northamptonshire", "Kettering", "Wellingborough"),
                       "North Northamptonshire",
                       lad),
         lad = if_else(lad %in% c("Daventry", "Northampton", "South Northamptonshire"),
                       "West Northamptonshire",
                       lad)) %>%
  rename(ltla_name = lad)

## save
saveRDS(areas, here::here("data", "cis_areas.rds"))

saveRDS(files, list_file)
