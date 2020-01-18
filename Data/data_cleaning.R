## Import Casualty Data
somalia_raw <- read.csv("Data/1900-01-01-2020-01-16-Somalia.csv")
ethiopia_raw <- read.csv("Data/1900-01-01-2020-01-16-Ethiopia.csv")
djibouti_raw <- read.csv("Data/1900-01-01-2020-01-16-Djibouti.csv")
kenya_raw <- read.csv("Data/1900-01-01-2020-01-16-Kenya.csv")

## Creating object to keep columns
keep <- c("event_date", "year", "event_type", "sub_event_type", "actor1", "actor2", "admin1", 
          "location", "fatalities")

## New Datasets with keep columns
somalia_keep <- somalia_raw[keep]
ethiopia_keep <- ethiopia_raw[keep]
djibouti_keep <- djibouti_raw[keep]
kenya_keep <- kenya_raw[keep]

## Separating Somaliland and Somalia
country <- ifelse(somalia_keep$admin1 %in% c("Woqooyi Galbeed", "Awdal", "Togdheer", "Sanaag", "Sool"), 
                  as.character("Somaliland"), as.character("Somalia"))
somalia_keep$country <- country
data_somalia <- somalia_keep[somalia_sl_clean$country == "Somalia", ]
data_somaliland <- somalia_keep[somalia_sl_clean$country == "Somaliland", ]
