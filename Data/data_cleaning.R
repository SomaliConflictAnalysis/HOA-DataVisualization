## Import Casualty Data
somalia_raw <- read.csv("Data/Somalia.csv")
ethiopia_raw <- read.csv("Data/Ethiopia.csv")
djibouti_raw <- read.csv("Data/Djibouti.csv")
kenya_raw <- read.csv("Data/Kenya.csv")

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
data_somalia <- somalia_keep[somalia_keep$country == "Somalia", ]
data_somaliland <- somalia_keep[somalia_keep$country == "Somaliland", ]

## Separating Somali Region from Ethiopia and adding country column
data_somali_region <- ethiopia_keep[ethiopia_keep$admin1 == "Somali", ]
data_somali_region$country <- "Somali Region (Ethiopia)"

## Separating North Eastern Province from Kenya and adding country column
data_nep <- kenya_keep[kenya_keep$admin1 %in% c("Garissa", "Mandera", "Wajir"), ]
data_nep$country <- "North Eastern Province (Kenya)"

## Renaming Djibouti dataset & adding country column
data_djibouti <- djibouti_keep
data_djibouti$country <- "Djibouti"

## Combining datasets for combined analysis
data_combined <- rbind(data_somalia, data_somaliland, data_somali_region, data_djibouti, data_nep)

## Format dates for all datasets
data_somalia$event_date <- as.Date(trimws(data_somalia$event_date))
data_somaliland$event_date <- as.Date(trimws(data_somaliland$event_date))
data_somali_region$event_date <- as.Date(trimws(data_somali_region$event_date))
data_djibouti$event_date <- as.Date(trimws(data_djibouti$event_date))
data_nep$event_date <- as.Date(trimws(data_nep$event_date))
data_combined$event_date <- as.Date(trimws(data_combined$event_date))

## Saving each dataset as individual .RData files
save(data_somalia, file = "Data/data_somalia.RData")
save(data_somaliland, file = "Data/data_somaliland.RData")
save(data_somali_region, file = "Data/data_somali_region.RData")
save(data_djibouti, file = "Data/data_djibouti.RData")
save(data_nep, file = "Data/data_nep.RData")
save(data_combined, file = "Data/data_combined.RData")