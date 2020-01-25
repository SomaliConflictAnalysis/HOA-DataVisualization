## Load Somalia data to environment
load(file = "Data/data_somalia.RData")

## Creating dataset for fatalities involving civilians
som_civilians <- data_somalia[(data_somalia$actor1 == "Civilians (Somalia)" | 
                                 data_somalia$actor2 == "Civilians (Somalia)"), ]

## Graphing fatalities involving civilians by year
library(ggplot2)
ggplot(som_civilians, aes(som_civilians$year, som_civilians$fatalities)) + 
  geom_col(fill = "#e34a33")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15), 
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities involving civilians in Somalia by year",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/AnnualCivFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Creating a categorical year variable
som_civilians$year_cat <- ifelse(som_civilians$year %in% 1997:1999, "1997-1999",
                          ifelse(som_civilians$year %in% 2000:2004, "2000-2004",
                          ifelse(som_civilians$year %in% 2005:2009, "2005-2009",
                          ifelse(som_civilians$year %in% 2010:2014, "2010-2014",
                          ifelse(som_civilians$year %in% 2015:2019, "2015-2019",
                          ifelse(som_civilians$year == 2020, "2020-present", NA))))))

## Calculating total fatalities by year categories
fatalities <- aggregate(som_civilians$fatalities, by = list(year_cat = som_civilians$year_cat), FUN = sum)
fatalities <- as.data.frame(fatalities)
names(fatalities) <- c("year_cat", "fatalities")

## Graphing fatalities by year categories
ggplot(fatalities, aes(year_cat, fatalities, fill = year_cat)) + 
  geom_col()+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.caption = element_text(size = 12.5),
        legend.position = "none") +
  geom_text(aes(label = fatalities), vjust = -0.5, size = 7.5) +
  labs(x= NULL, y = NULL, title = "Fatalities involving civilians in Somalia by grouped years",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/CivCatYearFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculate sum of civilian involved fatalities in 2017
sum_2017 <- som_civilians[som_civilians$year == 2017, ]
print(sum(sum_2017$fatalities))
  
## Calculating percentage for 2015-2019
sum_2015_2019 <- fatalities[fatalities$year_cat == "2015-2019",]
sum_2015_2019 <- sum(sum_2015_2019$fatalities)

sum_total <- sum(fatalities$fatalities)
print(sum_total)

pct_2015_2019 <- (sum_2015_2019/sum_total) * 100
print(pct_2015_2019)

## Calculating civilian involved fatalities by province
fat_prov <- aggregate(som_civilians$fatalities, by = list(prov = som_civilians$admin1), FUN = sum)
fat_prov <- as.data.frame(fat_prov)
names(fat_prov) <- c("prov", "fatalities")

## Graphing terror fatalities by province in Somalia
ggplot(fat_prov, aes(prov, fatalities)) + 
  geom_col(fill = "#1c9099")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  geom_text(aes(label = fatalities), vjust = -0.5, size = 7.5) + 
  labs(x= NULL, y = NULL, title = "Civilian-involved fatalities in Somalia by province (1997-2020)",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/CivProvinceFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating percentage for Banadir
sum_banadir <- fat_prov[fat_prov$prov == "Banadir",]
sum_banadir <- sum(sum_banadir$fatalities)

sum_total <- sum(fat_prov$fatalities)

pct_banadir <- (sum_banadir/sum_total) * 100
print(pct_banadir)
