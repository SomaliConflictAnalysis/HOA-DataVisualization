## Load Somalia data to environment
load(file = "Data/data_somalia.RData")

## Graphing fatalities per year in Somalia
library(ggplot2)
ggplot(data_somalia, aes(data_somalia$year, data_somalia$fatalities)) + 
  geom_col(fill = "#3182bd")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Somalia by year",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/AnnualFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating sum of fatalities in 2017 (highest year)
sum_2017 <- data_somalia[data_somalia$year == 2017,]
print(sum(sum_2017$fatalities))

## Creating a categorical year variable
data_somalia$year_cat <- ifelse(data_somalia$year %in% 1997:1999, "1997-1999",
                         ifelse(data_somalia$year %in% 2000:2004, "2000-2004",
                         ifelse(data_somalia$year %in% 2005:2009, "2005-2009",
                         ifelse(data_somalia$year %in% 2010:2014, "2010-2014",
                         ifelse(data_somalia$year %in% 2015:2019, "2015-2019",
                         ifelse(data_somalia$year == 2020, "2020-present", NA))))))

## Calculating total fatalities by year categories
fatalities <- aggregate(data_somalia$fatalities, by = list(year_cat = data_somalia$year_cat), FUN = sum)
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
  labs(x= NULL, y = NULL, title = "Fatalities in Somalia by grouped years",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/CatYearFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating percentage for 2015-2019
sum_2015_2019 <- fatalities[fatalities$year_cat == "2015-2019",]
sum_2015_2019 <- sum(sum_2015_2019$fatalities)

sum_total <- sum(fatalities$fatalities)

pct_2015_2019 <- (sum_2015_2019/sum_total) * 100
print(pct_2015_2019)