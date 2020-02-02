## Load North Eastern Province data to environment
load(file = "Data/data_nep.RData")

## Graphing fatalities per year in NEP
library(ggplot2)
ggplot(data_nep, aes(data_nep$year, data_nep$fatalities)) + 
  geom_col(fill = "#e34a33")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in North Eastern Province (Kenya) by year",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "NEP Analysis/Figures/AnnualFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Print total # of fatalities
print(sum(data_nep$fatalities))

## Calculating sum of fatalities in 1998 (highest year)
sum_1998 <- data_nep[data_nep$year == 1998,]
print(sum(sum_1998$fatalities))

## Calculating sum of fatalities in 2015 (second highest year)
sum_2015 <- data_nep[data_nep$year == 2015,]
print(sum(sum_2015$fatalities))