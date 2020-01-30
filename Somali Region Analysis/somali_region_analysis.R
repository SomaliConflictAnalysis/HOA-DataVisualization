## Load Somali Region data to environment
load(file = "Data/data_somali_region.RData")

## Graphing fatalities per year in Somali Region
library(ggplot2)
ggplot(data_somali_region, aes(data_somali_region$year, data_somali_region$fatalities)) + 
  geom_col(fill = "#3182bd")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Somali Region (Ethiopia) by year",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somali Region Analysis/Figures/AnnualFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating sum of fatalities in 2017 (highest year)
sum_2009 <- data_somali_region[data_somali_region$year == 2009,]
print(sum(sum_2009$fatalities))
print(sum(data_somali_region$fatalities))

## Graphing fatalities by type in Somali Region (Ethiopia)
ggplot(data_somali_region, aes(data_somali_region$event_type, data_somali_region$fatalities)) + 
  geom_col(fill = "#1c9099")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Somali Region (Ethiopia) by type",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somali Region Analysis/Figures/TypeFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating percentage of fatalities that were battles
sum_battles <- data_somali_region[data_somali_region$event_type == "Battles",]
print(sum(sum_battles$fatalities)/sum(data_somali_region$fatalities) * 100)

## Calculating total fatalities by province
fat_prov <- aggregate(data_somali_region$fatalities, by = list(prov = data_somali_region$admin2), FUN = sum)
fat_prov <- as.data.frame(fat_prov)
names(fat_prov) <- c("prov", "fatalities")

## Graph fatalities in Somali Region by zone
ggplot(fat_prov, aes(prov, fatalities)) + 
  geom_col(fill = "#018571")+ 
  theme(axis.text.x = element_text(size=15), legend.position = "none",
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Somali Region (Ethiopia) by zone (1997-2020)",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  geom_text(aes(label = fatalities), vjust = -0.5, size = 7.5) + 
  ggsave(filename = "Somali Region Analysis/Figures/ProvinceFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating percentages for top 3 provinces
fafan <- fat_prov[fat_prov$prov == "Fafan",]
print(sum(fafan$fatalities)/sum(fat_prov$fatalities) * 100)
jarar <- fat_prov[fat_prov$prov == "Jarar",]
print(sum(jarar$fatalities)/sum(fat_prov$fatalities) * 100)
korahe <- fat_prov[fat_prov$prov == "Korahe",]
print(sum(korahe$fatalities)/sum(fat_prov$fatalities) * 100)

