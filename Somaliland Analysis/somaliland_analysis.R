## Load Somaliland data to environment
load(file = "Data/data_somaliland.RData")

## Calculating total fatalities by province
fat_prov <- aggregate(data_somaliland$fatalities, by = list(prov = data_somaliland$admin1), FUN = sum)
fat_prov <- as.data.frame(fat_prov)
names(fat_prov) <- c("prov", "fatalities")

## Graphing fatalities by province in Somaliland
library(ggplot2)
ggplot(fat_prov, aes(prov, fatalities, fill = prov)) + 
  geom_col()+ coord_flip() +
  theme(axis.text.x = element_text(size=15), legend.position = "none",
        axis.text.y = element_text(size=15, face = "bold"),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Somaliland by province (1997-2020)",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  geom_text(aes(label = fatalities), hjust = "right", size = 7.5) + 
  ggsave(filename = "Somaliland Analysis/Figures/ProvinceFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## East vs West fatality count
data_east <- data_somaliland[data_somaliland$admin1 %in% c("Sool", "Sanaag", "Togdheer"),]
data_west <- data_somaliland[data_somaliland$admin1 %in% c("Awdal", "Woqooyi Galbeed"),]
print(sum(data_east$fatalities)/sum(data_somaliland$fatalities)*100)

## Create dataset for Sool 
data_sool <- data_somaliland[data_somaliland$admin1 == "Sool", ]

## Graph fatalities in Sool by town
ggplot(data_sool[which(data_sool$fatalities >0),], aes(location, fatalities)) + 
  geom_col(fill = "#e66101")+ coord_flip() +
  theme(axis.text.x = element_text(size=25), legend.position = "none",
        axis.text.y = element_text(size=25),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 20)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Sool by town (1997-2020)",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somaliland Analysis/Figures/SoolFatalities.png", last_plot(),
         width = 20, height = 20, dpi = 400)

## Calculate percentage of fatalities in Laascaanood
data_laaska <- data_sool[data_sool$location == "Laascaanood",]
print(sum(data_laaska$fatalities)/sum(data_sool$fatalities) * 100)

## Create dataset for Togdheer 
data_togdheer <- data_somaliland[data_somaliland$admin1 == "Togdheer", ]

## Graph fatalities in Sool by town
ggplot(data_togdheer[which(data_togdheer$fatalities >0),], aes(location, fatalities)) + 
  geom_col(fill = "#1a9641")+ coord_flip() +
  theme(axis.text.x = element_text(size=25), legend.position = "none",
        axis.text.y = element_text(size=25),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 20)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Togdheer by town (1997-2020)",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somaliland Analysis/Figures/TogdheerFatalities.png", last_plot(),
         width = 20, height = 20, dpi = 400)

## Calculate percentage of fatalities in Buuhoodle
data_buuhoodle <- data_togdheer[data_togdheer$location == "Buuhoodle",]
print(sum(data_buuhoodle$fatalities)/sum(data_togdheer$fatalities) * 100)