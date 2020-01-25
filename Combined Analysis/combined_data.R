## Load combined data to environment
load(file = "Data/data_combined.RData")

## Graphing deaths by country/region
# Aggregating total number of deaths per country
fatalities <- aggregate(data_combined$fatalities, by = list(Country = data_combined$country), FUN = sum)
fatalities <- as.data.frame(fatalities)
names(fatalities) <- c("Country", "Fatalities")

# Creating graph
library(ggplot2)
plot <- ggplot(fatalities, aes(x = Country, y = Fatalities, fill = Country)) +
  geom_col() + theme(axis.text.x = element_text(size=12.5, face = "bold"), legend.position = "none", 
                     plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + 
  geom_text(aes(label = Fatalities), size = 7.5, vjust = -0.5) + 
  labs(x= NULL, y = NULL, title = "Conflict Fatalities in the Horn of Africa (1997-2020)", 
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Combined Analysis/Figures/CountryDeaths.png", last_plot(),
         width = 15, height = 10, dpi = 400)
