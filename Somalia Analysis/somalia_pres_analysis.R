## Load Somalia data to environment
load(file = "Data/data_somalia.RData")

## Adding column for president
library(data.table)
president <- ifelse((data_somalia$event_date %between% c("1991-01-27", "1997-01-03")), 
                as.character("Ali Mahdi Mohamed (Jan 1991-Jan 1997)"),
              ifelse((data_somalia$event_date %between% c("1997-01-04","2000-08-27")), 
                as.character("Vacant (Jan 1997-Aug 2000)"),
              ifelse((data_somalia$event_date %between% c("2000-08-28","2004-10-14")), 
                as.character("Abdiqasim Salad (Aug 2000-Oct 2004)"),
              ifelse((data_somalia$event_date %between% c("2004-10-15","2008-12-29")), 
                as.character("Abdullahi Yusuf (Oct 2004-Dec 2008)"),
              ifelse((data_somalia$event_date %between% c("2009-01-31","2012-08-20")),
                as.character("Sharif Sh. Ahmed (Jan 2009-Aug 2012)"),
              ifelse((data_somalia$event_date %between% c("2012-09-16","2017-02-16")),
                as.character("Hassan Sh. Mohamud (Sep 2012-Feb 2017)"),
              ifelse((data_somalia$event_date %between% c("2017-02-17", "2020-01-30")), 
                as.character("Mohamed Farmaajo (Feb 2017-present)"), 
              as.character("Fatalities during acting/interim president terms"))))))))
data_somalia$president <- president

## Creating an order between presidents
data_somalia$president <- factor(data_somalia$president, 
                                 levels = c("Fatalities during acting/interim president terms",
                                            "Ali Mahdi Mohamed (Jan 1991-Jan 1997)", "Vacant (Jan 1997-Aug 2000)", 
                                            "Abdiqasim Salad (Aug 2000-Oct 2004)", "Abdullahi Yusuf (Oct 2004-Dec 2008)",
                                            "Sharif Sh. Ahmed (Jan 2009-Aug 2012)", "Hassan Sh. Mohamud (Sep 2012-Feb 2017)",
                                            "Mohamed Farmaajo (Feb 2017-present)"))

## Graphing fatalities by president
fat_pres <- aggregate(data_somalia$fatalities, by=list(pres = data_somalia$president), 
                          FUN = sum)
names(fat_pres) <- c("pres", "fatalities")

ggplot(fat_pres, aes(fat_pres$pres, fat_pres$fatalities, fill = fat_pres$pres)) + 
  geom_col() + coord_flip() + 
  theme(axis.text.y = element_text(size=15, face = "bold"), legend.position = "none",
        axis.text.x = element_text(size=15),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) + 
  labs(x= NULL, y = NULL, title = "Conflict fatalities in Somalia by president",
       caption = "Data as of January 18, 2020
       Source: ACLED Data, Somali Conflict Analysis Group") +
  geom_text(aes(label = fat_pres$fatalities), size = 7.5, hjust = "right") + 
  ggsave(filename = "Somalia Analysis/Figures/PresFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Creating dataset for terrorism only fatalities
som_terrorism <- data_somalia[data_somalia$event_type == "Explosions/Remote violence", ]

## Graphing terrorism only fatalities by president
fat_pres_ter <- aggregate(som_terrorism$fatalities, by=list(pres = som_terrorism$president), 
                          FUN = sum)
names(fat_pres_ter) <- c("pres", "fatalities")

ggplot(fat_pres_ter, aes(fat_pres_ter$pres, fat_pres_ter$fatalities, fill = fat_pres_ter$pres)) + 
  geom_col() + coord_flip() + 
  theme(axis.text.y = element_text(size=15, face = "bold"), legend.position = "none",
        axis.text.x = element_text(size=15),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +  
  labs(x= NULL, y = NULL, title = "Terror related fatalities in Somalia by president",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  geom_text(aes(label = fat_pres_ter$fatalities), size = 7.5, hjust = "right") + 
  ggsave(filename = "Somalia Analysis/Figures/PresTerrorFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Creating dataset for fatalities involving civilians
som_civilians <- data_somalia[(data_somalia$actor1 == "Civilians (Somalia)" | 
                                 data_somalia$actor2 == "Civilians (Somalia)"), ]

## Graphing fatalities involving civilians by president
fat_pres_civ <- aggregate(som_civilians$fatalities, by=list(pres = som_civilians$president), 
                          FUN = sum)
names(fat_pres_civ) <- c("pres", "fatalities")

ggplot(fat_pres_civ, aes(fat_pres_civ$pres, fat_pres_civ$fatalities, fill = fat_pres_civ$pres)) + 
  geom_col() + coord_flip() + 
  theme(axis.text.y = element_text(size=15, face = "bold"), legend.position = "none",
        axis.text.x = element_text(size=15),
        plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) +  
  labs(x= NULL, y = NULL, title = "Fatalities involving civilians Somalia by president",
       caption = "Data as of January 18, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  geom_text(aes(label = fat_pres_civ$fatalities), size = 7.5, hjust = "right") + 
  ggsave(filename = "Somalia Analysis/Figures/PresCivFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)
