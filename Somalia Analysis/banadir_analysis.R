## Load Somalia data to environment
load(file = "Data/data_somalia.RData")

## Creating dataset for terrorism only fatalities
data_banadir <- data_somalia[data_somalia$admin1 == "Banadir", ]

## Graphing fatalities per year in Banadir
library(ggplot2)
ggplot(data_banadir, aes(data_banadir$year, data_banadir$fatalities)) + 
  geom_col(fill = "#3182bd")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Banadir by year",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/BanadirAnnFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating sum of fatalities in Banadir
print(sum(data_banadir$fatalities))

## Calculating sum of fatalities in 2010
sum_2010 <- data_banadir[data_banadir$year == 2010, ]
print(sum(sum_2010$fatalities))

## Creating a categorical year variable
data_banadir$year_cat <- ifelse(data_banadir$year %in% 1997:1999, "1997-1999",
                         ifelse(data_banadir$year %in% 2000:2004, "2000-2004",
                         ifelse(data_banadir$year %in% 2005:2009, "2005-2009",
                         ifelse(data_banadir$year %in% 2010:2014, "2010-2014",
                         ifelse(data_banadir$year %in% 2015:2019, "2015-2019",
                         ifelse(data_banadir$year == 2020, "2020-present", NA))))))

## Calculating total fatalities by year categories
fatalities <- aggregate(data_banadir$fatalities, by = list(year_cat = data_banadir$year_cat), FUN = sum)
fatalities <- as.data.frame(fatalities)
names(fatalities) <- c("year_cat", "fatalities")

## Graphing fatalities by year categories
ggplot(fatalities, aes(year_cat, fatalities, fill = year_cat)) + 
  geom_col()+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5),
        legend.position = "none") +
  geom_text(aes(label = fatalities), vjust = -0.5, size = 7.5) +
  labs(x= NULL, y = NULL, title = "Fatalities in Banadir by grouped years",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/BanadirCatYearFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Adding column for president
library(data.table)
president <- ifelse((data_banadir$event_date %between% c("1991-01-27", "1997-01-03")), 
              as.character("Ali Mahdi Mohamed (Jan 1991-Jan 1997)"),
             ifelse((data_banadir$event_date %between% c("1997-01-04","2000-08-27")), 
              as.character("Vacant (Jan 1997-Aug 2000)"),
             ifelse((data_banadir$event_date %between% c("2000-08-28","2004-10-14")), 
              as.character("Abdiqasim Salad (Aug 2000-Oct 2004)"),
             ifelse((data_banadir$event_date %between% c("2004-10-15","2008-12-29")), 
              as.character("Abdullahi Yusuf (Oct 2004-Dec 2008)"),
             ifelse((data_banadir$event_date %between% c("2009-01-31","2012-08-20")),
              as.character("Sharif Sh. Ahmed (Jan 2009-Aug 2012)"),
             ifelse((data_banadir$event_date %between% c("2012-09-16","2017-02-16")),
              as.character("Hassan Sh. Mohamud (Sep 2012-Feb 2017)"),
             ifelse((data_banadir$event_date %between% c("2017-02-17", "2020-01-17")), 
              as.character("Mohamed Farmaajo (Feb 2017-present)"), 
             as.character("Fatalities during acting/interim president terms"))))))))
data_banadir$president <- president

## Creating an order between presidents
data_banadir$president <- factor(data_banadir$president, 
                                 levels = c("Fatalities during acting/interim president terms",
                                            "Ali Mahdi Mohamed (Jan 1991-Jan 1997)", "Vacant (Jan 1997-Aug 2000)", 
                                            "Abdiqasim Salad (Aug 2000-Oct 2004)", "Abdullahi Yusuf (Oct 2004-Dec 2008)",
                                            "Sharif Sh. Ahmed (Jan 2009-Aug 2012)", "Hassan Sh. Mohamud (Sep 2012-Feb 2017)",
                                            "Mohamed Farmaajo (Feb 2017-present)"))

## Graphing fatalities by president
fat_pres <- aggregate(data_banadir$fatalities, by=list(pres = data_banadir$president), 
                      FUN = sum)
names(fat_pres) <- c("pres", "fatalities")

ggplot(fat_pres, aes(fat_pres$pres, fat_pres$fatalities, fill = fat_pres$pres)) + 
  geom_col() + coord_flip() + 
  theme(axis.text.y = element_text(size=15, face = "bold"), legend.position = "none",
        axis.text.x = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold")) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Banadir by president",
       caption = "Data as of January 11, 2020
       Source: ACLED Data, Somali Conflict Analysis Group") +
  geom_text(aes(label = fat_pres$fatalities), size = 7.5, hjust = "right") + 
  ggsave(filename = "Somalia Analysis/Figures/BanadirPresFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating fatality counts for Sharif, HSM, Farmaajo
sum_sharif <- data_banadir[data_banadir$president == "Sharif Sh. Ahmed (Jan 2009-Aug 2012)", ]
sum_sharif <- sum(sum_sharif$fatalities)
sum_hsm <- data_banadir[data_banadir$president == "Hassan Sh. Mohamud (Sep 2012-Feb 2017)", ]
sum_hsm <- sum(sum_hsm$fatalities)
sum_farmaajo <- data_banadir[data_banadir$president == "Mohamed Farmaajo (Feb 2017-present)", ]
sum_farmaajo <- sum(sum_farmaajo$fatalities)

## Calculating percent change from Sharif to HSM
pct_change_hsm <- (sum_hsm - sum_sharif)/sum_sharif * 100
print(pct_change_hsm)

## Calculating percent change from HSM to Farmaajo
pct_change_farmaajo <- (sum_farmaajo - sum_hsm)/sum_hsm * 100
print(pct_change_farmaajo)

## Graphing fatalities by type in Banadir
fat_type <- aggregate(data_banadir$fatalities, by=list(type = data_banadir$event_type), 
                      FUN = sum)
names(fat_type) <- c("type", "fatalities")

ggplot(fat_type, aes(type, fatalities)) + 
  geom_col(fill = "#3182bd")+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Fatalities in Banadir by type (1997-2020)",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  geom_text(aes(label = fat_type$fatalities), size = 7.5, vjust = -0.5) + 
  ggsave(filename = "Somalia Analysis/Figures/BanadirTypeFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating totals for the top 3 categories
sum_battles <- fat_type[fat_type$type == "Battles",]
sum_battles <- sum(sum_battles$fatalities)
print(sum_battles/sum(fat_type$fatalities) *100)


sum_terror <- fat_type[fat_type$type == "Explosions/Remote violence",]
sum_terror <- sum(sum_terror$fatalities)
print(sum_terror/sum(fat_type$fatalities)*100)

sum_civ_violence <- fat_type[fat_type$type == "Violence against civilians",]
sum_civ_violence <- sum(sum_civ_violence$fatalities)
print(sum_civ_violence/sum(fat_type$fatalities)*100)

## Examining terror fatalities in Banadir
terror_banadir <- data_banadir[data_banadir$event_type == "Explosions/Remote violence",]

## Calculating terror fatalities in Banadir by year_cat
ban_ter_year <- aggregate(terror_banadir$fatalities, by = list(year_cat = terror_banadir$year_cat), FUN = sum)
ban_ter_year <- as.data.frame(ban_ter_year)
names(ban_ter_year) <- c("year_cat", "fatalities")

## Graphing terror fatalities in Banadir by year categories
ggplot(ban_ter_year, aes(year_cat, fatalities, fill = year_cat)) + 
  geom_col()+
  theme(axis.text.x = element_text(size=15, face = "bold"),
        axis.text.y = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5),
        legend.position = "none") +
  geom_text(aes(label = fatalities), vjust = -0.5, size = 7.5) +
  labs(x= NULL, y = NULL, title = "Terror-related fatalities in Banadir by grouped years",
       caption = "Data as of January 11, 2020
       Source: ACLED, Somali Conflict Analysis Group") +
  ggsave(filename = "Somalia Analysis/Figures/BanadirTerrCatYearFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating terror fatalities in Banadir by president
ban_ter_pres <- aggregate(terror_banadir$fatalities, by = list(pres = terror_banadir$president), FUN = sum)
ban_ter_pres <- as.data.frame(ban_ter_pres)
names(ban_ter_pres) <- c("president", "fatalities")

## Graphing terror fatalities in Banadir by president
ggplot(ban_ter_pres, aes(president, fatalities, fill = president)) + 
  geom_col() + coord_flip() + 
  theme(axis.text.y = element_text(size=15, face = "bold"), legend.position = "none",
        axis.text.x = element_text(size=15),
        plot.title = element_text(size = 40, face = "bold"),
        plot.caption = element_text(size = 12.5)) + 
  labs(x= NULL, y = NULL, title = "Terror-related fatalities in Banadir by president",
       caption = "Data as of January 11, 2020
       Source: ACLED Data, Somali Conflict Analysis Group") +
  geom_text(aes(label = fatalities), size = 7.5, hjust = "right") + 
  ggsave(filename = "Somalia Analysis/Figures/BanadirPresTerFatalities.png", last_plot(),
         width = 20, height = 10, dpi = 400)

## Calculating fatality counts for Sharif, HSM, Farmaajo
sum_sharif <- ban_ter_pres[ban_ter_pres$president == "Sharif Sh. Ahmed (Jan 2009-Aug 2012)", ]
sum_sharif <- sum(sum_sharif$fatalities)
sum_hsm <- ban_ter_pres[ban_ter_pres$president == "Hassan Sh. Mohamud (Sep 2012-Feb 2017)", ]
sum_hsm <- sum(sum_hsm$fatalities)
sum_farmaajo <- ban_ter_pres[ban_ter_pres$president == "Mohamed Farmaajo (Feb 2017-present)", ]
sum_farmaajo <- sum(sum_farmaajo$fatalities)

## Calculating percent change from Sharif to HSM
pct_change_hsm <- (sum_hsm - sum_sharif)/sum_sharif * 100
print(pct_change_hsm)

## Calculating percent change from HSM to Farmaajo
pct_change_farmaajo <- (sum_farmaajo - sum_hsm)/sum_hsm * 100
print(pct_change_farmaajo)
