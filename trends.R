## Tables for Creative Economy
## Written By: Andrew Yu (4/20/22)

## Packages
library(readstata13)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(readxl)
library(stringr)

## Employment Growth Tables
data <- read.dta13("ca_emplt_growth.dta")
ca <- read.dta13("ca_summary.dta")
yoy <- read.dta13("yoy_new.dta")
yoy2 <- read.dta13("yoy2_new.dta")
ca_data <- read.dta13("ca_data.dta")
ca_tot <- read.dta13("ca_total_new.dta")
ce <- read.dta13("ce_totals.dta")

## California Summary

ca_data <- ca_data %>% filter(year == 2021) %>%
  subset(select = -c(year, total, value)) 
ggplot(ca_data, aes(fill=sector, y=share, x=Category)) + 
  geom_bar(position='fill',stat='identity') +  
  theme_fivethirtyeight() + 
  theme(axis.title=element_text(size=12),legend.position = "bottom") +
  labs(title = "Shares by Sector in Creative Economy Sectors 2021", fill='Sectors') + 
  xlab("Share Type") + 
  ylab("Share (%)") + 
  scale_x_discrete(labels=c('Employment','Establishments','Wages'))
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/shares.png")  

## YOY
new_yoy <- bind_rows(yoy,ca_tot)
new_yoy$id <- 1 
new_yoy$id[new_yoy$Sectors!='California'] <- 0
new_yoy$id <- as.character(new_yoy$id)

ggplot(new_yoy, aes(x=year, y=indemplt, linetype = id)) + 
  geom_line(aes(colour = Sectors),size=2) + 
  labs(title = "Indexed Employment Change in California (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") + 
  theme_fivethirtyeight() + 
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) +
  scale_colour_manual(values=c('Architecture and Related Services' = 'seagreen4',
                               "Creative Goods and Products: Design & Manufacture" = "deeppink3",
                               "Entertainment" = "dodgerblue4",
                               "Fashion" = "purple3",
                               "Fine Arts: Performance and Institutions" = "darkorange1",
                               "Media & Digital Media" = "dodgerblue1",
                               "California" = 'red')) +
  guides(linetype = 'none',colour=guide_legend(nrow=3)) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/yoy.png")  

ggplot(new_yoy, aes(x=year, y=indwageEmp, linetype = id)) + 
  geom_line(aes(colour = Sectors),size=2) + 
  labs(title = "Indexed Wages per Employee in California (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() + 
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) +
  scale_colour_manual(values=c('Architecture and Related Services' = 'seagreen4',
                               "Creative Goods and Products: Design & Manufacture" = "deeppink3",
                               "Entertainment" = "dodgerblue4",
                               "Fashion" = "purple3",
                               "Fine Arts: Performance and Institutions" = "darkorange1",
                               "Media & Digital Media" = "dodgerblue1",
                               "California" = 'red')) +
  guides(linetype = 'none',colour=guide_legend(nrow=3)) + 
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/creative1.png")

ggplot(new_yoy, aes(x=year, y=indempEst, linetype = id)) + 
  geom_line(aes(colour = Sectors),size=2) + 
  labs(title = "Indexed Employment per Establishment in California (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() + 
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) +
  scale_colour_manual(values=c('Architecture and Related Services' = 'seagreen4',
                               "Creative Goods and Products: Design & Manufacture" = "deeppink3",
                               "Entertainment" = "dodgerblue4",
                               "Fashion" = "purple3",
                               "Fine Arts: Performance and Institutions" = "darkorange1",
                               "Media & Digital Media" = "dodgerblue1",
                               "California" = 'red')) +
  guides(linetype = 'none',colour=guide_legend(nrow=3)) + 
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/creative2.png")

ceCA <- bind_rows(ce,ca_tot)
ceCA$Sectors <- ifelse(is.na(ceCA$Sectors), ceCA$sector, ceCA$Sectors)
ceCA <- ceCA %>% mutate(emplt=round(emplt,digits=0)) %>%
  mutate(wageEmp=round(wageEmp,digits=0)) %>% 
  mutate(empEst=round(empEst,digits=0)) %>%
  mutate(wageEmp_na=round(wageEmp_na,digits=0))

ceCA$emplt <- format(ceCA$emplt, nsmall=0, big.mark=",")
ceCA$numest <- format(round(ceCA$numest,0), nsmall=0, big.mark=",")
ceCA$wage <- format(ceCA$wage, nsmall=0, big.mark=",")
ceCA$adjwage <- format(ceCA$adjwage, nsmall=0, big.mark=",")
ceCA$wageEmp <- format(ceCA$wageEmp, nsmall=0, big.mark=",")
ceCA$empEst <- format(ceCA$empEst, nsmall=0, big.mark=",")
ceCA$wageEmp_na <- format(ceCA$wageEmp_na, nsmall=0, big.mark=",")
ceCA$wageEmp <-paste("$",ceCA$wageEmp)
ceCA$adjwage <-paste("$",ceCA$adjwage)
ceCA$wageEmp_na <- paste("$",ceCA$wageEmp_na)

ggplot(ceCA, aes(x=year, y=indemplt)) + 
  geom_line(size=2,aes(color=Sectors)) + 
  geom_label_repel(aes(label = emplt,color=Sectors),
             label.padding = unit(0.25, "lines"),
             label.r = unit(0.15, "lines"),
             label.size = 0.2,
             show.legend = FALSE) +
  labs(title = "Indexed Employment \nCalifornia vs Creative Economy (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() +  
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) + 
  scale_color_manual(values = c('red','blue'), labels = c('California','Creative Economy')) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/ce1.png")

ggplot(ceCA, aes(x=year, y=indadjwage)) + 
  geom_line(size=2,aes(color=Sectors)) + 
  geom_label_repel(aes(label = adjwage,color=Sectors),
                   label.padding = unit(0.25, "lines"),
                   label.r = unit(0.15, "lines"),
                   label.size = 0.2,
                   show.legend = FALSE) +
  labs(title = "Indexed Wage (Adjusted for CPI) \nCalifornia vs Creative Economy (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() +  
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) + 
  scale_color_manual(values = c('red','blue'), labels = c('California','Creative Economy')) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/ce3.png")

ggplot(ceCA, aes(x=year, y=indwageEmp)) + 
  geom_line(size=2,aes(color=Sectors)) + 
  geom_label_repel(aes(label = wageEmp,color=Sectors),
                   label.padding = unit(0.25, "lines"),
                   label.r = unit(0.15, "lines"),
                   label.size = 0.2,
                   show.legend = FALSE) +
  labs(title = "Indexed Wages (Adjusted for CPI) per Employee \nCalifornia vs Creative Economy (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() +  
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) + 
  scale_color_manual(values = c('red','blue'), labels = c('California','Creative Economy')) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/ce4.png")

ggplot(ceCA, aes(x=year, y=indempEst)) + 
  geom_line(size=2,aes(color=Sectors)) + 
  geom_label_repel(aes(label = empEst,color=Sectors),
                   label.padding = unit(0.25, "lines"),
                   label.r = unit(0.15, "lines"),
                   label.size = 0.2,
                   show.legend = FALSE) +
  labs(title = "Indexed Employment per Establishment \nCalifornia vs Creative Economy (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() +  
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) + 
  scale_color_manual(values = c('red','blue'), labels = c('California','Creative Economy')) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/ce5.png")

ggplot(ceCA, aes(x=year, y=indwageEmp_na)) + 
  geom_line(size=2,aes(color=Sectors)) + 
  geom_label_repel(aes(label = wageEmp_na,color=Sectors),
                   label.padding = unit(0.25, "lines"),
                   label.r = unit(0.15, "lines"),
                   label.size = 0.2,
                   show.legend = FALSE) +
  labs(title = "Indexed Wages per Employee \nCalifornia vs Creative Economy (2015 to 2021)") + 
  xlab("Year") + 
  ylab("Indexed at 100") +
  theme_fivethirtyeight() +  
  scale_x_continuous(labels=as.character(yoy$year),breaks=yoy$year) + 
  scale_color_manual(values = c('red','blue'), labels = c('California','Creative Economy')) +
  theme(axis.title=element_text(size=12), legend.direction = "horizontal", legend.position = "bottom")
ggsave(filename = "/Users/andrewyu/Documents/Creative Economy/Graphs/CA/ce6.png")
