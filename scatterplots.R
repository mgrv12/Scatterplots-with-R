library(ggplot2)
library(dplyr)
library(ggthemes)
library(RColorBrewer)
options(scipen = 999999999)

#Import Data
setwd("your/path/here")

df <- read.csv("data.csv")
df <-as.data.frame(df)

#Creation of metrics for colour coding
df$metric1 <- df$Emissions/df$Population
df$metric2 <- df$Emissions/df$Size

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

df$metric1 <- range01(df$metric1)
df$metric2 <- range01(df$metric2)

#Chart A
df %>% 
  ggplot(aes(x=Population/1000000,y=Emissions/1000)) +
  geom_point(alpha=0.9, aes(color=-metric1,size=Population)) + 
  labs(x="Population (millions)",
       y= "Greenhouse gas emissions (million tonnes)",
       title="Greenhouse gas emissions vs EU Countries population",
       subtitle = "Excluding LULUCF and memory items, including international aviation.",
       caption = c("Axes in logarithmic scale, Colour: Emissions / Population ",
                   "Data: Eurostat.")) +
  scale_y_log10()+
  scale_x_log10()+
  scale_size_continuous(range = c(2.5,5.5))+
  ggrepel::geom_text_repel(aes(label = Country))+
  theme_economist()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=11.5),
        axis.title.y = element_text(size=11.5, hjust=0.4),
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=10.5,hjust = 0),
        plot.caption = element_text(size=9, hjust = c(0,1)))


#Chart B
df %>% 
  ggplot(aes(x=Size,y=Emissions/1000)) +
  geom_point(alpha=0.9, aes(color=-metric2,size=Size)) + 
  labs(x="Size (thousand square kilometers)",
       y= "Greenhouse gas emissions (million tonnes)",
       title="Greenhouse gas emissions vs EU Countries size",
       subtitle = "Excluding LULUCF and memory items, including international aviation.",
       caption = c("Axes in logarithmic scale, Colour: Emissions / Country size ",
                   "Data: Eurostat.")) +
  scale_y_log10()+
  scale_x_log10()+
  scale_size_continuous(range = c(2.5,5.5))+
  ggrepel::geom_text_repel(aes(label = Country))+
  theme_economist()+
  theme(legend.position = "none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10),
        axis.title.x = element_text(size=11.5),
        axis.title.y = element_text(size=11.5, hjust=0.4),
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=10.5,hjust = 0),
        plot.caption = element_text(size=9, hjust = c(0,1)))
