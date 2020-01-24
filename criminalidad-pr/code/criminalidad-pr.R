####
####    Author: Rolando J. Acosta
####

# -- Libraries
library(tidyverse)
library(lubridate)
library(ggthemes)
library(leaflet)
library(mapview)

# -- Loading & Wrangling data
dat <- read_csv("criminalidad-pr/data/criminalidad.csv")

# -- Carjackings
dat %>%
  group_by(Year, Month, Delito, Region) %>%
  summarize(Events = sum(Events)) %>%
  ungroup() %>%
  arrange(desc(Events)) %>%
  ungroup() %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  arrange(Date) %>%
  filter(Delito == "Vehiculo Hurtado") %>%
  filter(Date >= "2017-01-01" & Date <= "2018-07-01") %>%
  filter(Region %in% c("Área de Carolina", "Área de Caguas", "Área de San Juan", "Área de Bayamón")) %>%
  ggplot(aes(Date, (Events))) +
  geom_vline(xintercept = make_date(2017,09,01), color="#ca0020", lty=2) + 
  geom_line() +
  geom_point(alpha=0.90, size=3) +
  geom_point(pch=1, color="black", size=3) +
  facet_wrap(~Region) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by=30)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme_fivethirtyeight() +
  ylab("# de Carjackings") +
  xlab("") +
  ggtitle("Carjackings en el Área Metroplolitana de Puerto Rico",
          subtitle = "Total mensual de carjackings") +
  labs(caption = "Data from Jan 2017 to July 2018") +
  theme(plot.title    = element_text(face="bold", color="black"),
        plot.subtitle = element_text(face="bold", color="black", size=10),
        strip.text    = element_text(face="bold", color="black"),
        
        axis.text.x   = element_text(angle=45, hjust=1, size=8, face="bold"),
        axis.text.y   = element_text(face="bold"),
        axis.title    = element_text(face="bold"),
        
        legend.text     = element_text(face="bold", color="black"),
        legend.title    = element_text(face="bold", color="black", size=9),
        legend.position = "bottom")

# -- Robos
dat %>%
  group_by(Year, Month, Delito, Region) %>%
  summarize(Events = sum(Events)) %>%
  ungroup() %>%
  arrange(desc(Events)) %>%
  ungroup() %>%
  mutate(Date = make_date(Year, Month, 1)) %>%
  arrange(Date) %>%
  filter(Delito == "Robo") %>%
  filter(Date >= "2017-01-01" & Date <= "2018-07-01") %>%
  filter(Region %in% c("Área de Carolina", "Área de Caguas", "Área de San Juan", "Área de Bayamón")) %>%
  ggplot(aes(Date, (Events))) +
  geom_vline(xintercept = make_date(2017,09,01), color="#ca0020", lty=2) + 
  geom_line() +
  geom_point(alpha=0.90, size=3) +
  geom_point(pch=1, color="black", size=3) +
  facet_wrap(~Region) +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by=15)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "1 months") +
  theme_fivethirtyeight() +
  ylab("# de Robos") +
  xlab("") +
  ggtitle("Robos en el Área Metroplolitana de Puerto Rico",
          subtitle = "Total mensual de robos") +
  labs(caption = "Data from Jan 2017 to July 2018") +
  theme(plot.title    = element_text(face="bold", color="black"),
        plot.subtitle = element_text(face="bold", color="black", size=10),
        strip.text    = element_text(face="bold", color="black"),
        
        axis.text.x   = element_text(angle=45, hjust=1, size=8, face="bold"),
        axis.text.y   = element_text(face="bold"),
        axis.title    = element_text(face="bold"),
        
        legend.text     = element_text(face="bold", color="black"),
        legend.title    = element_text(face="bold", color="black", size=9),
        legend.position = "bottom")

# -- Geo. distribution de Carjackings, robos y asesinatos en PR
delitos <- c("Vehiculo Hurtado", "Robo", "Asesinato")
pal     <- colorFactor(c("#00AFBB", "black", "#FC4E07"), domain = delitos)

# -- 2018
a <- leaflet(data = filter(dat, Year == 2018 & Delito %in% delitos)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = 2,
                   popup = ~paste0(Region, " : ", Delito),
                   fillOpacity = 0.50,
                   stroke = FALSE,
                   color  = ~pal(Delito)) %>%
  addLegend("bottomright", 
            pal=pal, 
            values = delitos, 
            title = "Crimenes en 2018",
            opacity = 1)

# -- 2017
b <- leaflet(data = filter(dat, Year == 2017 & Delito %in% delitos)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = 2,
                   popup = ~paste0(Region, " : ", Delito),
                   fillOpacity = 0.50,
                   stroke = FALSE,
                   color  = ~pal(Delito)) %>%
  addLegend("bottomright", 
            pal=pal, 
            values = delitos, 
            title = "Crimenes en 2017",
            opacity = 1)

# -- 2016
c <- leaflet(data = filter(dat, Year == 2016 & Delito %in% delitos)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = 2,
                   popup = ~paste0(Region, " : ", Delito),
                   fillOpacity = 0.50,
                   stroke = FALSE,
                   color  = ~pal(Delito)) %>%
  addLegend("bottomright", 
            pal=pal, 
            values = delitos, 
            title = "Crimenes en 2016",
            opacity = 1)

# -- 2015
d <- leaflet(data = filter(dat, Year == 2015 & Delito %in% delitos)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(radius = 2,
                   popup = ~paste0(Region, " : ", Delito),
                   fillOpacity = 0.50,
                   stroke = FALSE,
                   color  = ~pal(Delito)) %>%
  addLegend("bottomright", 
            pal=pal, 
            values = delitos, 
            title = "Crimenes en 2015",
            opacity = 1)

# -- Viz
sync(a,b,c,d)