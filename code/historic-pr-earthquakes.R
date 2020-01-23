# -- Libraries
library(tidyverse)
library(lubridate)
library(ggrepel)

# -- Loading data
dat <- read.delim("data/historic-earthquakes.txt")

# -- Looking at earthquakes in PR
pr.quakes <- dat %>%
  as_tibble() %>%
  filter(grepl("PUERTO RICO", LOCATION_NAME)) %>%
  mutate(EQ_PRIMARY = ifelse(YEAR==1867 & MONTH==3, 7.3, EQ_PRIMARY)) %>%
  select(YEAR, MONTH, DAY, HOUR, MINUTE, SECOND, FOCAL_DEPTH, EQ_PRIMARY)

# -- Viz
pr.quakes %>%
  filter(!is.na(EQ_PRIMARY)) %>%
  ggplot(aes(YEAR, EQ_PRIMARY, label = paste("Year =",YEAR,"\n Mag =",EQ_PRIMARY))) +
  geom_point(size=4) +
  geom_label_repel() +
  ylab("Magnitude")
  
