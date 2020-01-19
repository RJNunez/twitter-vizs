####
####
####    Author: Rolando J. Acosta
####    Data from: https://earthquake.usgs.gov/earthquakes/search/#%7B%22autoUpdate%22%3A%5B%5D%2C%22basemap%22%3A%22grayscale%22%2C%22feed%22%3A%221578075319632%22%2C%22listFormat%22%3A%22default%22%2C%22mapposition%22%3A%5B%5B17.845446982925584%2C-67.79937744140625%5D%2C%5B18.841313810317%2C-66.26129150390625%5D%5D%2C%22overlays%22%3A%5B%22plates%22%5D%2C%22restrictListToMap%22%3A%5B%22restrictListToMap%22%5D%2C%22search%22%3A%7B%22id%22%3A%221578075319632%22%2C%22name%22%3A%22Search%20Results%22%2C%22isSearch%22%3Atrue%2C%22params%22%3A%7B%22starttime%22%3A%222000-01-01%2000%3A00%3A00%22%2C%22endtime%22%3A%222020-01-03%2023%3A59%3A59%22%2C%22maxlatitude%22%3A18.684%2C%22minlatitude%22%3A17.566%2C%22maxlongitude%22%3A-65.094%2C%22minlongitude%22%3A-67.972%2C%22minmagnitude%22%3A1%2C%22orderby%22%3A%22time%22%7D%7D%2C%22sort%22%3A%22newest%22%2C%22timezone%22%3A%22utc%22%2C%22viewModes%22%3A%5B%22list%22%2C%22map%22%5D%2C%22event%22%3Anull%7D
####

#### -- DON'T RUN -- ####
# library(data.table)
# url <- "https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2015-01-01%2000:00:00&endtime=2020-01-18%2023:59:59&maxlatitude=18.684&minlatitude=17.566&maxlongitude=-65.094&minlongitude=-67.972&minmagnitude=1&orderby=time"
# dat <- fread(url)
# dat <- dat %>%
#   as_tibble() %>%
#   mutate(time = gsub("T", " ", time),
#          time = gsub("Z", "", time),
#          time = ymd_hms(time))
# write.csv(dat, file = "data/seismic-activity-pr.csv", row.names = FALSE)
#### -- DON'T RUN -- ####

# -- Libraries
library(tidyverse)
library(lubridate)
library(gganimate)
library(ggrepel)
library(sf)

# -- Loading data
dat_pr   <- read_csv("data/seismic-activity-pr.csv") %>%
  mutate(date = make_date(year(time), month(time), day(time)))

# -- dates to be used later
dates <- expand.grid(2010:2020, 1:12, 1:365) %>%
          as_tibble() %>%
          setNames(c("year", "month", "day")) %>%
          mutate(date = make_date(year, month, day)) %>%
          arrange(date) %>%
          filter(!(month==2 & day==29))

# -- Time line in Mexico
tmp_dat_mex <- dat_mex %>%
  as_tibble() %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  filter(time >= "2017-09-01 00:00:00") %>%
  filter(time <= "2017-09-30 00:00:00") %>%
  filter(latitude <= 22.5) %>%
  arrange(desc(time))

# -- Time line in Mexico
tmp_dat_mex %>%
  ggplot(aes(time, mag, color=mag, alpha=alpha, size=mag, label=label)) +
  geom_point(show.legend = FALSE) +
  geom_point(pch=1, alpha=1, show.legend = FALSE) +
  geom_point(pch=1, alpha=1, color="black", data = filter(tmp_dat_mex, mag>=5.4), show.legend = FALSE) +
  geom_text(data = filter(tmp_dat_mex, mag>=5.4, latitude!=17.9223, longitude != -66.7308), show.legend = FALSE,
            color="black",
            hjust=-0.5,
            alpha=1,
            size=3.5,
            fontface="bold") +
  scale_size(range = c(0,2)) +
  scale_color_gradient(low="#a1d99b", high="red3",
                       limits = c(0,9),
                       breaks = c(1:9)) +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%b %d") +
  scale_y_continuous(breaks = c(1:9), limits=c(1,9)) +
  xlab("") +
  ylab("Magnitude") +
  ggtitle("Eartquakes in the Southern Mexico Area", subtitle = "September 2017") +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(angle=45, hjust=1, face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Looking at latest earthquakes
tmp_dat_pr <- dat_pr %>%
  as_tibble() %>%
  mutate(time = gsub("T", " ", time),
         time = substr(time, 1, 19),
         time = as.POSIXct(time, format="%Y-%m-%d %H:%M:%S")) %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  filter(time >= "2019-12-20 00:00:00") %>%
  arrange(desc(time))

# -- Looking at latest earthquakes
tmp_dat_pr %>%
  ggplot(aes(time, mag, color=mag, alpha=alpha, size=mag, label=label)) +
  geom_point(show.legend = FALSE) +
  geom_point(pch=1, alpha=1, show.legend = FALSE) +
  geom_point(pch=1, alpha=1, color="black", data = filter(tmp_dat_pr, mag>=5), show.legend = FALSE) +
  geom_text(data = filter(tmp_dat_pr, mag>=5, latitude!=17.9223, longitude != -66.7308), show.legend = FALSE,
            color="black",
            hjust=-0.5,
            alpha=1,
            size=3.5,
            fontface="bold") +
  scale_size(range = c(0,2)) +
  scale_color_gradient(low="#a1d99b", high="red3",
                       limits = c(0,7),
                       breaks = c(1:7)) +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%b %d") +
  scale_y_continuous(breaks = c(1:7), limits=c(1,7)) +
  xlab("") +
  ylab("Magnitude") +
  ggtitle("Eartquakes in the Puerto Rico Area", subtitle = "From December 20, 2019 to January 11, 2020") +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(angle=45, hjust=1, face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Average magnitude (15-19)
dat_pr %>%
  group_by(date) %>%
  summarize(avg.mag = mean(mag),
            n = n()) %>%
  ungroup() %>%
  filter(year(date) >= 2015) %>%
  ggplot(aes(date, avg.mag)) +
  geom_point(alpha=0.50) +
  ylab("Average magnitude") +
  xlab("date") +
  ggtitle("Average Magnitude of Earthquakes in the PR Area per day") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title      = element_text(face="bold", color="black"),
        axis.text.x     = element_text(angle=45, hjust=1, face="bold", color="black"),
        axis.text.y     = element_text(face="bold", color="black"),
        legend.text     = element_text(face="bold", color="black"),
        plot.caption    = element_text(face="bold", color="black"),
        plot.title      = element_text(face="bold", color="black")) 

# -- Frequency of earthquakes (15-19)
dat_pr %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  filter(year(date) >= 2015) %>%
  ggplot(aes(date, n)) +
  geom_line(alpha=0.70) +
  scale_y_continuous(breaks = seq(0,200,by=40), limits=c(0,200)) +
  ylab("# of earthquakes") +
  xlab("date") +
  ggtitle("Daily Frequency of Earthquakes in the PR Area") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title      = element_text(face="bold", color="black"),
        axis.text.x     = element_text(angle=45, hjust=1, face="bold", color="black"),
        axis.text.y     = element_text(face="bold", color="black"),
        legend.text     = element_text(face="bold", color="black"),
        plot.caption    = element_text(face="bold", color="black"),
        plot.title      = element_text(face="bold", color="black")) 

# -- Avg. Daily Frequency in last three days of the year
dat_pr %>%
  filter(day(date) %in% c(29:31) & month(date)==12) %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg = mean(n),
            se  = sd(n),
            n = n()) %>%
  ungroup() %>%
  filter(year >= 2014) %>%
  ungroup() %>%
  ggplot(aes(year, avg)) +
  geom_point(size=3, alpha=0.90) +
  geom_point(size=3, pch=1) +
  geom_line(size=1) +
  ylab("Average") +
  xlab("Year") +
  scale_x_continuous(breaks=2010:2019) +
  scale_y_continuous(breaks=seq(0, 160, by=20)) +
  ggtitle("Avg. Daily Frequency of Earthquakes in the PR Area", 
          subtitle = "In the last 3 days of the year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title      = element_text(face="bold", color="black"),
        axis.text.x     = element_text(face="bold", color="black"),
        axis.text.y     = element_text(face="bold", color="black"),
        legend.text     = element_text(face="bold", color="black"),
        plot.caption    = element_text(face="bold", color="black"),
        plot.title      = element_text(face="bold", color="black")) 

# -- Avg. Daily Frequency in first three days of the year
dat_pr %>%
  filter(day(date) %in% c(1:3) & month(date)==1) %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarize(avg = mean(n),
            se  = sd(n),
            n = n()) %>%
  ungroup() %>%
  filter(year >= 2014) %>%
  ungroup() %>%
  ggplot(aes(year, avg)) +
  geom_point(size=3, alpha=0.90) +
  geom_point(size=3, pch=1) +
  geom_line(size=1) +
  ylab("Average") +
  xlab("Year") +
  scale_x_continuous(breaks=2010:2020) +
  scale_y_continuous(breaks=seq(0, 90, by=5)) +
  ggtitle("Avg. Daily Frequency of Earthquakes in the PR Area", 
          subtitle = "In the first 3 days of the year") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.title      = element_text(face="bold", color="black"),
        axis.text.x     = element_text(face="bold", color="black"),
        axis.text.y     = element_text(face="bold", color="black"),
        legend.text     = element_text(face="bold", color="black"),
        plot.caption    = element_text(face="bold", color="black"),
        plot.title      = element_text(face="bold", color="black")) 

# -- Magnitude vs Depth of e-quakes
for(y in sort(unique(year(dat_pr$date))))
{
  cat("Currently at:", y, "\n")
  
  tmp_dat_pr <- dat_pr %>%
    filter(year(date) == y)
  
  p <- tmp_dat_pr %>%
    ggplot(aes(depth, mag)) +
    geom_point(alpha=0.50) +
    xlab("Depth") +
    ylab("Magnitude") +
    scale_x_continuous(limits=c(0,200), breaks=seq(0,200,by=20)) +
    scale_y_continuous(limits=c(1,6), breaks=seq(1,6,by=1)) +
    ggtitle(paste0("Puerto Rico's Seismic Activity in ",y)) +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.title      = element_text(face="bold", color="black"),
          axis.text.x     = element_text(face="bold", color="black"),
          axis.text.y     = element_text(face="bold", color="black"),
          legend.text     = element_text(face="bold", color="black"),
          plot.caption    = element_text(face="bold", color="black"),
          plot.title      = element_text(face="bold", color="black"))
  
  ggsave(filename = paste0("figs/mag-depth-temblores-",y,".pdf"),
         plot = p,
         height = 4, width = 6,
         units = "in",
         dpi = 300)
}

# -- Loading spatial data for Puerto Rico
municipios <- st_read("data/pr-shapefiles/g03_legales_municipios_edicion_octubre2015.shp") %>%
    st_transform(4326)

# -- Drawing PR map (Don't print this, save the figure as below)
pr.map <- ggplot() +
  geom_sf(data = municipios, color="black", fill="#d9d9d9") +
  coord_sf(xlim = c(-67.33, -65.2)) +
  scale_fill_viridis_d(name="") +
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.text  = element_blank(),
        axis.line  = element_blank(),
        axis.ticks = element_blank())

for(y in sort(unique(year(dat_pr$date))))
{
  cat("Currently at:", y, "\n")
  
  tmp_dat_pr <- dat_pr %>%
    filter(year(date) == y)
  
  p <- pr.map + 
    geom_point(aes(longitude, latitude, color=mag), data=tmp_dat_pr, alpha=0.70) +
    scale_color_viridis_c(name="Magnitude", limits = c(1, 5), option="B") +
    scale_y_continuous(limits = c(17, 19)) +
    ggtitle(paste0("Puerto Rico's Seismic Activity in ",y)) +
    theme(legend.text     = element_text(face="bold", color="black"),
          legend.title     = element_text(face="bold", color="black"),
          plot.title     = element_text(face="bold", color="black")) 
  
  ggsave(filename = paste0("figs/temblores-",y,".pdf"),
         plot = p,
         height = 4, width = 6,
         units = "in",
         dpi = 300)
  
  
}