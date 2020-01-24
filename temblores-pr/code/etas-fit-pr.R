####
####
####    Author: Rolando J. Acosta
####    Data from: https://earthquake.usgs.gov/fdsnws/event/1/query.csv?starttime=2014-01-01%2000:00:00&endtime=2020-01-15%2023:59:59&maxlatitude=18.684&minlatitude=17.566&maxlongitude=-65.094&minlongitude=-67.972&minmagnitude=1&orderby=time
####

# -- Libraries
library(tidyverse)
library(lubridate)
library(ggrepel)
library(leaflet)
library(scales)
library(ETAS)
library(sf)

# -- Loading Puerto Rico's earthquake data
pr.quakes <- read_csv("temblores-pr/data/seismic-activity-pr.csv") %>%
  separate(time, c("date", "time"), sep=" ") %>%
  select(date, time, longitude, latitude, mag) %>%
  setNames(c("date", "time", "long", "lat", "mag")) %>%
  as.data.frame() %>%
  arrange(date, time)

# -- Creating a catalog object
pr.quakes %>% head()
pr.cat <- catalog(pr.quakes, 
                  # time.begin    = "2015/01/01",
                  # study.start   = "2020/01/7",
                  # study.end     = "2020/01/21",
                  lat.range     = c(17,19),
                  long.range    = c(-68, -65),
                  mag.threshold = 3)

# -- Summary of the catalog object
print(pr.cat)

# -- Viz: Summary of catalog
plot(pr.cat)

# -- Fitting ETAS model (This takes a while to run)
nthreads <- parallel::detectCores()
pr.fit   <- etas(pr.cat, nthreads=nthreads)

# -- Results of ETAS
print(pr.fit)
plot(pr.fit)
rates(pr.fit)
resid.etas(pr.fit)

pr <- probs(pr.fit)
summary(pr$prob)

plot(pr.cat$longlat.coord[pr$target & (1 - pr$prob > 0.95), 1:2])
points(pr.cat$longlat.coord[pr$target & (pr$prob > 0.95), 1:2], pch = 3, col = 2)
map("world", add = TRUE, col = "grey")
legend("bottomleft", c("background", "triggered"), pch = c(1, 3), col = 1:2)

pr.res <- resid.etas(pr.fit)
ks.test(pr.res$U, punif)

##########################################################################################################
##########################################################################################################

# -- Viz: Latitude vs Date (Fig 2)
pr.quakes %>%
  as_tibble() %>%
  filter(date >= "2019-12-20") %>%
  mutate(date.time = ymd_hms(paste(date, time))) %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  arrange((mag)) %>%
  ggplot(aes(date.time, lat, color=mag, alpha=alpha, size=mag, label=label)) +
  geom_point(show.legend = FALSE) +
  scale_size(range = c(0,2)) +
  xlab("") +
  ylab("Latitude") +
  ggtitle("Latitude of Earthquakes in the PR area", 
          subtitle = "Since Dec. 20, 2019") +
  scale_color_gradient(low="#a1d99b", high="red3",
                       limits = c(0,7),
                       breaks = c(1:7)) +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(17.5, 18.7), 
                     breaks=seq(17.5, 18.7,by=0.20)) +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(angle=45, hjust=1, face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Viz: Longitude vs Date (Fig 3)
pr.quakes %>%
  as_tibble() %>%
  filter(date >= "2019-12-20") %>%
  mutate(date.time = ymd_hms(paste(date, time))) %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  arrange((mag)) %>%
  ggplot(aes(date.time, long, color=mag, alpha=alpha, size=mag, label=label)) +
  geom_point(show.legend = FALSE) +
  scale_size(range = c(0,2)) +
  xlab("") +
  ylab("Longitude") +
  ggtitle("Longitude of Earthquakes in the PR area", 
          subtitle = "Since Dec. 20, 2019") +
  scale_color_gradient(low="#a1d99b", high="red3",
                       limits = c(0,7),
                       breaks = c(1:7)) +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(-67.8, -65.1),
                     breaks=seq(-67.8, -65.1,by=0.30)) +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(angle=45, hjust=1, face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Viz: Magnitude vs Date (Fig 4)
tmp_dat <- pr.quakes %>%
  as_tibble() %>%
  # filter(mag >= 3) %>%
  filter(date >= "2019-12-20") %>%
  mutate(date.time = ymd_hms(paste(date, time))) %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  arrange((mag))
tmp_dat %>%
  ggplot(aes(date.time, mag, color=mag, alpha=alpha, size=mag, label=label)) +
  geom_point(show.legend = FALSE) +
  geom_text(data = filter(tmp_dat, mag>=5, lat!=17.9223, long != -66.7308), show.legend = FALSE,
            color="black",
            hjust=-0.5,
            alpha=1,
            size=3.5,
            fontface="bold") +
  scale_size(range = c(0,2)) +
  xlab("") +
  ylab("Longitude") +
  ggtitle("Earthquakes in the Puerto Rico Area", 
          subtitle = "Since Dec. 20, 2019") +
  scale_color_gradient(low="#a1d99b", high="red3",
                       limits = c(0,7),
                       breaks = c(1:7)) +
  scale_x_datetime(date_breaks = "1 days", date_labels = "%b %d") +
  scale_y_continuous(limits=c(1, 7),
                     breaks=seq(1, 7, by=1)) +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(angle=45, hjust=1, face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Viz: Assesing Guttenberg-Richter Law (Fig 5)
mags <- sort(filter(pr.quakes, date >= "2019-12-20")$mag)
Nm   <- rep(NA, length(mags))
for(i in 1:length(mags))
{
  Nm[i] <- sum(mags[i] <= mags)
}
a <- coef(lm(log10(Nm)~mags))[1]
b <- coef(lm(log10(Nm)~mags))[2]

# -- Guttenberg-Richter Law I
tibble(Nm = Nm, m = mags) %>%
  unique() %>%
  ggplot(aes(m, log10(Nm))) +
  geom_point(alpha=0.50) +
  geom_abline(intercept = a, slope = b, color="blue2", lty=2) +
  scale_x_continuous(breaks = seq(1, 7, by=1)) +
  scale_y_continuous(limits = c(0, 3.50),
                     breaks = seq(0, 3.50, by=0.5)) +
  xlab("Magnitude") +
  ylab(expression(log[10] * " " * N[m])) +
  ggtitle("Gutenberg-Richter Law") +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(face="bold", color="#525252"),
        axis.title  = element_text(color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Guttenberg-Richter Law II
tibble(Nm = Nm, m = mags) %>%
  unique() %>%
  ggplot(aes(m, 10^(a+b*m))) +
  geom_line(size=1) +
  scale_y_continuous(limits=c(0,4250),
                     breaks=seq(0,4000,by=500)) +
  scale_x_continuous(breaks=c(1:7)) +
  xlab("Magnitude (m)") +
  ylab(expression("Frequency of events with magnitude > m ("*N[m]*")")) +
  ggtitle("Theoretical Frequency of Events in the Puerto Rico Area",
          subtitle = "Gutenberg-Ritcher Law") +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(face="bold", color="#525252"),
        axis.title  = element_text(color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Viz: Assesing the Stationarity of the Earthquake Process (Fig 6)
pr.quakes %>%
  as_tibble() %>%
  mutate(date = ymd(date)) %>%
  filter(date >= "2019-12-20") %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  mutate(Nt   = cumsum(n),
         t = as.numeric(date - min(date))) %>%
  ggplot(aes(t, Nt)) +
  geom_vline(xintercept = 8, color="red3", lty=2) +
  geom_vline(xintercept = 26, color="red3", lty=2) +
  geom_line(size=1) +
  geom_abline(intercept = -378.79, slope = 80.49, color="blue2", lty=2) +
  scale_y_continuous(limits = c(0,1670), breaks = seq(0, 1600, by=200)) +
  scale_x_continuous(limits = c(0,26), breaks = seq(0, 26, by=2)) +
  ylab("Seismic activity up to t (Nt)") +
  xlab("Days since Dec. 20 (t)") +
  ggtitle("Assesing the Stationarity of the Earthquake Process") +
  theme_minimal() +
  theme(axis.line   = element_blank(),
        axis.ticks  = element_line(color="#525252"),
        axis.text.y = element_text(face="bold", color="#525252"),
        axis.text.x = element_text(face="bold", color="#525252"),
        axis.title  = element_text(face="bold", color="#525252"),
        plot.title  = element_text(face="bold", color="#525252"),
        plot.subtitle = element_text(face="bold", color="#525252"))

# -- Geographical Distribution of temblores in PR in 2020
bins <- c(1:7)
pal  <- colorBin("YlOrRd", domain = filter(pr.quakes, year(date)==2020)$mag, bins = bins)

pr.quakes %>%
  filter(year(date)==2020) %>%
  mutate(alpha = mag - min(mag),
         alpha = alpha / max(alpha),
         label = paste0(round(mag,2))) %>%
  arrange((mag)) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircles(lng     = ~long, 
             lat     = ~lat, 
             color   = ~pal(mag), 
             opacity = ~alpha,
             weight  = ~mag) %>%
  addLegend(pal      = pal, 
            values   = ~mag, 
            opacity  = 0.7, 
            title    = "Magnitude",
            position = "bottomright")