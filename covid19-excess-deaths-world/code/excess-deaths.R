# -- Libraries
library(directlabels)
library(excessmort)
library(data.table)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(scales)
library(readxl)

### --------- ------------------------------------ ---------
### --------- Loading and wrangling mortality data ---------
### --------- ------------------------------------ ---------
url <- "https://raw.githubusercontent.com/Financial-Times/coronavirus-excess-mortality-data/master/data/ft_excess_deaths.csv"
dat <- fread(url) %>% 
  as_tibble() %>%
  mutate(date = ymd(date))

# -- Countries to take out because of insuficient data
out <- c("Brazil")

# -- Subsetting data
dat <- filter(dat, country == region, !country %in% out) %>%
        select(date, deaths, expected_deaths, country) %>%
        rename(outcome = deaths) %>%
        mutate(country = case_when(country=="US" ~ "United States of America",
                                   country=="UK" ~ "United Kingdom",
                                   TRUE ~ country))
countries <- unique(dat$country)
### --------- ------------------------------------ ---------
### --------- Loading and wrangling mortality data ---------
### --------- ------------------------------------ ---------

### --------- ------------------------------------- ---------
### --------- Loading and wrangling population data ---------
### --------- ------------------------------------- ---------
pop <- read_excel("covid19-excess-deaths-world/data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")

# -- Retrieving columns names
names <- pop[12,]

# -- Wrangle
pop <- pop %>%
  slice(-c(1:12)) %>%
  setNames(names) %>%
  select(`Region, subregion, country or area *`, contains("201"), `2020`) %>%
  filter(`Region, subregion, country or area *` %in% countries) %>%
  rename(country = `Region, subregion, country or area *`) %>%
  gather(year, population, -country) %>%
  mutate(population = as.numeric(population),
         population = population * 1000) %>%
  arrange(country, year)

# -- Interpolation population values
pop <- approx_demographics(pop, 
                    first_day = make_date(2010, 01, 01),
                    last_day  = make_date(2020, 12, 31),
                    by        = c("year", "country"))
### --------- ------------------------------------- ---------
### --------- Loading and wrangling population data ---------
### --------- ------------------------------------- ---------

### --------- ----------------------------- ---------
### --------- Percent increase in mortality ---------
### --------- ----------------------------- ---------
# -- Putting everything together
counts <- left_join(dat, pop, by=c("date", "country"))

# -- Control and exclude dates
exclude       <- seq(make_date(2020, 01, 01), make_date(2020, 05, 31), "days")
control_dates <- seq(make_date(2010, 01, 01), make_date(2019, 12, 31), "days")

# -- Computing percent_change
percent_change <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
                            filter(country == x) %>%
                            excess_model(.,
                                         start          = make_date(2020, 01, 01),
                                         end            = make_date(2020, 06, 30),
                                         exclude        = exclude,
                                         control.dates  = control_dates,
                                         model          = "correlated",
                                         weekday.effect = FALSE))
  
  
  tibble(date = fit$date, expected = fit$expected, observed = fit$observed, fitted = fit$fitted, se = fit$se) %>%
    mutate(lwr = fitted - 1.96*se, 
           upr = fitted + 1.96*se, 
           country = x)
})

# -- Three worst countries
top_3 <- c("Spain", "Peru", "United Kingdom")

# -- Percent change visualization 1
ggplot() +
  geom_hline(yintercept = 0, lty = 2, alpha = 0.40) +
  geom_line(aes(date, fitted, group=country), color="gray", alpha=0.50, data = percent_change) +
  # geom_ribbon(aes(date, ymin=lwr, ymax=upr, fill=country), alpha=0.50, data = filter(percent_change, country %in% top_3)) +
  geom_line(aes(date, fitted, color=country), size=0.80, show.legend = FALSE, data = filter(percent_change, country %in% top_3)) +
  geom_dl(aes(date, fitted, color=country, label=country), method=list(fontface="bold", "last.points"), data = filter(percent_change, country %in% top_3)) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(-0.20, 1.30), 
                     breaks = seq(-0.20, 1.20, by=0.20)) +
  scale_x_date(date_breaks = "1 months",
               date_labels = "%b %d",
               limits = c(make_date(2020,01,01), make_date(2020,07,15))) +
  scale_color_manual(name   = "",
                     values = c("#252525", "#cb181d", "#2171b5")) +
  ylab("Percent change from average") +
  xlab("Date") +
  ggtitle("Percent Increase in Mortality from Average") +
  theme_bw()

# -- Percent change visualization 2
percent_change %>%
  ggplot(aes(date, fitted)) +
  geom_hline(yintercept = 0, lty=2, color="red2") +
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.60, fill="black") +
  geom_line() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  ylab("Percent increase from expected mortality") +
  xlab("Date") +
  ggtitle("Pecent Increase in Mortality", subtitle = "Jan 2020 to May 2020") +
  facet_wrap(~country) +
  theme_minimal() +
  theme(axis.text.x  = element_text(angle=45, hjust=1, face = "bold", color = "black"),
        axis.text.y  = element_text(face = "bold", color = "black"),
        strip.text   = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold", color = "black", size=18),
        plot.subtitle = element_text(face = "bold", color = "black"))
### --------- ----------------------------- ---------
### --------- Percent increase in mortality ---------
### --------- ----------------------------- ---------

### --------- ---------------------- ---------
### --------- Excess death estiamtes ---------
### --------- ---------------------- ---------
# -- Computing excess deaths
excess_deaths <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
    filter(country == x) %>%
    excess_model(.,
                 start          = make_date(2020, 03, 01),
                 end            = make_date(2020, 05, 31),
                 exclude        = exclude,
                 control.dates  = control_dates,
                 model          = "correlated",
                 weekday.effect = FALSE))
  
  
  excess_cumulative(fit, start = make_date(2020, 01, 01), end = make_date(2020, 06, 30)) %>%
    mutate(country = x)
}) %>% 
  as_tibble() %>%
  mutate(lwr = fitted - 1.96 * se, 
         upr = fitted + 1.96 * se)

# -- Viz cumulative excess deaths
excess_deaths %>%
  as_tibble() %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  mutate(fitted = fitted / 1000) %>%
  ggplot(aes(reorder(country, fitted), fitted, label = round(fitted, 2))) +
  geom_col(color="black", width = 0.80, fill="#252525", size=0.10) +
  ylab("Cumulative excess deaths (thousands)") +
  xlab("") +
  ggtitle("Cumulative Excess Deaths", subtitle = "Mar 2020 to May 2020") +
  geom_label(fill        = "white",
             fontface    = "bold",
             size        = 3,
             color       = "#a50f15",
             show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text  = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black"),
        plot.title = element_text(face = "bold", color = "black", size=18),
        plot.subtitle = element_text(face = "bold", color = "black"))

# -- Viz cumulative excess deaths per 100,000
excess_deaths %>%
  as_tibble() %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  left_join(pop, by = c("date", "country")) %>%
  mutate(rate = fitted / population * 100000) %>%
  ggplot(aes(reorder(country, rate), rate, label = round(rate, 2))) +
  geom_col(color="black", width = 0.80, fill="#252525", size=0.10) +
  ylab("Cumulative excess deaths per 100,000 people") +
  xlab("") +
  ggtitle("Cumulative Excess Deaths per 100,000", subtitle = "Mar 2020 to May 2020") +
  geom_label(fill        = "white",
             fontface    = "bold",
             size        = 3,
             color       = "#a50f15",
             show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text  = element_text(face = "bold", color = "black"),
        axis.title = element_text(face = "bold", color = "black"),
        plot.title    = element_text(face = "bold", color = "black", size=18),
        plot.subtitle = element_text(face = "bold", color = "black"))
### --------- ---------------------- ---------
### --------- Excess death estiamtes ---------
### --------- ---------------------- ---------

### --------- --------------------------------------------------------------- ---------
### --------- Comparsiong b|w excess deaths and covid19 for several countries ---------
### --------- --------------------------------------------------------------- ---------
# -- Loading covid19 data from european center for disease control
eudat <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
                  na.strings = "", stringsAsFactors = FALSE,
                  fileEncoding = "UTF-8-BOM") %>%
  as_tibble() %>%
  mutate(date = dmy(dateRep)) %>%
  select(date, cases, deaths, countriesAndTerritories, popData2018) %>%
  rename(country = countriesAndTerritories) %>%
  arrange(date, country) %>%
  filter(deaths >= 0) %>%
  group_by(country) %>%
  mutate(covid19 = cumsum(deaths),
         country = gsub("_", " ", country)) %>%
  ungroup() %>%
  select(date, country, deaths, covid19)

# -- Countries in this dataset
eu_countries <- unique(eudat$country)

# -- All countries
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  group_by(country) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(difference = fitted - covid19,
         lwr = difference - 1.96 * se,
         upr = difference + 1.96 * se,
         flag = difference > TRUE) %>%
  ggplot(aes(reorder(country, difference), difference, color=flag)) +
  geom_hline(yintercept = 0, lty=2, color="#252525") +
  geom_errorbar(aes(ymin=lwr, ymax=upr), width = 0, show.legend = F) +
  geom_point(size=2, show.legend = F) +
  geom_point(size=2, pch=1, color="black") +
  coord_flip() +
  ylab("Difference") +
  xlab("") +
  ggtitle("Difference between Excess and Covid19 deaths",
          subtitle = "by May 2020") +
  scale_color_manual(name = "",
                     values = c("#2171b5", "#cb181d")) +
  scale_y_continuous(labels = scales::comma,
                     breaks = seq(-20000, 40000, by=10000)) +
  theme_minimal() +
  theme(axis.title = element_text(face="bold"),
        axis.text  = element_text(face="bold"),
        plot.title     = element_text(face="bold"),
        plot.subtitle  = element_text(face="bold"))

# -- Italy: Excess deaths vs covid 19
x <- "Italy"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)


# -- Spain: Excess deaths vs covid 19
x <- "Spain"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                     values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)
  
# -- Peru: Excess deaths vs covid 19
x <- "Peru"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)

# -- United Kingdom: Excess deaths vs covid 19
x <- "United Kingdom"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)

# -- Sweden: Excess deaths vs covid 19
x <- "Sweden"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)

# -- Belgium: Excess deaths vs covid 19
x <- "Belgium"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)

# -- France: Excess deaths vs covid 19
x <- "France"
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  ggplot(aes(date, fitted)) +
  geom_ribbon(aes(ymin=lwr, ymax=upr, fill="Excess \ndeaths"), alpha=0.50, show.legend = FALSE) +
  geom_line(aes(color="Excess \ndeaths")) +
  geom_line(aes(date, covid19, color="Covid19 \ndeaths")) +
  scale_color_manual(name="",
                     values = c("#252525", "#cb181d")) +
  scale_fill_manual(name="",
                    values = c("#cb181d")) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%b %d", limits = c(ymd("2020-03-01"), ymd("2020-06-01")),
               breaks = c(ymd("2020-03-01"), ymd("2020-03-15"),
                          ymd("2020-04-01"), ymd("2020-04-15"),
                          ymd("2020-05-01"), ymd("2020-05-15"),
                          ymd("2020-06-01"))) +
  ylab("Cumulative deaths") +
  xlab("Date") +
  ggtitle(paste0("Excess deaths vs Covid-19 deaths in ", x), subtitle = "Mar 2020 to May 2020") +
  theme_minimal() +
  theme(legend.position   = c(0.20, 0.80),
        legend.text       = element_text(face="bold"),
        legend.direction  = "horizontal",
        legend.title      = element_blank(),
        legend.background = element_rect(color="black"))
excess_deaths %>%
  left_join(eudat, by=c("date", "country")) %>%
  filter(country == x) %>%
  filter(date == max(date)) %>%
  mutate(dif = fitted - covid19, 
         lwr = dif - 1.96 * se,
         upr = dif + 1.96 * se) %>%
  select(dif, lwr, upr)
### --------- --------------------------------------------------------------- ---------
### --------- Comparsiong b|w excess deaths and covid19 for several countries ---------
### --------- --------------------------------------------------------------- ---------
