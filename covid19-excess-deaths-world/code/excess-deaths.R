# -- Libraries
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

# -- Putting everything together
counts <- left_join(dat, pop, by=c("date", "country"))

# -- Control and exclude dates
exclude       <- seq(make_date(2020, 01, 01), make_date(2020, 05, 31), "days")
control_dates <- seq(make_date(2010, 01, 01), make_date(2019, 12, 31), "days")

# -- Check that our expected values fit better (THIS IS JUST ONE EXAMPLE)
x   <- "Netherlands"
tmp <- compute_expected(counts  = filter(counts, country == x),
                        exclude = exclude)
ggplot() +
  geom_point(aes(date, outcome), alpha=0.30, data = tmp) +
  geom_line(aes(date, expected, color="mine"), data = tmp) +
  geom_line(aes(date, expected_deaths, color="them"), data = filter(dat, country == x))

# -- Computing percent_change
percent_change <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
                            filter(country == x) %>%
                            excess_model(.,
                                         start          = make_date(2020, 01, 01),
                                         end            = make_date(2020, 05, 31),
                                         exclude        = exclude,
                                         control.dates  = control_dates,
                                         model          = "correlated",
                                         weekday.effect = FALSE))
  
  
  tibble(date = fit$date, expected = fit$expected, observed = fit$observed, fitted = fit$fitted, se = fit$se) %>%
    mutate(lwr = fitted - 1.96*se, 
           upr = fitted + 1.96*se, 
           country = x)
})

# -- Viz Percent change in mortality 
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

# -- Computing excess deaths
excess_deaths <- map_df(countries, function(x){
  
  print(x)
  fit <- suppressMessages(counts %>%
    filter(country == x) %>%
    excess_model(.,
                 start          = make_date(2020, 01, 01),
                 end            = make_date(2020, 05, 31),
                 exclude        = exclude,
                 control.dates  = control_dates,
                 model          = "correlated",
                 weekday.effect = FALSE))
  
  
  excess_cumulative(fit, start = make_date(2020, 03, 01), end = make_date(2020, 05, 31)) %>%
    mutate(country = x)
})

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

  
