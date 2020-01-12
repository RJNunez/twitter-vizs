####
####    Author: Rolando J. Acosta
####

# -- Libraries
library(tidyverse)
library(lubridate)
library(data.table)

# -- URL where the data is stored
url <- "https://datos.estadisticas.pr/dataset/bc0de091-b513-4b48-bfaa-c238522e180a/resource/953fa091-2fc3-4020-8c3e-77b79c2bc0eb/download/regdem-defunciones-01-enero-2017-hasta-junio-10-2019.csv"

# -- Downloading data
raw <- fread(url)

# -- Wranling municipality level data
towns <- raw %>%
          as_tibble() %>%
          filter(TypeOfDeath == "HOMICIDIO") %>%
          select(Gender, Age, MunicipalityDeathPlace, DeathPlace, DeathDate_Year, DeathDate_Month) %>%
          mutate(Date   = make_date(DeathDate_Year, DeathDate_Month, 01), 
                 Gender = ifelse(Gender == "M", "Male", "Female") ) %>%
          select(-DeathDate_Month, - DeathDate_Year, -DeathPlace) %>%
          setNames(c("Gender", "Age", "Municipality", "Date"))
  
# -- Wrangling island level data
island <- towns %>%
            group_by(Date, Gender) %>%
            summarize(Deaths   = n(),
                      Mean.Age = mean(Age, na.rm = TRUE))

# -- Viz number of monthly deaths
island %>%
  ggplot(aes(Date, Deaths, color=Gender, fill=Gender)) +
  geom_vline(xintercept = make_date(2017,03,01)) +
  geom_vline(xintercept = make_date(2019,07,01)) +
  geom_line() +
  geom_point(size=3, alpha=0.80) +
  geom_point(size=3, pch=1, color="black") +
  geom_smooth(method = "loess") +
  ggtitle("", subtitle = "The black-line correspond to the 2017 World Baseball Classic") +
  scale_y_continuous(breaks = seq(0, 80, by=10)) +
  theme_minimal()

# -- Viz average deaths of those killed each month
island %>%
  ggplot(aes(Date, Mean.Age, color=Gender)) +
  geom_vline(xintercept = make_date(2017,03,01)) +
  geom_line() +
  geom_point(size=3, alpha=0.80) +
  geom_point(size=3, pch=1, color="black") +
  ggtitle("", subtitle = "The black-line correspond to the 2017 World Baseball Classic") +
  scale_y_continuous(breaks = seq(20, 80, by=10)) +
  theme_minimal()


