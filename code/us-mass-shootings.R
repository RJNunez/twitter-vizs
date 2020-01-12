####
####    Author: Rolando J. Acosta
####

# -- Libraries
library(tidyselect)
library(tidyverse)
library(lubridate)
library(ggthemes)

# -- Loading data
dat <- read_csv("data/us-shootings.csv")

# -- Some data wrangling
dat <- dat %>% 
        separate(date, c("Month", "Day", "Year"), sep = "/") %>%
        mutate(Month  = as.numeric(Month),
               Day    = as.numeric(Day),
               Year   = as.numeric(Year),
               Year   = Year + c(rep(2000, 26), rep(0, 88)),
               gender = ifelse(gender == "M", "Male", gender),
               gender = ifelse(gender == "F", "Female", gender),
               Date   = make_date(Year, Month, Day),
               weapons_obtained_legally = ifelse(weapons_obtained_legally == "\nYes", "Yes", weapons_obtained_legally),
               weapons_obtained_legally = ifelse(weapons_obtained_legally == "Yes (\"some of the weapons were purchased legally and some of them may not have been\")", "Yes", weapons_obtained_legally),
               weapon_type              = str_to_lower(weapon_type)) %>%
        arrange(Date) %>%
        separate(location, c("Location", "State"), sep=", ")

# -- Types of fire arms
types <- unique(dat$weapon_type)

# -- More data wrangling
dat   <- dat %>%
          mutate(semi.rifle    = ifelse(weapon_type %in% vars_select(types, contains("semiautomatic rifle")), "Yes", "No"),
                 semi.rifle    = factor(semi.rifle, levels = c("Yes", "No")),
                 semiautomatic = ifelse(weapon_type %in% vars_select(types, contains("semiautomatic")), "Yes", "No"),
                 semiautomatic = factor(semiautomatic, levels = c("Yes", "No")))

# -- Some fixes
dat[91,]$semi.rifle    <- "Yes"
dat[91,]$semiautomatic <- "Yes"

# -- Mores fixes
tmp_states <- dat$State
tmp_states <- sapply(tmp_states, function(x){
  if(nchar(x) == 2)
  {
    name <- state.name[state.abb == x]
    return(name)
  }
  name <- x
  return(name)
})
dat        <- mutate(dat, State = tmp_states)

# -- Frequency of mass shootings in the US since 1982
dat %>%
  group_by(State) %>%
  summarize(Times = n()) %>%
  ungroup() %>%
  arrange(desc(Times)) %>%
  ggplot(aes(reorder(State, Times), Times)) +
  geom_col(color="black", fill="#525252") + 
  scale_y_continuous(breaks = seq(0, 20, by=1)) + 
  xlab("States") +
  ylab("# of mass-shootings since 1982") +
  coord_flip() +
  ggtitle("Frequency of US Mass Shootings since 1982") +
  labs(caption = "Data from 1982 to 2019") +
  theme_fivethirtyeight() +
  theme(plot.title    = element_text(face="bold", color="black"),
        plot.caption  = element_text(face="bold", color="black"),
        plot.subtitle = element_text(face="bold", color="black", size=10),
        strip.text    = element_text(face="bold", color="black"),
        
        axis.text     = element_text(face="bold", color="black"),
        axis.title    = element_text(face="bold", color="black"),
        
        legend.text     = element_text(face="bold", color="black"),
        legend.title    = element_text(face="bold", color="black", size=9),
        legend.position = "bottom")

# -- EDA on US mass shootings
dat %>%
  filter(weapons_obtained_legally %in% c("No", "Yes")) %>%
  mutate(weapons_obtained_legally = ifelse(weapons_obtained_legally == "Yes", "Weapons Obtained Legally: Yes", "Weapons Obtained Legally: No"),
         weapons_obtained_legally = factor(weapons_obtained_legally, levels = c("Weapons Obtained Legally: Yes", "Weapons Obtained Legally: No"))) %>%
  ggplot(aes(Date, fatalities, pch=semiautomatic, color=semi.rifle)) +
  geom_point(alpha=0.80, size=3) +
  facet_wrap(~weapons_obtained_legally) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0,60,by=10)) +
  scale_shape_manual(name = "Semiautomatic Weapon: ",
                     values = c(16, 17)) +
  scale_color_manual(name = "Semiautomatic Rifle: ",
                     values = c("#ca0020", "#252525")) +
  theme_fivethirtyeight() +
  labs(caption = "Semiautomatic Rifle: Yes = Red and No = Black") +
  ggtitle("Mass Shooting in the US") +
  ylab("# of Deaths") +
  xlab("") +
  guides(color=FALSE) +
  theme(plot.title    = element_text(face="bold", color="black"),
        plot.caption  = element_text(face="bold", color="black"),
        plot.subtitle = element_text(face="bold", color="black", size=10),
        strip.text    = element_text(face="bold", color="black"),
        axis.text.y     = element_text(face="bold", color="black"),
        axis.text.x     = element_text(angle=45, hjust=1, face="bold", color="black"),
        axis.title    = element_text(face="bold", color="black"),
    
        legend.text     = element_text(face="bold", color="black"),
        legend.title    = element_text(face="bold", color="black", size=9),
        legend.position = "bottom")