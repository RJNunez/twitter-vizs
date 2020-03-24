# -- Libraries
library(tidyverse)
library(lubridate)
library(ggrepel)

# -- Loading & wrangling data
dat <- read_csv("covid19-pr/dat/Database.csv") %>%
  setNames(c("id", "sex", "region", "age", "result", "date", "source")) %>%
  separate(date, c("day", "month.name")) %>%
  mutate(day = as.numeric(day))  %>%
  mutate(source = ifelse(source=="DS\r", "DS", source),
         source = ifelse(source=="HV\r", "HV", source)) %>%
  mutate(result = factor(result, levels=c("Positivo", "Negativo", "Pendiente")))

# -- Age distribution of tests in DS
dat %>%
  filter(source == "DS") %>%
  ggplot() +
  geom_boxplot(aes(sex, age), fill="gray", color="black", size=0.50) +
  geom_jitter(aes(sex, age), width=0.10, alpha=0.70) +
  facet_wrap(~result) +
  scale_x_discrete(labels = c("F"="Female", "M"="Male")) +
  xlab("") +
  ylab("Age") +
  theme_bw() +
  theme(axis.title = element_text(face="bold"),
        axis.text  = element_text(face="bold"),
        strip.text = element_text(face="bold"))

# -- Numero de pruebas por dia con resultados en DS
num_test <- dat %>%
  filter(source == "DS") %>%
  filter(result != "Pendiente") %>%
  group_by(day) %>%
  summarize(n = n()) %>%
  mutate(num.test = cumsum(n)) %>%
  ungroup() %>%
  select(-n)

# -- Porciento de positivos y negativos por covid19 en DS
tmp <- dat %>%
  filter(source == "DS") %>%
  filter(result != "Pendiente") %>%
  group_by(result, day) %>%
  summarize(n = n()) %>%
  mutate(cum.n = cumsum(n)) %>%
  ungroup() %>%
  left_join(num_test, by = "day") %>%
  mutate(percent = 100 * cum.n / num.test)

# -- Viz de porciento de positivos y negativos por covid19 en DS
tmp %>%
  ggplot(aes(day, percent, color=result, label=round(percent,1))) +
  geom_line(size=0.80, ) +
  geom_point(size=3, show.legend = F) +
  geom_label(fontface    = "bold", 
             label.size    = 0.60,
             show.legend = FALSE) +
  ylab("Porcentaje") +
  xlab("Dias de marzo") +
  ggtitle("Porciento de Casos Positivos/Negativos",
          subtitle = "Prubas hechas por Departamento de Salud de Puerto Rico") +
  labs(caption = "Rolando J. Acosta (@RJANunez)") +
  scale_x_continuous(limits = c(9, 21), breaks = seq(9, 21, by=1)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by=20)) +
  scale_color_manual(name="",
                     values=c("#3182bd", "#cb181d")) +
  theme_fivethirtyeight() +
  theme(axis.title  = element_text(face="bold", color="black"),
        axis.text   = element_text(face="bold", color="black"),
        legend.text = element_text(face="bold", color="black"),
        plot.title  = element_text(face="bold", color="black", size=12),
        plot.subtitle  = element_text(face="bold", color="black", size=9),
        plot.caption      = element_text(face="bold", color="black", size=9),
        legend.direction  = "vertical",
        legend.position   = c(0.70,0.50),
        legend.title      = element_blank(),
        legend.background = element_rect(color="black", linetype = "solid"))
