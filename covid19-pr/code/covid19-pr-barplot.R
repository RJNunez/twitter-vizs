# -- SOURCE: https://www.politico.com/interactives/2020/coronavirus-testing-by-state-chart-of-new-cases/
library(tidyverse)
library(lubridate)
library(ggrepel)
library(ggthemes)
library(gridExtra)

# -- Data
state   <- c("Kansas", "Arkansas", "Nevada", "Iowa", "Utah", "Conneticut", "Puerto Rico")
colores <- c("#9ecae1","#fd8d3c","#525252","gold","#2171b5","#41ae76","#cb181d")
cases   <- c(1268, 1226, 2700, 1510, 2206, 11510, 788)
tests   <- c(12343, 18578, 23587, 17132, 42546, 39831, 6371)
dat     <- tibble(state, cases, tests, ratio = cases/tests) %>%
            mutate(state = factor(state, levels = state)) %>%
  arrange(desc(tests))

dat

# -- Cumulative cases
a<-dat %>%
  ggplot(aes(reorder(state, cases), cases, fill=state, label=cases)) +
  geom_col(color="black", size=0.50, show.legend = FALSE) +
  xlab("") +
  ggtitle("Casos de COVID 19 Acumulados hasta abril 10, 2020") +
  ylab("Casos positivos acumulados") +
  scale_fill_manual(name="",
                     values = colores) +
  scale_color_manual(name="",
                    values = colores) +
  scale_y_continuous(limits=c(0, 12000),
                     breaks = seq(0, 12000, by=2000)) +
  geom_label(aes(color=state), 
             fill="white",
             fontface="bold",
             show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  theme(axis.text  = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        plot.subtitle = element_text(face="bold"),
        plot.title = element_text(face="bold"),
        plot.caption  = element_text(face="bold"))

# -- Porcentaje de casos positivos
b<-dat %>%
  ggplot(aes(reorder(state, ratio), 100*ratio, fill=state, label=paste0(round(100*ratio,1),"%"))) +
  geom_col(color="black", size=0.50, show.legend = FALSE) +
  xlab("") +
  ggtitle("Porcentaje de Casos Positivos en abril 10, 2020") +
  ylab("Porciento de positivos") +
  scale_fill_manual(name="",
                    values = colores) +
  scale_color_manual(name="",
                    values = colores) +
  scale_y_continuous(limits=c(0, 30), 
                     breaks = seq(0, 30, by=5)) +
  geom_label(aes(color=state), 
             fill="white",
             fontface="bold",
             show.legend = FALSE) +
  labs(caption = "Rolando J. Acosta (@RJANunez)") +
  coord_flip() +
  theme_minimal() +
  theme(axis.text  = element_text(face="bold"),
        axis.title = element_text(face="bold"),
        plot.subtitle = element_text(face="bold"),
        plot.title = element_text(face="bold"),
        plot.caption  = element_text(face="bold"))


grid.arrange(a, b)

