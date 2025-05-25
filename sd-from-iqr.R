# -- Libraries
library(patchwork)
library(ggthemes)
library(ggplot2)
library(stringr)
library(purrr)
library(dplyr)
library(tidyr)

# -- Parameters for the Normal distibution
mu <- 64
sd <- 2.8

# -- Support for the Normal density
x <- seq(floor(mu - 3*sd), ceiling(mu + 3*sd), by = 0.001)
y <- dnorm(x=x, mean=mu, sd=sd)

# -- Viz the Normal distribution
tibble(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(lwd = 1) +
  labs(x = "Height (in)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"))

# -- Parameters for the Gamma distribution
shape <- 3.2
rate  <- 0.015

x <- seq(0, (shape/rate) + 3*sqrt(shape/rate^2), by = 0.01)
y <- dgamma(x = x, shape = shape, rate = rate)

# -- Viz the Gamma distribution
tibble(x, y) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(lwd = 1) +
  labs(x = "Total rainfall (mm)",
       y = "Density") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"))

# -- Get true SDs
true_normal_sd <- sd
true_gamma_sd  <- sqrt(shape / rate^2)

# -- Function to estimate SD from IQR
sd_from_iqr <- function(mu, sd, shape, rate, n) {
  # -- Get standard normal IQR
  iqr_z <- qnorm(0.75) - qnorm(0.25)
  
  # -- Sample from Gaussian distribution
  Xn <- rnorm(n=n, mean=mu, sd=sd)
  
  # -- Sample from Exponential distribution
  Xg <- rgamma(n=n, shape = shape, rate = rate)
  
  # -- Get IQR
  iqr_xn <- IQR(Xn)
  iqr_xg <- IQR(Xg)
  
  # -- Get estimate of SD
  sdn_estimate <- iqr_xn / iqr_z
  sdg_estimate <- iqr_xg / iqr_z
  
  # -- Data to return
  df <- tibble(sd_from_normal = sdn_estimate, sd_from_gamma = sdg_estimate, ssize = n)
  
  # -- Return
  return(df)
}

# -- Vector of sample size
n_vec <- c(10, 50, 100, 500, 1000, 5000)

# -- Number of times we will sample from distributions
R <- 100

# -- For reproducibility
set.seed(54345)

# -- Iterate of sample size vector
sim_dat <- map_df(n_vec, function(n) {
  
  # -- Iterate over R
  map_df(1:R, function(r) {
  
    # -- Call the function
    sd_from_iqr(mu=mu, sd=sd, shape=shape, rate=rate, n=n) %>%
      # -- Add iteration tag
      mutate(iteration = r)
  })
}) %>%
  pivot_longer(cols = 1:2, names_to = "distribution", values_to = "estimate") %>%
  mutate(distribution = ifelse(str_detect(distribution, "normal"), "Normal", "Gamma"),
         true_sd      = ifelse(distribution == "Normal", true_normal_sd, true_gamma_sd))

# -- Viz the estimates from Normal density
sim_dat %>%
  filter(distribution == "Normal") %>%
  ggplot(aes(x = estimate)) +
  facet_wrap(~ssize,
             labeller = as_labeller(c("10"   = "N = 10",
                                      "50"   = "N = 50",
                                      "100"  = "N = 100",
                                      "500"  = "N = 500",
                                      "1000" = "N = 1,000",
                                      "5000" = "N = 5,000"))) +
  geom_histogram(bins  = 50,
                 color = "white",
                 lwd   = 0.10,
                 fill  = "steelblue4") +
  geom_vline(xintercept = true_normal_sd,
             linetype   = 2) +
  labs(x = "SD estimate",
       y = "Frequency") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"),
        strip.background = element_rect(fill = "#252525"),
        strip.text = element_text(size  = 15,
                                  face = "bold",
                                  color = "white"))

# -- Viz the estimates from Gamma density
sim_dat %>%
  filter(distribution == "Gamma") %>%
  ggplot(aes(x = estimate)) +
  facet_wrap(~ssize,
             labeller = as_labeller(c("10"   = "N = 10",
                                      "50"   = "N = 50",
                                      "100"  = "N = 100",
                                      "500"  = "N = 500",
                                      "1000" = "N = 1,000",
                                      "5000" = "N = 5,000"))) +
  geom_histogram(bins  = 50,
                 color = "white",
                 lwd   = 0.10,
                 fill  = "steelblue4") +
  geom_vline(xintercept = true_gamma_sd,
             linetype   = 2) +
  labs(x = "SD estimate",
       y = "Frequency") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"),
        strip.background = element_rect(fill = "#252525"),
        strip.text = element_text(size  = 15,
                                  face = "bold",
                                  color = "white"))

# -- Get summary statistics
summary_stats <- sim_dat %>%
  mutate(difference = true_sd - estimate) %>%
  group_by(distribution, ssize) %>%
  summarize(avg_estimate   = mean(estimate),
            std_estimate   = sd(estimate),
            avg_difference = mean(difference),
            std_difference = sd(difference),
            .groups = "drop") %>%
  mutate_if(is.numeric, round, 3) %>%
  mutate(estimate   = paste0(avg_estimate, " (", std_estimate, ")"),
         difference = paste0(avg_difference, " (", std_difference, ")"))

# -- Viz the average difference (free axes)
p1 <- summary_stats %>%
  ggplot(aes(x=factor(ssize), y=avg_difference)) +
  facet_wrap(~distribution, scales = "free") +
  geom_hline(yintercept = 0, 
             linetype   = 2) +
  geom_errorbar(aes(ymin = avg_difference - std_difference,
                    ymax = avg_difference + std_difference),
                color = "red3",
                width = NA) +
  geom_point(shape = 21,
             size  = 3,
             color = "black",
             fill  = "red3") +
  labs(x = "Sample size",
       y = "Avg. Difference (±SD)") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"),
        strip.background = element_rect(fill = "#252525"),
        strip.text = element_text(size  = 15,
                                  face = "bold",
                                  color = "white"))

# -- Viz the average difference (fixed axes)
p2 <- summary_stats %>%
  ggplot(aes(x=factor(ssize), y=avg_difference)) +
  facet_wrap(~distribution) +
  geom_hline(yintercept = 0, 
             linetype   = 2) +
  geom_errorbar(aes(ymin = avg_difference - std_difference,
                    ymax = avg_difference + std_difference),
                color = "red3",
                width = NA) +
  geom_point(shape = 21,
             size  = 3,
             color = "black",
             fill  = "red3") +
  labs(x = "Sample size",
       y = "Avg. Difference (±SD)") +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size  = 15,
                                 color = "black"),
        axis.title = element_text(size  = 15,
                                  color = "black"),
        strip.background = element_rect(fill = "#252525"),
        strip.text = element_text(size  = 15,
                                  face = "bold",
                                  color = "white"))

# -- Viz combined figure.
(p1 / p2) +
  plot_annotation(tag_levels = "A")

# -- Summary statistics of estimates
summary_stats %>%
  select(distribution, ssize, estimate) %>%
  pivot_wider(names_from = ssize, values_from = estimate)

# -- Summary statistics of differences
summary_stats %>%
  select(distribution, ssize, difference) %>%
  pivot_wider(names_from = ssize, values_from = difference)


















