# -- Libraries 
library(tidycensus)
library(tidyverse)

# -- Plot options
theme_set(theme_void(base_size   = 12, 
                     base_family = "Helvetica"))

# -- Census API key (may need to change this)
census_api_key("0382062eb1c992d6115c6c04455c1994d0c1f891")

# -- Extracting data
res <- get_acs(geography = "county", 
               variables = c(internet   = "B28002_004",
                             computer   = "B28003_002",
                             poverty    = "B17001_002"), 
               survey    = "acs5",
               state     = "PR",
               year      = 2019,
               geometry  = TRUE) %>%
  mutate(municipio = str_replace(NAME, " Municipio, Puerto Rico", ""),
         year      = 2019) %>%
  arrange(municipio)

# -- Extracting population data
pop <- get_acs(geography = "county", 
               variables = c(population = "B01003_001"), 
               survey    = "acs5",
               state     = "PR",
               year      = 2019) %>%
  mutate(municipio = str_replace(NAME, " Municipio, Puerto Rico", ""),
         year      = 2019) %>%
  rename(population = estimate) %>%
  select(population, municipio)

# -- Getting municipality centroids
centroids <- sf::st_coordinates(sf::st_centroid(res)) %>%
  as_tibble() %>%
  unique() %>%
  mutate(municipio = unique(res$municipio),
         X = ifelse(municipio == "Mayagüez", -67.1052, X),
         Y = ifelse(municipio == "Mayagüez", 18.2013, Y))

# -- Putting everything together
res <- res %>%
  left_join(pop, by = "municipio") %>%
  left_join(centroids, by = "municipio") %>%
  mutate(proportion = estimate / population,
         labs       = paste0(round(100*proportion, 1), "%"))

# -- Viz: Households with an internet subscription as a function of populatio size
res %>%
  filter(variable == "internet") %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = proportion)) + 
  geom_sf(color = "black",
          size  = 0.10) + 
  geom_text(aes(X, Y, label = labs),
            size     = 3,
            fill     = "white",
            color    = "white",
            label.padding = unit(0.10, "lines"),
            label.size    = 0.10,
            fontface = "bold") +
  coord_sf(xlim = c(-67.25, -65.3)) +
  viridis::scale_fill_viridis(name   = "Hogares con subscribción de internet como proporción del tamaño de la población",
                              labels = scales::percent,
                              option = "E",
                              limits = c(0.05, 0.35),
                              breaks = seq(0.05, 0.35, by = 0.05)) +
  theme(text             = element_text(size = 15),
        legend.position  = "top",
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position = "top", 
                                title.hjust   = .5, 
                                barwidth      = unit(45, "lines"), 
                                barheight     = unit(0.50, "lines")))

# -- Viz: Households with at least one computer as a function of populatio size
res %>%
  filter(variable == "computer") %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = proportion)) + 
  geom_sf(color = "black",
          size  = 0.10) + 
  geom_text(aes(X, Y, label = labs),
            size     = 3,
            fill     = "white",
            color    = "white",
            label.padding = unit(0.10, "lines"),
            label.size    = 0.10,
            fontface = "bold") +
  coord_sf(xlim = c(-67.25, -65.3)) +
  viridis::scale_fill_viridis(name   = "Hogares con al menos una computadora como proporción del tamaño de la población",
                              labels = scales::percent,
                              option = "E",
                              limits = c(0.05, 0.35),
                              breaks = seq(0.05, 0.35, by = 0.05)) +
  theme(text             = element_text(size = 15),
        legend.position  = "top",
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust   = .5, 
                               barwidth      = unit(45, "lines"), 
                               barheight     = unit(0.50, "lines")))

# -- Viz: Poverty as a percentage of population
res %>%
  filter(variable == "poverty") %>%
  sf::st_as_sf() %>%
  ggplot(aes(fill = proportion)) + 
  geom_sf(color = "black",
          size  = 0.10) + 
  geom_text(aes(X, Y, label = labs),
            size     = 3,
            fill     = "white",
            color    = "white",
            label.padding = unit(0.10, "lines"),
            label.size    = 0.10,
            fontface = "bold") +
  coord_sf(xlim = c(-67.25, -65.3)) +
  viridis::scale_fill_viridis(name   = "Porciento de la población que vive bajo niveles de probreza",
                              labels = scales::percent,
                              option = "E",
                              limits = c(0.25, 0.65),
                              breaks = seq(0.25, 0.65, by = 0.10)) +
  theme(text             = element_text(size = 15),
        legend.position  = "top",
        legend.direction = "horizontal") +
  guides(fill = guide_colorbar(title.position = "top", 
                               title.hjust   = .5, 
                               barwidth      = unit(45, "lines"), 
                               barheight     = unit(0.50, "lines")))

##
tab <- res %>%
  as_tibble() %>%
  filter(municipio %in% c("Cataño", "Trujillo Alto", "Gurabo", "San Juan", "Dorado")) %>%
  select(municipio, variable, proportion) %>%
  pivot_wider(names_from  = variable,
              values_from = proportion) %>%
  mutate(poverty  = paste0(prettyNum(100 * poverty, digits = 4), "%"),
         internet = paste0(prettyNum(100 * internet, digits = 4), "%"),
         computer = paste0(prettyNum(100 * computer, digits = 4), "%"),
         `% de Matricula en Fracaso (ENDI)` = paste0(prettyNum(c(30.69, 26.63, 23.90, 22.04, 20.80), digits = 4), "%")) %>%
  select(municipio, `% de Matricula en Fracaso (ENDI)`, poverty, internet, computer) %>%
  rename(`% de Hogares con subscripción de internet` = internet,
         `% de Hogares con al menos una computadora` = computer,
         `% de Pobreza` = poverty,
         Municipio = municipio)
kableExtra::kbl(tab) %>%
  kableExtra::kable_classic(full_width = F, html_font = "Cambria")


res %>%
  as_tibble() %>%
  select(municipio, variable, proportion) %>%
  filter(variable == "poverty") %>%
  arrange(desc(proportion)) %>%
  mutate(proportion = 100 * proportion)

res %>%
  as_tibble() %>%
  select(municipio, variable, proportion) %>%
  filter(variable == "internet") %>%
  arrange(proportion) %>%
  mutate(proportion = 100 * proportion)

res %>%
  as_tibble() %>%
  select(municipio, variable, proportion) %>%
  filter(variable == "computer") %>%
  arrange(proportion) %>%
  mutate(proportion = 100 * proportion)

