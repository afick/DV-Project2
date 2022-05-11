# Data Visualization (GOVT16_QSS17) Spring 2022
# Data Visualization Project: Lab 1
# 
# Name: Alex Fick
# Date: May 3, 2022


# Initial Settings --------------------------------------------------------

library(tidyverse)
library(ggthemes)

fig_h <- 8
fig_w <- 11


# Load Data ---------------------------------------------------------------

folder <- "data/"
file1 <- "V-Dem-CY-Core-v12.rds"
file2 <- "fossil-fuel-consumption-by-fuel-type.csv"
file3 <- "population-by-country.csv"

politics <- read_rds(paste0(folder, file1)) %>% 
  rename(country = country_name) %>% 
  select(country, year, ends_with("_libdem")) %>% 
  mutate(country = country %>% 
           str_trim())

resources <- read_csv(paste0(folder, file2)) 

population <- read_csv(paste0(folder, file3)) %>% 
  rename(population = contains("pop"))

emissions <- read_csv(str_c(folder, file4))

resources <- left_join(resources, population,
                       by = c("Entity" = "Entity",
                              "Year" = "Year", 
                              "Code" = "Code")) %>% 
  rename(country = Entity)


# Eliminate Extraneous Info -----------------------------------------------

# Replace spaces in column names with underscores
# and make all names lower-case for easy referencing
names(resources) <- names(resources) %>%  
  str_replace_all("[\\s-]+","_") %>% 
  str_to_lower()

resources <- resources %>% 
  select(-code) %>% 
  mutate(country = 
           case_when(str_detect(country, "United States") ~ "United States of America",
                             TRUE ~ country),
         country = str_replace(country, "Czechia", "Czech Republic"))

politics <- politics %>% 
  filter(year > 1969 & year < 2021)

# Combine data by year and country ----------------------------------------

full <- full_join(politics, resources, 
                  by = c("country" = "country",
                         "year" = "year")) %>% 
  fill(population, .direction = "downup") 

# Transform Data to Focus on 10 Biggest Consumers -------------------------

full <- full %>% 
  mutate(total_consumption = coal_consumption_twh +
           oil_consumption_twh +
           gas_consumption_twh,
         population = population * 1000,
         consump_percap = total_consumption * 10^6 / population) %>% 
  filter(!is.na(v2x_libdem))

t10 <- full %>% 
  group_by(country) %>% 
  summarise(avgtotal_cons = mean(total_consumption),
            .groups = "drop") %>% 
  slice_max(avgtotal_cons, n = 10)

t10countries <- t10 %>% pull(country)

top10data <- full %>% 
  filter(country %in% t10countries) %>% 
  group_by(country) %>% 
  mutate(avgtotal_cons = mean(total_consumption)) %>% 
  ungroup() %>% 
  mutate(country = fct_reorder(country, desc(avgtotal_cons)))


# Create Graphic ----------------------------------------------------------

kyoto <- "Signing of\nKyoto Protocol\n(1997)"
paris <- "Signing of\nParis Agreement\n(2015)"
subtitle1 <- "The Total Fossil Fuel Consumption per Capita of the Top 10 Total Consumption Countries from 1970 to 2020,"
subtitle2 <- "in Order of Total Fossil Fuel Consumption"

ggplot(top10data, aes(year,  consump_percap)) +
  geom_segment(aes(x = 1997 + 11/12, xend = 1997 + 11/12, 
                   y = -Inf, yend = 95),
               linetype = "dashed",
               color = "gray",
               alpha = 0.5) +
  geom_segment(aes(x = 2015 + 11/12, xend = 2015 + 11/12, 
                  y = -Inf, yend = 95),
              linetype = "dashed",
              color = "gray",
              alpha = 0.5) +
  annotate(geom = "text", x = 2002 + 11/12, y = 105, 
           size = 2.5,
           label = kyoto,
           hjust = 1,
           color = "#013220") +
  annotate(geom = "text", x = 2005 + 11/12, y = 105, 
           size = 2.5,
           label = paris, 
           hjust = 0,
           color = "#301934") +
  geom_smooth(se = FALSE, color = "#565656", 
              size = 1.2) +
  geom_jitter(aes(color = v2x_libdem),
             size = 2.5,
             alpha = 0.5) +  
  scale_x_continuous(breaks = seq(1975, 2020, 15),
                     limits = c(1965, 2030)) +
  scale_y_continuous(limits = c(0, 110),
                     breaks = seq(25, 100, 25)) +
  scale_color_gradient2(low = "red", mid = "orange",
                        high = "yellow", midpoint = 0.5,
                        breaks = seq(0, 1, 0.5),
                        limits = c(0,1), 
                        labels = c("0\n(Least Liberal)", 0.5, 
                                   "1\n(Most Liberal)")) +
  facet_wrap(~ country, nrow = 2) +
  labs(x = NULL, 
       y = "Fossil Fuel Consumption per Capita (Megawatts)",
       color = "Liberal Democracy Index",
       title = "Do Liberal Countries Care More About the Environment?",
       subtitle = str_c(subtitle1, subtitle2, sep = "\n"),
       caption = "Sources: Our World in Data and V-Dem Institute") +
  theme_few() +
  theme(legend.position = "bottom",
        legend.justification = "left",
        legend.key.width = unit(1, 'cm')) +
  guides(color = guide_colorbar(title.vjust = .9))

ggsave("figures/figure1.pdf", width = fig_w, height = fig_h)
