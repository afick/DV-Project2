# Data Visualization (GOVT16_QSS17) Spring 2022
# Data Visualization Project: Lab 2
# 
# Name: Alex Fick
# Date: May 10, 2022

# Initial Settings --------------------------------------------------------

library(tidyverse)
library(ggthemes)
library(patchwork)

fig_h <- 8
fig_w <- 11

# Load data ---------------------------------------------------------------

folder <- "data/"
dict <- "documentation/CollegeScorecardDataDictionary.xlsx"
fos <- "Most-Recent-Cohorts-Field-of-Study.csv"

read_dict <- function(.sheet) {
  out <- readxl::read_xlsx(dict, sheet = .sheet, na = "NULL")
  return(out)
}

fieldsofstudy <- read_csv(paste0(folder, fos), na = "NULL")
fosdictionary <- read_dict(7)

# Fix column names --------------------------------------------------------

names_stdz <- function(df){
  names(df) <- names(df) %>% 
    str_to_lower() %>% 
    str_replace_all("[\\s-]+","_")
  
  return(df)  
}

fieldsofstudy <- names_stdz(fieldsofstudy)
fosdictionary <- names_stdz(fosdictionary)

# Explore data ------------------------------------------------------------

names(fosdictionary)
peek <- fosdictionary %>% filter(str_detect(name_of_data_element, "male")) 

# Let's look at debt and salary 1 year after college of men and women.  
myvariables <- tibble()

getvar <- function(s1, s2) {
  out <- peek %>% filter(str_detect(name_of_data_element, s1) &
                           str_detect(name_of_data_element, s2)) %>% 
    pull(variable_name) %>% 
    str_to_lower()
}

debtvars <- getvar("Median Stafford", "at this institution") 
earningvars <- getvar("Median earnings", "1 year")

myvariables <- c(debtvars, earningvars)

# Wrangle Data ------------------------------------------------------------

data <- fieldsofstudy %>% 
  filter(str_detect(creddesc, "Bachelor")) %>% 
  select(instnm, cipdesc, creddesc, myvariables) 

# Check if we can continue
names(data) # We should see what we selected
data %>% count(creddesc) # We should only see Bachelors Degree

data <- data %>% 
  mutate(subject = case_when(
    str_detect(cipdesc, "[Ss]cience") | str_detect(cipdesc, "[Tt]ech") ~ "STEM",
    TRUE ~ "Non-STEM")) 

pivot_data <- function(var) {
  out <- data %>% 
    pivot_longer(contains(var), 
                 names_to = "gender",
                 values_to = var) %>% 
    mutate(gender = ifelse(!str_detect(gender, "notm|nom"), "Male", "Female")) %>% 
    select(!contains("mdn")) %>% 
    distinct()
}

debtdata <- pivot_data("debt")
earndata <- pivot_data("earn")

fulldata <- left_join(debtdata, earndata, 
                      by = NULL) %>% 
  mutate(debt = as.numeric(debt)) %>%  # Ignore warnings, this is intended
  mutate(earn = as.numeric(earn)) %>%  # ^
  rename(earnings = earn) %>% 
  mutate(subject = fct_relevel(subject, "Non-STEM", after = Inf)) %>% 
  mutate(gender = factor(gender, levels = c("Male", "Female")))

medians <- fulldata %>% 
  group_by(subject, gender) %>% 
  summarise(medianearnings = median(earnings, na.rm = T),
            mediandebt = median(debt, na.rm = T),
            .groups = "drop") %>% 
  pivot_longer(contains("median"), names_to = "type",
               values_to = "val") %>% 
  mutate(
    x = case_when(
      str_detect(type, "earn") ~ 125000,
      str_detect(type, "debt") ~ 42000
    ),
    y = case_when(
      str_detect(type, "earn") ~ 2.5*(10^-5),
      str_detect(type, "debt") ~ 5*(10^-5)
    ))

# Visualize Data ----------------------------------------------------------

graph_facet <- function(check) {
  df <- medians %>% filter(str_detect(type, check))
  
  text <- medians %>% filter(str_detect(type, check)) %>% 
    group_by(subject) %>% 
    summarise(x, y, type, diff = max(val) - min(val), .groups = "drop") %>% 
    distinct()
  
  difftext <- "The difference between\nthe male and female\n"
  
  ggplot(fulldata, aes_string(check)) +
    geom_vline(data = df, mapping = aes(xintercept = val, group = gender,
                                        color = gender),
               alpha = 0.75, linetype = "dashed") +
    geom_density(aes(group = gender, color = gender),
                 na.rm = T) +
    geom_text(data = text, mapping = aes(x, 
                                         y, 
                                         label = paste0(difftext, 
                                                        check,
                                                        " median is $",
                                                        # Use base R format 
                                                        # function to create 
                                                        # thousands comma
                                                        # separators 
                                                        format(diff, 
                                                               big.mark = ",",
                                                               nsmall = 2, 
                                                               trim = T))),
              size = 3) +
    facet_wrap(~ subject) +
    # Use Scales package to automatically format the x axis in dollar format.
    # Most efficient function for accomplishing this
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_y_continuous(name = "Density", breaks = NULL) +
    scale_color_manual(values = c("Male" = "blue",
                                  "Female" = "pink")) +
    labs(x = str_c(str_to_upper(str_sub(check, 1, 1)), 
                   str_sub(check, 2, -1), " (USD)"),
         color = "Gender:") +
    theme_few()
}

p2 <- graph_facet("earnings")
p3 <- graph_facet("debt")

title <- "Median Male Earnings are Greater than Female Earnings 1 Year after College but They Have a Similar Levels of Debt"
note <- "Note: Dashed lines represent overall median values"
source <- "Source: College Scorecard, Most Recent Data by Field of Study"

p2 / p3 + plot_annotation(title = title,
                          caption = source, 
                          subtitle = note) + 
  plot_layout(guides = "collect") &
  theme(legend.position = 'bottom', legend.justification = 'right')

ggsave("figures/figure.pdf", width = fig_w, height = fig_h)