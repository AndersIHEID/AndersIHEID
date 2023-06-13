# Logistic Regression - R&D spending 
library(tidyverse)
library(janitor)
library(purrr)
library(broom)
library(ggthemes)

#load data from .csv (OECDstat)

raw <- read.csv(file.choose())
unique_values <- unique(raw[, 2])
filtered_data <- raw[raw[, 2] %in% c("GERD as a percentage of GDP", "Gross Domestic Expenditure on R&D (GERD) at current PPP $"), ]
filtered_data <- filtered_data[,c(2, 4, 6, 13)]

relative <- filtered_data[filtered_data[, 1] == "GERD as a percentage of GDP", ]
colnames(relative) <- c("DELETE", "Country", "year", "GERD as a percentage of GDP")
relative <- relative[,c(2,3,4)]
absolute <- filtered_data[filtered_data[, 1] == "Gross Domestic Expenditure on R&D (GERD) at current PPP $", ]
colnames(absolute) <- c("DELETE", "Country", "year", "Gross Domestic Expenditure on R&D (GERD) at current PPP $")
absolute <- absolute[,c(2,3,4)]

#merge the two
merged_data <- merge(absolute, relative, by = c("Country", "year"))

# remove irrelevant countries that are not aprt of PRISM
merged_data <- merged_data[merged_data$Country %in% PRISM_selected$Country, ]


# combine filtered_data and PRISM dataset with sectoral coverage data

# load sorted PRISM dataset

# Load PRISM dataset

install.packages("readxl")
library(readxl)
library(dplyr)

raw_PRISM <- read_excel(file.choose(), sheet = "Time Series Data")

# Columns to keep: 1, 7, 35:49, 51:70

PRISM_selected <- raw_PRISM[, c(1, 7, 35:49, 51:70)]
PRISM_selected$year <- as.numeric(PRISM_selected$year)

#merge the two datasets based on country + year
# Assuming your datasets are stored in the 'PRISM_selected' and 'filtered_data' variables
finalsolution <- merge(PRISM_selected, merged_data, by = c("Country", "year"), all.x = TRUE)


# Finalsolution Latest available year (for GERD data)
finalsolution <- finalsolution[finalsolution$year <= 2022, ]

##### UPDATING WITH ESTIMATES

# Filter the dataset to include only rows where the year is 2021
finalsolution_2022 <- finalsolution[finalsolution$year == 2022, ]

# Add most recent invest value for each country
# Find most recent research values for each country
most_recent_research_value <- finalsolution %>%
  group_by(Country) %>%
  filter(!is.na(`Gross Domestic Expenditure on R&D (GERD) at current PPP $`) & !is.na(`GERD as a percentage of GDP`)) %>%
  slice_max(n = 1, order_by = year) %>%
  transmute(Country, research_year = year, `Gross Domestic Expenditure on R&D (GERD) at current PPP $`, `GERD as a percentage of GDP`)

# Add to main dataset
finalsolution_2022 <- finalsolution_2022 %>%
  # Remove research per-year columns
  select(-c(`Gross Domestic Expenditure on R&D (GERD) at current PPP $`, `GERD as a percentage of GDP`)) %>%
  # Add columns that have the same most recent value
  left_join(., most_recent_research_value, by = "Country")

# Add Korea Data from alternative source (World Bank) 
finalsolution_2022[26,40] <- 4.930121e+00
finalsolution_2022[26,38] <- 2021
finalsolution_2022[26,39] <- 1.196173e+05

# Greece, Sweden, Iceland have no FDI screening, change NA to 0 for all values
finalsolution_2022_updated <- finalsolution_2022
finalsolution_2022_updated[is.na(finalsolution_2022_updated)] <- 0

# Turn dependent variables into factors for logit model
finalsolution_2022_updated <- finalsolution_2022_updated %>%
  mutate(across(c(3:37), as.factor))

# Lets add REAL GDP FORECAST
gdpraw <- read.csv(file.choose())
gdpraw <- gdpraw %>% filter(TIME == 2022)
gdpforecast <- gdpraw[, c("LOCATION", "REALGDPFORECAST" = "Value")]
colnames(gdpforecast) <- c("Country", "REALGDPFORECAST")
install.packages("countrycode")
library(countrycode)
gdpforecast$Country <- countrycode(gdpforecast$Country, "iso3c", "country.name")

#Add to finalsolution_2022_updated
PRISM_GERD_GDP <- merge(finalsolution_2022_updated, gdpforecast, by = "Country", all.x = TRUE)
PRISM_GERD_GDP[5,41] <- 2.4227480
PRISM_GERD_GDP[26,41] <- 2.7433382
PRISM_GERD_GDP[27,41] <- 1.6266535


# Starting Logistic Regression
library(stats)
install.packages("coefplot")
library(coefplot)
install.packages("gridExtra")
library(gridExtra)

library(coefplot)

# Subset the dataset
subset_df <- PRISM_GERD_GDP[, c(3:37, 39, 40, 41)]

#test first sector
library(ggplot2)
library(dplyr)
install.packages("glm")
library(glm)

model <- glm(`Defense Production` ~ `Gross Domestic Expenditure on R&D (GERD) at current PPP $` + `GERD as a percentage of GDP` + REALGDPFORECAST,
             data = subset_df, family = binomial())

subset_df$predictedDefense <- predict(model, type = "response")

# Prepare before modelling on each of 35 outcome variables
# Pivot longer
subset_df_long <- subset_df %>%
  tibble() %>%
  pivot_longer(cols = c(1:35)) %>%
  clean_names() %>%
  rename(gross_rd = 1, gerd_percent = 2, real_gdp_forecast = 3) %>%
  arrange(name)

# Nest
subset_df_nested <- subset_df_long %>%
  group_by(name) %>%
  nest()

# Create model function
fit_logit_model <- function(input_data){
  
  glm(value ~ gross_rd + gerd_percent + real_gdp_forecast,
      data = input_data, family = binomial(link = "logit"))
  
}


glm(value ~ gross_rd + gerd_percent + real_gdp_forecast,
    data = subset_df_nested[[2]][[1]], family = binomial(link = "logit"))


# Run model with map
model_results <- subset_df_nested %>%
  mutate(model_1 = purrr::map(data, ~fit_logit_model(.x))) %>%
  select(-data)

# Check results
model_results <- model_results %>%
  mutate(summary = purrr::map(model_1, tidy)) %>%
  select(-model_1) %>%
  unnest() %>%
  ungroup()


# Plot results
library(tidytext)

# Add average
data_for_plot <- bind_rows(model_results %>%
                             mutate(to_highlight = "Sectors"),
                           # Create row for average across sectors
                           model_results %>%
                             group_by(term, name = "Average", to_highlight = "Average") %>%
                             summarise(estimate = mean(estimate)),
                           # Create average for specific sectors
                           model_results %>%
                             filter(name %in% c("Defense Technologies", "Defense Production")) %>%
                             group_by(term, name = "Defense Average", to_highlight = "Defense Average") %>%
                             summarise(estimate = mean(estimate)), 
                            # Phys / Conv Infra
                             model_results %>%
                             filter(name %in% c("Agricultural/Food Security","Energy Infrastructure (Electricity/Gas/Coal Production","Energy Storage", "Mineral Resources", "Telecommunications Infrastructure", "Transportation Infrastructure", "Water Infrastructure")) %>%
                              group_by(term, name = "Physical/Conventional Critical Infrastructure Average", to_highlight = "Physical/Conventional Critical Infrastructure") %>%
                              summarise(estimate = mean(estimate)),
                            # Next Gen
                           model_results %>%
                             filter(name %in% c("Education and Training","Healthcare Infrastructure","Research Institutions")) %>%
                             group_by(term, name = "Next Gen Critical Infrastructure Average", to_highlight = "Next Gen Critical Infrastructure Average") %>%
                             summarise(estimate = mean(estimate)),
                           # Critical Technologies and Dual Use
                           model_results %>%
                             filter(name %in% c("Advanced Computing","Advanced Materials","Artificial Intelligence and Machine Learning", "Biotechnologies", "Brain-Computer Interface", "Civil Nuclear", "Critical supplies", "Controlled Dual-use", "Cybersecurity", "Data Analytics", "Hypersonics", "Information and Sensing", "Logistics Technology", "Microprocessors", "Position/Navigation/Timing Tech", "Robotics", "Surveillance", "Space")) %>%
                             group_by(term, name = "Critical Technologies and Dual Use", to_highlight = "Critical Technologies and Dual Use") %>%
                             summarise(estimate = mean(estimate)),
                           # Non-tradeable Services 
                           model_results %>%
                             filter(name %in% c("Gambling","Tourism")) %>%
                             group_by(term, name = "Non-tradeable Services", to_highlight = "Non-tradeable Services") %>%
                             summarise(estimate = mean(estimate))
 )


# Use tidytext package to reorder each facet
data_for_plot <- data_for_plot %>%
  mutate(term = case_match(term,
                           "gerd_percent" ~ "Gross Expenditure R&D (share of GDP)",
                           "gross_rd" ~ "Gross Expenditure R&D (Absolute)",
                           "real_gdp_forecast" ~ "Projected Annual GDP Growth (Percentage)",
                           .default = term),
         term = as.factor(term),
         name = reorder_within(name, estimate, term))

# Plot
data_for_plot %>%
  # Plot
  ggplot(.,
         aes(x = estimate,
             y = name)) +
  geom_col(aes(fill = to_highlight), show.legend = FALSE) +
  # Create arrow for viz
  geom_segment(data = data_for_plot %>% filter(name == "gambling___Gross Expenditure R&D (share of GDP)"),
               aes(x = 1, y = "Additive Manufacturing___Gross Expenditure R&D (share of GDP)", xend = 3, yend = "Additive Manufacturing___Gross Expenditure R&D (share of GDP)"),
               linewidth = 0.7, colour = "black",
               arrow = arrow(length = unit(0.4, "cm"))) +
  geom_text(data = data_for_plot %>% filter(name == "Space___Gross Expenditure R&D (share of GDP)"),
            aes(x = 2, y = "Space___Gross Expenditure R&D (share of GDP)", label = "FDI-check more likely"),
            colour = "black", size = 3) + 
  # Other
  facet_wrap(~term, scales = "free") +
  scale_y_reordered() +
  labs(
    title = bquote(bold("Logit Model: Influence on Sectoral Coverage by GDP, R&D")),
    subtitle = bquote(italic(textstyle("Logit Model based on PRISM Dataset + OECD R&D Data + OECD GDP Forecast Data. Sample Countries: OECD Members"))),
    y = NULL,
    x = NULL,
  ) + theme(plot.title = element_text(size = 14, face = "bold"), plot.subtitle = element_text(size = 12, face = "italic"))








