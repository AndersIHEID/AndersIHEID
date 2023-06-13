# Create bar plot that illustrates sectoral coverage over time
# Load PRISM dataset

install.packages("readxl")
library(readxl)
library(dplyr)

raw_PRISM <- read_excel(file.choose(), sheet = "Time Series Data")

# Columns to keep: 1, 7, 35:49, 51:70

PRISM_selected <- raw_PRISM[, c(1, 7, 35:49, 51:70)]

#  Sorting and visualizing Sectoral coverage over time
library(dplyr)


# tidy datasets:

PRISM_selected <- arrange(PRISM_selected, year) # arrange by year

PRISM_selected <- PRISM_selected[complete.cases(PRISM_selected[,3:37]), ] #remove rows where years have no data

PRISM_selected <- PRISM_selected %>%
  mutate_at(vars(2:37), as.numeric)

# create new datasets that illustrates values in each sector over time:

# Create new dataframe
new_dataframe <- PRISM_selected %>%
  select(-Country) %>%
  group_by(year) %>%
  summarise(across(everything(), sum, na.rm = TRUE))
# Remove row 17
new_dataframe <- new_dataframe[-17, ]

library(tidyverse)

long_dataframe <- new_dataframe %>%
  pivot_longer(cols = 2:36, names_to = "variable", values_to = "value")

library(tidyverse)
library(ggthemes)
# new 
long_dataframe$variable <- str_wrap(long_dataframe$variable, width = 15)

long_dataframe <- long_dataframe %>%
  mutate(variable = str_to_title(variable)) %>% 
  mutate(variable = str_remove_all(variable, "[0-9]|\\.\\.\\.")) %>% 
  mutate(variable = str_trim(variable))


ggplot(long_dataframe, aes(x = year, y = value)) +
  geom_point(colour = "black", fill = "white", size = 2.5) +
  geom_line(linetype = "dotted", colour = "black", size = 1) +
  geom_text(aes(label = value), vjust = -1, size = 3) + # add labels to points
  facet_wrap(~ variable, scales = "free_x") +
  scale_x_continuous(limits = range(long_dataframe$year), breaks = seq(min(long_dataframe$year), max(long_dataframe$year), by = 1)) +
  scale_y_continuous(limits = c(0, 28)) +
  labs(title = "Change in FDI Screening Coverage by Sector over time: OECD Countries", 
       subtitle = "Each graph corresponds to one sector, numbers indicate how many OECD sectors have legal provisions in place for that sector", 
       x = "", 
       y = "Number of OECD Countries with Sectoral Coverage") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.5, vjust = 1, size = 8),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),  # Increase r value to push y-axis title further left
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 10)),
        plot.margin = margin(t = 30, r = 20, b = 60, l = 20),
        strip.text.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0), face = "bold")) +
  labs(caption = "Source: personal coding & data manipulation utilizing the PRISM dataset")

#save
ggsave(filename = "your_plot.png", plot = last_plot(), width = 22, height = 22, units = "in")


