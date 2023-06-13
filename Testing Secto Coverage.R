# Testing: Number of new sectors per OECD country

# Load PRISM dataset

install.packages("readxl")
library(readxl)
library(dplyr)

raw_PRISM <- read_excel(file.choose(), sheet = "Time Series Data")

# remove all rows where there was no new legislation on fdi screening
raw_PRISM <- raw_PRISM[raw_PRISM[,8] != 'No', ]


#Illustrate change in number of sectors covered by FDI Screening

#isolate variables: country: sectors covered, threshold, total sectors

raw_PRISM_selected <- select(raw_PRISM, "Country","year", "total sectors", "Threshold")

# calculate number of new sectors for each country with each given legislative change

#graphing 
install.packages("ggplot2")
library(ggplot2)
install.packages("viridis")
library(viridis)



countries_to_remove <- c("Belgium", "Czech Republic", "Estonia", "Finland", 
                         "Latvia", "Norway", "Portugal", "Slovenia")

raw_PRISM_selected <- filter(raw_PRISM_selected, !(Country %in% countries_to_remove))

raw_PRISM_selected$Country_Year <- paste(raw_PRISM_selected$Country, raw_PRISM_selected$year, sep = " - ")


# Install and load the ggpattern package
install.packages("ggpattern")
library(ggpattern)

# Install and load the ggpattern package
install.packages("ggpattern")
library(ggpattern)
install.packages("ggthemes")
library(ggthemes)


# Create a new variable to assign the patterns
raw_PRISM_selected$Pattern <- as.factor(ifelse(as.integer(as.factor(raw_PRISM_selected$Country)) %% 2 == 0, 
                                               "stripe", 
                                               "crosshatch"))

# Create the plot

ggplot(data = raw_PRISM_selected, aes(x = Country_Year, y = `total sectors`, pattern = Pattern)) +
  geom_bar_pattern(stat = "identity", fill = "white", color = "black", pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.01,
                   pattern_key_scale_factor = 1) +
  geom_text(aes(label = `total sectors`), vjust = -0.3, size = 3.5) +
  xlab("Country - Year of New FDI Screening") +
  ylab("Total Sectors Covered by Investment Screening Mechanisms") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 10)),
        plot.margin = margin(t = 30, r = 20, b = 60, l = 20)) +
  labs(title = "Tracking Sectoral Coverage on FDI Screening Legislations: OECD Countries",
       subtitle = "All OECD Countries that have introduced at least two FDI screening legislations since 2007 are included",
       caption = "Source: personal coding & data manipulation utilizing the PRISM dataset")


# Number of countries that apply FDI screening to each sector:

columns_to_keep <- c(1, 7, 35:49, 51:70) # isolate relevant sectors

raw_PRISM_selected2 <- raw_PRISM[, columns_to_keep]

#only keep the last instance of FDI screening expansion

raw_PRISM_selected2_last <- raw_PRISM_selected2 %>%
  group_by(Country) %>%
  slice_tail(n = 1) %>%
  ungroup()
# count number of occurances in each sector

selected_columns <- raw_PRISM_selected2_last[, 3:37]
sectoral_coverage_per_sector <- data.frame(Column = colnames(selected_columns),
                            Total_Count = colSums(selected_columns))

# graphing

# Create a bar plot with a similar style # BAR PLOT REPRESENTS THE COVERAGE OF EACH SECTOR AS SPECIFIED IN THE LATEST AVAILABLE YEAR
# Create a bar plot with rotated x-axis labels
# Create a bar plot with smaller font size for x-axis labels
ggplot(data = sectoral_coverage_per_sector, aes(x = Column, y = Total_Count)) +
  geom_bar(stat = "identity", fill = "white", color = "black") +
  geom_text(aes(label = Total_Count), vjust = -0.3, size = 3.5) +
  xlab("Specific Sector") +
  ylab("Frequency of Investment Screening Mechanism Coverage by OECD Countries") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 10)),
        plot.margin = margin(t = 30, r = 20, b = 60, l = 20)) +
  labs(title = "How Often are Specific Sectors Under Scrutiny by FDI Screening Agencies? Last Update: 30 August 2022",
       subtitle = "Number of times each sector is 'covered' by FDI screening: OECD Countries",
       caption = "Source: personal coding & data manipulation utilizing the PRISM dataset")

# Create bar plot that illustrates sectoral coverage over time

# Visualize every sectoral category change over time

raw_PRISM <- read_excel(file.choose(), sheet = "Time Series Data")



