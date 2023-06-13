# 
install.packages("readxl")
library(readxl)
library(dplyr)




#load dataset

raw_PRISM <- read_excel(file.choose(), sheet = "Time Series Data")

# Subset the dataset to keep only the desired columns
selected_columns <- c(1, 7, 23, 24)
raw_PRISM_subset <- raw_PRISM[, selected_columns]

#Remove all NAs
raw_PRISM_clean <- na.omit(raw_PRISM_subset)

# Order the dataset by column 2


raw_PRISM_clean <- as.data.frame(raw_PRISM_clean)
raw_PRISM_clean[, 2] <- as.numeric(raw_PRISM_clean[, 2])

# Order the dataset by column 2
raw_PRISM_ordered <- raw_PRISM_clean[order(raw_PRISM_clean[, 2]), ]

# pr year observations
summary_df <- raw_PRISM_ordered %>%
  group_by(year) %>%
  summarize(
    sum_National_Security = sum(`National Security/Public Order Test`, na.rm = TRUE),
    sum_Net_Benefit = sum(`Net Benefit Test`, na.rm = TRUE)
  )
summary_df <- summary_df[-17, ]

# visualize
library(ggplot2)
# Create the line graph (unsorted)
ggplot(summary_df, aes(x = year)) +
  geom_line(aes(y = sum_National_Security, color = "National Security")) +
  geom_line(aes(y = sum_Net_Benefit, color = "Net Benefit")) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(x = "Year", y = "Value", color = "Legend") +
  scale_color_manual(values = c("National Security" = "blue", "Net Benefit" = "red")) +
  theme_minimal()

library(ggplot2)
library(ggthemes)

# Create the line graph with desired style and legend
ggplot(summary_df, aes(x = year)) +
  geom_point(aes(y = sum_National_Security, color = "National Security/Public Order Tests"), fill = "white", size = 2.5) +
  geom_line(aes(y = sum_National_Security, color = "National Security/Public Order Tests"), linetype = "dotted", size = 1.5) +
  geom_text(aes(y = sum_National_Security, label = sum_National_Security), vjust = -1, size = 3) +
  geom_point(aes(y = sum_Net_Benefit, color = "Net Benefit Tests"), fill = "white", size = 2.5) +
  geom_line(aes(y = sum_Net_Benefit, color = "Net Benefit Tests"), linetype = "dotted", size = 1.5) +
  geom_text(aes(y = sum_Net_Benefit, label = sum_Net_Benefit), vjust = -1, size = 3) +
  scale_x_continuous(limits = range(summary_df$year), breaks = seq(min(summary_df$year), max(summary_df$year), by = 1)) +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Change in National Security/Public Order Tests & Net Benefit Tests: OECD Countries", 
       subtitle = "Each line corresponds to how many OECD Screening legislations conduct 1) National Security & Public Order Tests or 2) Net Benefit Tests", 
       x = "Year", 
       y = "Total Number") +
  scale_color_manual(values = c("National Security/Public Order Tests" = "blue", "Net Benefit Tests" = "red"),
                     labels = c("National Security/Public Order Tests", "Net Benefit Tests")) +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1.5, vjust = 1, size = 8),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)), 
        legend.position = "bottom",  # Set the legend position to bottom
        legend.title = element_blank(),
        legend.text = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 14, margin = margin(b = 10)),
        plot.subtitle = element_text(hjust = 0.5, size = 8, margin = margin(t = 10, b = 0)),  # Adjust the subtitle margin
        plot.margin = margin(t = 30, r = 20, b = 60, l = 20),
        strip.text.x = element_text(margin = margin(t = 1, r = 0, b = 1, l = 0), face = "bold")) +
  labs(caption = "Source: personal coding & data manipulation utilizing the PRISM dataset") +
  labs(color = "Variables")











