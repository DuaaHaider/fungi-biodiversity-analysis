

# Project Description -----------------------------------------------------
#Mushroom biodiversity and soil moisture analysis
#Aim : Investigate the effects of abiotic factors on fungal biodivrsity
#Comparison : between shaded and unshaded areas


# Load required practices -------------------------------------------------

library(tidyverse) #includes ggplot2 for plotting and dplyr for data handling


# Creating dataset --------------------------------------------------------
#This dataset includes mushroom counts and soil moisture values that have been collected from shaded and unshaded areas

mushroom_data <- data.frame(
  Location = c(rep("Unshaded" , 4), rep("Shaded", 4)),
  Count = c(66, 49, 108, 9, 9, 12, 4, 6),
  Moisture = c(16.03, 21.95, 19.96, 27.20, 18.76, 19.28, 17.91, 14.92)
)



# Summary Statistics ------------------------------------------------------

#Calculate mean mushroom count and mean soil moisture for each location type

summary_stats <- mushroom_data %>%
  group_by(Location) %>%
  summarise(
    mean_count = mean(count),
    mean_moisture = mean(Moisture),
    sd_count = sd(Count),
    sd_moisture = sd(Moisture),
    .groups = "drop"
  )

summary_stats
    


# Welch two-sample T-test -------------------------------------------------

#compare soil moisture content between shaded and unshaded areas.
#Welch's t-test is used as it does not assume equal variance

moisture_t_test <- t.test(Moisture ~ Location, data = mushroom_data)

moisture_t_test


# Boxplot : Soil moisture by location (unshaded v shaded) -----------------

#created a boxplot showing soil moisture content in shaded and unshaded areas
#individual data points are included using jitter as sample size is small.

ggplot(mushroom_data, aes(x = Location, y = Moisture, fill = Location)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 3, color = "black") +
  theme_minimal(base_size = 18) +
  scale_fill_manual(values = c("Shaded" = "#7FB3D5", "Unshaded" = "#F7DC6F")) +
  labs(
    title = "Unshaded areas exhibited higher soil moisture",
    subtitle = "Welch t-test, p = 0.229",
    y = "Moisture Content (%)",
    x = "Location"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 20),
    plot.subtitle = element_text(size = 14),
    axis.title = element_text(face = "bold")
  ) +
  annotate(
    "text",
    x = 1.5,
    y = max(mushroom_data$Moisture) * 1.05,
    label = "p = 0.229",
    size = 5
  )

#

ggplot(mushroom_data, aes(x = Location, y = Count, fill = Location)) +
  geom_boxplot()

