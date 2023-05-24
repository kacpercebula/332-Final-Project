library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(shiny)
library(tidymodels)
library(DT)
library(leaflet)
library(ggplot2)
library(tidyr)
library(leaflet.extras)


#set working directory
setwd("~/Documents/Data332")

df <- read.csv('Heart_Disease_Mortality.csv')

#Separate lat long values into two separate columns
df <- separate(df, Location.1, into = c("lat", "long"), sep = ", ")

#Use gsub to remove () from lat long character and make the values read as a number
df$lat <- as.numeric(gsub("\\(", "", gsub("\\)", "", df$lat)))
df$long <- as.numeric(gsub("\\(", "", gsub("\\)", "", df$long)))

#Rounding the lat long values to 2 decimal places
df$lat <- round(df$lat, 2)
df$long <- round(df$long, 2)
  
#Creating data frame by selecting specific columns
df1 <- df %>%
  select(LocationAbbr, Data_Value, Stratification1, Stratification2, lat, long)

#Removing rows that contain NA values
df1 <- na.omit(df1)

#Entire US Male v. Female visualization
df2 <- df1 %>%
  select(Stratification1, Data_Value) %>%
  filter(Stratification1 %in% c("Male", "Female")) %>%
  group_by(Stratification1, Data_Value)

# Calculate mean and standard error for each gender
mean_values <- aggregate(Data_Value ~ Stratification1, data = df2, FUN = mean)
se_values <- aggregate(Data_Value ~ Stratification1, data = df2, FUN = function(x) sd(x) / sqrt(length(x)))

# Merge mean and standard error data
df_summary <- merge(mean_values, se_values, by = "Stratification1")

# Plot grouped bar chart with error bars
ggplot(data = df_summary, aes(x = Stratification1, y = Data_Value.x, fill = Stratification1)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = Data_Value.x - Data_Value.y, ymax = Data_Value.x + Data_Value.y), 
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = c("pink", "blue")) +
  labs(x = "Gender", y = "Heart Disease Mortality Rate", 
       title = "Aggregate Total of Heart Disease Mortality By Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)))

#Southern States Male V. Female visualizations
df3 <- df1 %>%
  select(Stratification1, Data_Value, lat, long) %>%
  filter(lat < 39) %>%
  filter(Stratification1 %in% c("Male", "Female")) %>%
  group_by(Stratification1, Data_Value)

ggplot(data = df3, aes(x = Stratification1, y = Data_Value, fill = Stratification1)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("pink", "blue"), guide = FALSE) +
  labs(x = "Gender", y = "Heart Disease Mortality Rate",
       title = "Comparison of Heart Disease Mortality Rates by Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, max(df3$Data_Value) * 1.1))

southern_leaflet <- df1 %>%
  select(lat, long) %>%
  filter(lat < 39)

leaflet(southern_leaflet) %>%
  addTiles() %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~n)

#Northern States Male v. Female Visualizations
df4 <- df1 %>%
  select(Stratification1, Data_Value, lat, long) %>%
  filter(lat > 39) %>%
  filter(Stratification1 %in% c("Male", "Female")) %>%
  group_by(Stratification1, Data_Value)

ggplot(data = df4, aes(x = Stratification1, y = Data_Value, fill = Stratification1)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("pink", "blue"), guide = FALSE) +
  labs(x = "Gender", y = "Heart Disease Mortality Rate",
       title = "Comparison of Heart Disease Mortality Rates by Gender") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        legend.position = "none") +
  coord_cartesian(ylim = c(0, max(df3$Data_Value) * 1.1))

northern_leaflet <- df1 %>%
  select(lat, long) %>%
  filter(lat > 39)

leaflet(northern_leaflet) %>%
  addTiles() %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~n)

