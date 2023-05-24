library(tidyverse)
library(dplyr)
library(readxl)
library(lubridate)
library(tidymodels)
library(ggplot2)
library(tidyr)
library(cluster)

#set working directory
setwd("~/Documents/Data332")

df <- read.csv('US_Chronic_Disease.csv')

model_data <- df %>%
  group_by(Topic) %>%
  summarise(n = n())

#mutating n column to a count scale
df_scaled <- model_data %>%
  mutate(count_scaled = scale(n))

cluster_data <- df_scaled$count_scaled

#creating vector to find k
wcss <- vector()

#creating a k plot using elbow theory to find k
for (i in 1:10) {
  kmeans_model <- kmeans(cluster_data, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

k <- 2

kmeans_model <- kmeans(cluster_data, centers = k)

#assigning cluster values to data
model_data$cluster <- kmeans_model$cluster

cluster_summary <- model_data %>%
  group_by(cluster) %>%
  summarise(
    count_mean = mean(n),
    count_min = min(n),
    count_max = max(n),
    indicator_count = n()
  )

print(cluster_summary)

