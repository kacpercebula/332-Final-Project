library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
rm(list = ls())



HDMD <- readRDS('HDMD.rds')

#Separate lat long values into two separate columns
HDMD <- separate(HDMD, Location.1, into = c("lat", "long"), sep = ", ")

#Use gsub to remove () from lat long character and make the values read as a number
HDMD$lat <- as.numeric(gsub("\\(", "", gsub("\\)", "", HDMD$lat)))
HDMD$long <- as.numeric(gsub("\\(", "", gsub("\\)", "", HDMD$long)))

#Rounding the lat long values to 2 decimal places
HDMD$lat <- round(HDMD$lat, 2)
HDMD$long <- round(HDMD$long, 2)

#Creating data frame by selecting specific columns
df1 <- HDMD %>%
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
#This chart is an aggregate number of Heart Disease Mortality comparing males to females in the United States. Overall, it shows that Males have a higher number of Heart Disease Mortality than Females. 

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
#This chart shows the total number per 100,000 residents of Heart Disease mortality between males and females in the southern part of the United States.

southern_leaflet <- df1 %>%
  select(lat, long) %>%
  filter(lat < 39)

leaflet(southern_leaflet) %>%
  addTiles() %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~n)
#This leaflet map provides location points of a Heart Disease mortality.

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
#This chart shows the total number per 100,000 residents of Heart Disease mortality between males and females in the northern part of the United States.

northern_leaflet <- df1 %>%
  select(lat, long) %>%
  filter(lat > 39)

leaflet(northern_leaflet) %>%
  addTiles() %>%
  addMarkers(lng = ~long,
             lat = ~lat,
             popup = ~n)
#This leaflet map provides location points of a Heart Disease mortality.

HDMD18 <- readRDS('HDMD18.rds')

temp <- HDMD18 %>%
  select(Stratification1, Data_Value) %>%
  filter(Stratification1 %in% c("Male", "Female")) %>%
  group_by(Stratification1, Data_Value) %>%
  summarize(n = n())

model_data <- temp %>%
  group_by(Data_Value) %>%
  summarize(total_n = sum(n))

model <- lm(total_n ~ Data_Value, data = model_data)

#This linear regression model shows correlation between heart disease mortality and gender.

summary(model)



#K MEANS CLUSTER MODEL
chronicDis <- readRDS("chronicDis.rds")

model_data <- chronicDis %>%
  group_by(Topic) %>%
  summarise(n = n())

df_scaled <- model_data %>%
  mutate(count_scaled = scale(n))

cluster_data <- df_scaled$count_scaled

wcss <- vector()

for (i in 1:10) {
  kmeans_model <- kmeans(cluster_data, centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

plot(1:10, wcss, type = "b", xlab = "Number of Clusters", ylab = "WCSS")

k <- 2

kmeans_model <- kmeans(cluster_data, centers = k)

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

saveRDS(cluster_summary, "clusterModel.rds")
saveRDS(wcss, "wcss.rds")





#Saving data
# saveRDS(df_summary, "summaryData.rds")
# saveRDS(df3, "southData.rds")
# saveRDS(df4, "northData.rds")
# saveRDS(model_data, "modelData.rds")
 saveRDS(northern_leaflet, "northLeaf.rds")
 saveRDS(southern_leaflet, "southLeaf.rds")



# extrza stuff for helping setup app

rows <- nrow(southern_leaflet)
random <- sample(rows, 350)

southern_leaflet <- southern_leaflet[random,]
northern_leaflet <- northern_leaflet[random,]

save













