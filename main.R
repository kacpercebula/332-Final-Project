library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
rm(list = ls())


df <- readRDS("HDMD.rds")
#df_1 <- readRDS("HDMD.rds")

# df=df %>% drop_na()
# df_1 = df_1 %>% drop_na()
# df$lat <- gsub("[()]", "", df$lat)
# df$long <- gsub("[()]", "", df$long)
# df_1 = df[,c(1,3,4,7,8,9,16,19,20)]
# df_1 <- filter(df_1, grepl("State", GeographicLevel))

df <- separate(df, Location.1, into = c("lat", "long"), sep = ", ")

#Use gsub to remove () from lat long character and make the values read as a number
df$lat <- as.numeric(gsub("\\(", "", gsub("\\)", "", df$lat)))
df$long <- as.numeric(gsub("\\(", "", gsub("\\)", "", df$long)))

#Rounding the lat long values to 2 decimal places
df$lat <- round(df$lat, 2)
df$long <- round(df$long, 2)

df_1 = df[,c(1,3,4,7,8,9,16,19,20)]
df_1 <- filter(df_1, grepl("State", GeographicLevel))

summarized_df_1 <- df_1 %>%
  group_by(LocationDesc) %>%
  dplyr::summarize(Total = n(), Combined_Data_Values = paste(Data_Value, collapse = ", "), Sum_Data_Values = sum(Data_Value))


# Load the required packages

df_1$lat <- as.numeric(df_1$lat)
df_1$long <- as.numeric(df_1$long)
df_coords <- data.frame(lat = df_1$lat, lng = df_1$long, Data_Value = df_1$Data_Value, Data_Value_Unit = df_1$Data_Value_Unit)

summarized_df_1 <- df_1 %>%
  group_by(LocationDesc) %>%
  summarize(
    Total = n(),
    Combined_Data_Values = paste(Data_Value, collapse = ", "),
    Sum_Data_Values = sum(Data_Value),
    long = first(long),
    lat = first(lat),
    Data_Value_Unit = first(Data_Value_Unit),
    Stratification2 = first(Stratification2)
  )

# Create a leaflet map object
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -95.7129, lat = 37.0902, zoom = 4)




# Add markers to the map
map <- map %>%
  addCircleMarkers(data = summarized_df_1,
                   lng = ~long,
                   lat = ~lat,
                   radius = 5,
                   color = "red",
                   fillOpacity = 0.5,
                   popup = ~paste("Sum_Data Values: ", Sum_Data_Values, "<br>Data Value Unit: ", Data_Value_Unit))

# Display the map
map


#Visuals 

ggplot(summarized_df_1, aes(x = LocationDesc, y = Sum_Data_Values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "State", y = "Number Of Death", title = "State By Death") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

top_ten <- summarized_df_1 %>%
  top_n(10, wt = Sum_Data_Values) %>%
  arrange(desc(Sum_Data_Values))

bottom_ten <- summarized_df_1 %>%
  top_n(-10, wt = Sum_Data_Values) %>%
  arrange(Sum_Data_Values)

# Create ggplot for the top ten rows
ggplot(top_ten, aes(x = LocationDesc, y = Sum_Data_Values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "State", y = "Number Of Death", title = "Top Ten States By Death") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create ggplot for the bottom ten rows
ggplot(bottom_ten, aes(x = LocationDesc, y = Sum_Data_Values)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "State", y = "Number Of Death", title = "Bottom Ten States By Death") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df_filtered <- df %>%
  filter(Stratification2 != "Overall")

# Create ggplot with filtered data
ggplot(df_filtered, aes(x = Stratification2, y = Data_Value)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(x = "Race", y = "Number oF Mortality Rate using the Data Value", title = "Mortality rate by Race")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


saveRDS(bottom_ten, "bottomTen.rds")
saveRDS(top_ten, "topTen.rds")
saveRDS(map, "map.rds")
saveRDS(df_filtered, "raceOverall.rds")
saveRDS(summarized_df_1, "mapMainData.rds")



