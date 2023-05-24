# Heart Disease Mortality Analysis ❤️

Shiny App Link:
https://kacpercebula.shinyapps.io/Heart-Disease-Mortality-Analysis/

## Introduction
- This project utilizes a Heart Disease Mortality Rate dataset to conduct a comprehensive analysis, exploring various factors and their correlations with mortality rates. Through interactive visualizations and statistical models, the project highlights the significance of factors such as age, education, gender, income, and geographical location in influencing mortality rates. Minnesota consistently emerges as a state with favorable outcomes, while Mississippi tends to have lower rankings. The project emphasizes the complexity of mortality rates, demonstrating the interplay of multiple factors and the need for a holistic approach in addressing heart disease prevention and intervention strategies.
---

## Data Cleaning 
1. Imported necessary libraries to run visualizations and analysis such as leaflet, dplyr, tidyr, ggplot2, shiny, shinydashboard.
2. Imported the main datasets used to create pivot tables later on.
```r
HDMD <- read_rds("HDMD.rds")
HDMD18 <- readRDS('HDMD18.rds')
NHS <- readRDS("NHS.rds")
smokingRate <- readRDS("smokingRates.rds")
riskFactors <- readRDS("riskFactors.rds") 
chronicDis <- readRDS("chronicDis.rds")
```
3. Ran a few lines of code to clean the data such as removing NA's and fixing the lat/long.
```r
HDMD <- separate(HDMD, Location.1, into = c("lat", "long"), sep = ", ")
HDMD$lat <- as.numeric(gsub("\\(", "", gsub("\\)", "", HDMD$lat)))
HDMD$long <- as.numeric(gsub("\\(", "", gsub("\\)", "", HDMD$long)))
HDMD$lat <- round(HDMD$lat, 2)
HDMD$long <- round(HDMD$long, 2)
```


---

## Data Preparation 
- Bar Charts 
1. Made a pivot table for specific visualization, used group_by and summarize as main pivot_table making functions.
```r
smokingRateStates <- smokingRate %>%
  filter(LocationDesc == "Mississippi" | LocationDesc == "Minnesota" | LocationDesc == "Pennsylvania") %>%
  group_by(LocationDesc, Response) %>%
  summarize(average_percentage = mean(Data_Value)) %>%
  na.omit()
```
2. Made a ggplot using the pivot table to visualize the information in an appealing way.
```r
ggplot(smokingRateStates, aes(x = Response, y = average_percentage, fill = LocationDesc)) +
  geom_bar(stat = "identity") +
  labs(x = "Response", y = "Average Percentage", title = "Average Percentage by Response and Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 15)) +
  theme(legend.position = "right")
```
Another Example
1. Made a pivot table for specific visualization, used group_by and summarize as main pivot_table making functions.
```r
mean_values <- aggregate(Data_Value ~ Stratification1, data = df2, FUN = mean)
se_values <- aggregate(Data_Value ~ Stratification1, data = df2, FUN = function(x) sd(x) / sqrt(length(x)))

df_summary <- merge(mean_values, se_values, by = "Stratification1")
```
2. Made a ggplot using the pivot table to visualize the information in an appealing way.
```r
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
```

 Line Charts - 
1. Made a pivot table for specific visualization, used group_by and summarize as main pivot_table making functions.
```r
rfpaIncomeEffectTime <- riskFactorsStates %>%
  filter(Stratification != "Data not reported") %>%
  filter(StratificationCategory1 == "Income") %>%
  filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
  group_by(Question, Stratification, LocationDesc)
```
2. Made a ggplot using the pivot table with geom_tile to visualize the information in an appealing way.
```r
ggplot(rfpaIncomeEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)
```

Geospatial -
1. Made a smaller dataset of the original dataset so the map doesn't crash
2. Then made the map, set where it should view, and added markers using Lat, Lon, Date.Time and Base.
```r
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -95.7129, lat = 37.0902, zoom = 4)

map <- map %>%
  addCircleMarkers(data = mapMainData,
                   lng = ~long,
                   lat = ~lat,
                   radius = 5,
                   color = "red",
                   fillOpacity = 0.5,
                   popup = ~paste("Sum_Data Values: ", Sum_Data_Values, "<br>Data Value Unit: ", Data_Value_Unit))
```
 
---

## Shiny App
- All the visualizations and explanations of the charts can be found on the shiny app link below
- https://kacpercebula.shinyapps.io/Heart-Disease-Mortality-Analysis/
