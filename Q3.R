library(leaflet)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
rm(list = ls())


#which data sets?
NHS <- readRDS("NHS.rds")
df <- NHS

df_1 = df
df_1 = df[,c(2,4,10,11,15,21,22)]
df_1 <- filter(df_1, grepl("2014", Year))
df_1 =df_1 %>% drop_na()
df_1 <- filter(df_1, grepl("Cardiovascular Diseases", Category))


raceRisk <- filter(df_1, grepl("Race", Break_Out_Category))


ggplot(raceRisk, aes(x = Topic, fill = Break_Out)) +
  geom_bar(position = "dodge", color = "black", stat = "count") +
  labs(x = "Topic", y = "Count", title = "Risk Factors by Race") +
  theme(axis.text.x = element_text(size = 7, angle = 35)) +
  theme_minimal()

# Count the number of occurrences for each combination of Break_Out and Topic
rrCounts <- df_1 %>%
  group_by(Break_Out, Topic) %>%
  summarize(Count = n())

# Create a stacked bar plot
ggplot(rrCounts, aes(x = Break_Out, y = Count, fill = Topic)) +
  geom_bar(stat = "identity") +
  labs(x = "Race", y = "Count", title = "Risk Factors by Race") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



saveRDS(raceRisk, "raceRisk.rds")
saveRDS(rrCounts, "rrCounts.rds")


