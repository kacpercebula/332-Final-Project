library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)

rm(list = ls())

###-----HYPOTHESIS-----###
# Taking the two states with the highest mortality rate from heart disease and 
# the two states with the lowest mortality rate from heart disease for this hypothesis
# OR lowest, middlest, highest and compare all three. 

# Can a few reasons for the difference in mortality rates between states be the 
# smoking rates?, physical activity rates?, and differences in overall food culture? money? education?
###--------------------###

###--Main Data--###
#Reading in the main data and forming sub data
HDMD <- read_rds("HDMD.rds")
HDMDstates <- HDMD %>%
  filter(GeographicLevel == "State") %>%
  filter(Stratification1 == "Overall") %>%
  filter(Stratification2 == "Overall")

HDMDCountyStates <- HDMD %>%
  filter(GeographicLevel == "County" && Stratification1 == "Overall" && Stratification2 == "Overall") %>%
  filter(LocationAbbr == "MN" | LocationAbbr == "MS" | LocationAbbr == "PN")

HDMDthreeStates <- HDMDstates %>%
  filter(LocationDesc == "Mississippi" | LocationDesc == "Minnesota" | LocationDesc == "Pennsylvania")
###-------------###

###--Secondary Data--###
#Reading in smoking rates data for Q1
smokingRate <- readRDS("smokingRates.rds")

#Reading in healthy lifestyle rates data for Q2 & Q3
riskFactors <- readRDS("riskFactors.rds") 


###------------------###


###-Q1-###

## Simple visual to show difference in states
ggplot(HDMDthreeStates, aes(x = LocationDesc, y = Data_Value, fill = LocationDesc)) +
  geom_bar(stat = "identity") +
  labs(x = "State", y = "Value (per 100,000)", title = "Best, Worst, Middle States for heart Disease") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 25)) +
  theme(legend.position = "none")

## Simple data frame grouped by location and response ####### add years???
smokingRateStates <- smokingRate %>%
  filter(LocationDesc == "Mississippi" | LocationDesc == "Minnesota" | LocationDesc == "Pennsylvania") %>%
  group_by(LocationDesc, Response) %>%
  summarize(average_percentage = mean(Data_Value)) %>%
  na.omit()

smokingRateAvg <- smokingRateStates %>%
  group_by(LocationDesc) %>%
  summarize(average_percentage = mean(average_percentage)) %>%
  na.omit()

#2 Simple visual to show if there is an association with HDMD and secondary set for smoking
ggplot(smokingRateStates, aes(x = Response, y = average_percentage, fill = LocationDesc)) +
  geom_bar(stat = "identity") +
  labs(x = "Response", y = "Average Percentage", title = "Average Percentage by Response and Location") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 15)) +
  theme(legend.position = "right")

#3 same as #1
ggplot(smokingRateAvg, aes(x = LocationDesc, y = average_percentage, fill = LocationDesc)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Location", y = "Average Percentage", title = "Average Percentage by Location, Grouped by Response") +
  theme_minimal() +
  theme(legend.position = "right")

#1 Simple visual to show if there is an association with HDMD and secondary set for smoking
ggplot(smokingRateStates, aes(x = Response, y = average_percentage, fill = Response)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(average_percentage, 1)), vjust = -.2) +
  facet_wrap(~ LocationDesc, scales = "free_x") +
  labs(x = "Response", y = "Average Percentage", title = "Average Percentage by Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 7, angle = 25)) +
  theme(legend.position = "none")

#4 using every day(-), current(-), and former(+) to make statistic
smokingRateBig2 <- smokingRateStates %>%
  filter(Response == "Current" | Response == "Former") %>%
  group_by(LocationDesc) %>%
  mutate(diff = average_percentage - lag(average_percentage)) %>%
  summarize(diff) %>%
  na.omit()
ggplot(smokingRateBig2, aes(x = LocationDesc, y = diff, fill = diff > 0)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(x = "Location", y = "Difference between Current and Former", title = "Difference Comparison") +
  scale_fill_manual(values = c("red", "light green"), guide = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###----###



###-Q2-&-Q3-###

## data frame with just the three states
riskFactorsStates <- riskFactors %>%
  filter(YearStart == "2011" | YearStart == "2012" | YearStart == "2013" | YearStart == "2014" | 
           YearStart == "2015" | YearStart == "2016" | YearStart == "2017") %>%
  filter(LocationDesc == "Pennsylvania" | LocationDesc == "Minnesota" | LocationDesc == "Mississippi") %>%
  select(YearStart, LocationDesc, Class, Question, Data_Value, 
         ClassID, TopicID, StratificationCategory1, Stratification1) %>%
  rename("Stratification" = Stratification1) %>%
  na.omit()

#Cleaning the quesiton to be shorter
riskFactorsStates[riskFactorsStates == 'Percent of adults who engage in no leisure-time physical activity'] <- 'No Activity'
riskFactorsStates[riskFactorsStates == 'Percent of adults who engage in muscle-strengthening activities on 2 or more days a week'] <- 'Muscle-Strengthening 2 or More Days'
riskFactorsStates[riskFactorsStates == 'Percent of adults who achieve at least 300 minutes a week of moderate-intensity aerobic physical activity or 150 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)'] <- 'Moderate Weekly Cardio'
riskFactorsStates[riskFactorsStates == 'Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic activity (or an equivalent combination)'] <- 'Intense Weekly Cardio'
riskFactorsStates[riskFactorsStates == 'Percent of adults who achieve at least 150 minutes a week of moderate-intensity aerobic physical activity or 75 minutes a week of vigorous-intensity aerobic physical activity and engage in muscle-strengthening activities on 2 or more days a week'] <- 'Intense Weekly Cardio & Muscle-Strengthening'
riskFactorsStates[riskFactorsStates == 'Percent of adults aged 18 years and older who have obesity'] <- 'Obesity Classification'
riskFactorsStates[riskFactorsStates == 'Percent of adults aged 18 years and older who have an overweight classification'] <- 'Overweight Classification'



# visuals showing overall physical activity rates (no activity and some activity) overtime for each state
rfActivityRatesTime <- riskFactorsStates %>%
  filter(Class == "Physical Activity" & Stratification == "Total") %>%
  group_by(Question, LocationDesc) 
ggplot(rfActivityRatesTime, aes(x = YearStart, y = Data_Value, color = Question)) +
  geom_line() +
  facet_wrap(~ LocationDesc, scales = "free", ncol = 1) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 60)
##DONE

# visuals showing overall obesity rates (overweight and obese) overtime for each state   merge these two?
rfObesityRatesTime <- riskFactorsStates %>%
  filter(Class == "Obesity / Weight Status" & Stratification == "Total") %>%
  group_by(Question, LocationDesc) 
ggplot(rfObesityRatesTime, aes(x = YearStart, y = Data_Value, color = Question)) +
  geom_line() +
  facet_wrap(~ LocationDesc, scales = "free", ncol = 1) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 60)

## use the ones with some activity at least or also include no activity? I'm leaning towards combining 
  # overweight and obese and use some activity rates but could also use no activity?? idk tough to think, 
  # maybe do some for both??

# visuals showing overall obesity and pa over time for each state by Education
rfpaEducationEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Education") %>%
  filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfpaEducationEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)

rfobEducationEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Education") %>%
  filter(Question %in% c("Obesity Classification", "Overweight Classification")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfobEducationEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)


# visuals showing overall obesity and pa over time for each state by Age
rfpaAgeEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Age (years)") %>%
  filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfpaAgeEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)

rfobAgeEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Age (years)") %>%
  filter(Question %in% c("Obesity Classification", "Overweight Classification")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfobAgeEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)

# visuals showing overall obesity and pa over time for each state by Gender
rfpaGenderEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Gender") %>%
  filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfpaGenderEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)

rfobGenderEffectTime <- riskFactorsStates %>%
  filter(StratificationCategory1 == "Gender") %>%
  filter(Question %in% c("Obesity Classification", "Overweight Classification")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfobGenderEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)



# visuals showing overall obesity and pa over time for each state by Income
rfpaIncomeEffectTime <- riskFactorsStates %>%
  filter(Stratification != "Data not reported") %>%
  filter(StratificationCategory1 == "Income") %>%
  filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfpaIncomeEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)

rfobIncomeEffectTime <- riskFactorsStates %>%
  filter(Stratification != "Data not reported") %>%
  filter(StratificationCategory1 == "Income") %>%
  filter(Question %in% c("Obesity Classification", "Overweight Classification")) %>%
  group_by(Question, Stratification, LocationDesc)
ggplot(rfobIncomeEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
  geom_line() +
  facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
  labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
  theme_minimal() +
  ylim(0, 70)



####### NOT ENOUGH DATA ###########
# visuals showing overall obesity and pa over time for each state by Race
# rfpaRaceEffectTime <- riskFactorsStates %>%
#   filter(StratificationCategory1 == "Race/Ethnicity") %>%
#   filter(Question %in% c("Intense Weekly Cardio", "No Activity")) %>%
#   group_by(Question, Stratification, LocationDesc)
# ggplot(rfpaRaceEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
#   geom_line() +
#   facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
#   labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
#   theme_minimal() +
#   ylim(0, 70)
# 
# rfobRaceEffectTime <- riskFactorsStates %>%
#   filter(StratificationCategory1 == "Race/Ethnicity") %>%
#   filter(Question %in% c("Obesity Classification", "Overweight Classification")) %>%
#   group_by(Question, Stratification, LocationDesc)
# ggplot(rfobRaceEffectTime, aes(x = YearStart, y = Data_Value, color = Stratification)) +
#   geom_line() +
#   facet_wrap(~LocationDesc + Question, scales = "free", ncol = 2) +
#   labs(x = "Year", y = "Data Value", title = "Data Value by Year, State, and Question") +
#   theme_minimal() +
#   ylim(0, 70)



# Saving all the data
# saveRDS(HDMDthreeStates, "HDMDthreeStates .rds")
# 
# saveRDS(smokingRateStates, "smokingRateStates.rds")
# saveRDS(smokingRateAvg, "smokingRateAvg.rds")
# saveRDS(smokingRateBig2, "smokingRateBig2.rds")
# 
# saveRDS(rfActivityRatesTime, "rfActivityRatesTime.rds")
# saveRDS(rfObesityRatesTime, "rfObesityRatesTime.rds")
# saveRDS(rfpaAgeEffectTime, "rfpaAgeEffectTime.rds")
# saveRDS(rfobAgeEffectTime, "rfobAgeEffectTime.rds")
# saveRDS(rfpaEducationEffectTime, "rfpaEducationEffectTime.rds")
# saveRDS(rfobEducationEffectTime, "rfobEducationEffectTime.rds")
# saveRDS(rfpaGenderEffectTime, "rfpaGenderEffectTime.rds")
# saveRDS(rfobGenderEffectTime, "rfobGenderEffectTime.rds")
# saveRDS(rfpaIncomeEffectTime, "rfpaIncomeEffectTime.rds")
# saveRDS(rfobIncomeEffectTime, "rfobIncomeEffectTime.rds")
















