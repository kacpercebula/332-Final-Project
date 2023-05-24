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

df <- read.csv('Heart_Disease_18.csv')

df1 <- df %>%
  select(Stratification1, Data_Value) %>%
  filter(Stratification1 %in% c("Male", "Female")) %>%
  group_by(Stratification1, Data_Value) %>%
  summarize(n = n())

#creating model dataframe from df1
model_data <- df1 %>%
  group_by(Data_Value) %>%
  summarize(total_n = sum(n))

#linear regression model
model <- lm(total_n ~ Data_Value, data = model_data)

summary(model)
