library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(wordcloud)
library(sentimentr)
library(reshape2)
library(readr)
library(shiny)
library(DT)
library(lubridate)
library(tidyverse)
library(data.table)
library(leaflet)
rm(list = ls())

###-----HYPOTHESIS-----###
# Taking the two states with the highest mortality rate from heart disease and 
# the two states with the lowest mortality rate from heart disease for this hypothesis
# OR lowest, middlest, highest and compare all three. 

# Can a few reasons for the difference in mortality rates between states be the 
# food culture, more fast food than grocery stores, and less health based emphasis 
# on lifestyle?


# Reading in the main data
HDMD <- read_rds("HDMD.rds")
HDMDstates <- HDMD %>%
  filter(GeographicLevel == "State") %>%
  filter(Stratification1 == "Overall") %>%
  filter(Stratification2 == "Overall")
HDMDcounty <- HDMD %>%
  filter(GeographicLevel == "County" && Stratification1 == "Overall" && Stratification2 == "Overall")

threeStates <- HDMD %>%
  

#Reading in secondary data for Q1



#Simple visual to show if there is an association with HDMD and secondary set 














