
# ## EDSD 2024/2025
# Course name: Measuring Social and Generational Inequality###
# Instructor: Bernhard Binder-Hammer
# Student name:Gilbert Habaasa

# -----Tuesday 13 May 2025-----------#

# Homework: Measuring changes in income per capita in Europe after the 2008 economic crisis
# How did income change since the financial crisis 2008:
#   => Measure income per capita in constant (e.g. 2020) Euros for the period 2008 – 2023?
#   1. Choose two countries
#   2. Use national accounts data (if possible household sector only, measure in 2020 Euros). 
#   3. Make a line plot of the changes over time

#-------------------------------------------------------------------------------#
#SOLUTION: To answer the question, i choose Luxembourg and Hungary as my case study

#1.Setting Directory
setwd("C:/Users/admin/OneDrive - London School of Hygiene and Tropical Medicine/‌INED 2024/Measuring Generational and Social/Data analysis/income-percapita-changes-in-europe")

#2. Load packages
library(readr)
library(readxl)
library(tidyverse)
library(dplyr)
library(ggplot2)

#-------------------------------------------------------------------------------#
#3.Load disposable income net

netdisp_income <- read_excel("nasa_10_nf_tr__custom_16863071_spreadsheet.xlsx", 
                             sheet = "Sheet 1", skip = 9)
View(netdisp_income)

#remove empty row
netdisp_income <- netdisp_income[-1, ]
view(netdisp_income)

#rename column name from time to country
names(netdisp_income)
netdisp_income <- netdisp_income %>% 
  rename(country="TIME") 
head(netdisp_income)

#select 2 european countries
netdisp_income1 <- netdisp_income %>% 
 filter(country=="Luxembourg"|country=="Hungary")
head(netdisp_income1)
view(netdisp_income1)

#delete columns that have NA
netdisp_income1 <- netdisp_income1 %>% 
  select(where(~ !any(is.na(.))))
netdisp_income1

#Change the data arrangement
netdisp_income2 <- netdisp_income1 %>% 
  pivot_longer(c('2008':'2023'), names_to="year", values_to="income") #no data for 2024
view(netdisp_income2)
str(netdisp_income2)


#-------------------------------------------------------------------------------#
#4.Get Population counts
popcount <- read_excel("demo_pjan__custom_16684149_spreadsheet.xlsx", 
                       sheet = "Sheet 1", skip = 8)
View(popcount)

#rename column from time to country
popcount <- popcount %>% 
  rename(country="TIME")

#filter countries of choice
#view(popcount)
popcount1 <- popcount %>% 
  filter(country=="Luxembourg"|country=="Hungary")
view(popcount1)

#remove columns with NA information
popcount1 <- popcount1 %>% 
  select(where(~ !any(is.na(.))))
view(popcount1)


#delete column with wrong information labelled b="b"
popcount1 <- popcount1[,-7]

view(popcount1)

#Change the data arrangement
popcount2 <- popcount1 %>% 
  pivot_longer(c('2008':'2023'),names_to = "year", values_to = "population")
view(popcount2)


#-------------------------------------------------------------------------------#
#5.Load Consumer Price Index (CPI) data
cpi <- read_excel("prc_hicp_aind__custom_16663801_spreadsheet.xlsx", 
                  sheet = "Sheet 1", skip = 7)
View(cpi)


#rename column from time to country
cpi <- cpi %>% 
  rename(country="TIME")
view(cpi)

#filter countries of choice
cpi1 <- cpi %>% 
  filter(country=="Luxembourg"| country=="Hungary")
view(cpi1)

#remove columns with NA information
cpi1 <- cpi1 %>% 
  select(where(~ !any(is.na(.))))
view(cpi1)


#2016 to 2019 variables are numbers while others are character
 str(cpi1)  #check structure of dataset before changing tibble arrangement 

  cpi1 <- cpi1 %>%
  mutate(across(c('2016', '2017', '2018', '2019'), as.character)) #make all as character
  view(cpi1)
  str(cpi1)
  
##Change the data arrangement
  
   cpi2 <- cpi1 %>% 
    pivot_longer(c('2008':'2023'), names_to = "year", values_to = "cpi")
  view(cpi2) 
  
  
#6.Join 3 datasets and make it ready for calculation of income per capita
   pcap_income <- netdisp_income2 %>%
    left_join(popcount2, by = c("country", "year")) %>%
    left_join(cpi2, by = c("country", "year"))
  view(pcap_income)
  
#7. Calculate percapita income taking into account the consumer price index
  str(pcap_income) #but income, population and cpi are all characters and should be converted to numeric before calculation
  
  #percapita income calculation
  pcap_income1 <- pcap_income %>% 
    mutate(
      income = as.numeric(income),
      cpi=as.numeric(cpi),
      population = as.numeric(population),
      percapita_income = income * 1000000 / population
    )
  
 
#8. draw a graph showing percapita income
  pcap_income1 %>%
    filter(country %in% c("Luxembourg", "Hungary")) %>%
    ggplot(aes(x = year, y = percapita_income, color = country, group = country)) +
    geom_line(size = 1.2) +
    geom_point(size = 2) +
    labs(
      x = "Year",
      y = "Per Capita Income in Euros",
      title = "Per Capita Income for Luxembourg and Hungary"
    ) +
    scale_color_manual(values = c("Hungary" = "#F8766D", "Luxembourg" = "#00BFC4")) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(size = 12),
      legend.title = element_blank()
    )
  
  #9.Save the graph
  ggsave("percapita_income_for_Luxembourg_and_Hungary.jpeg", width = 8, height = 6)
  ggsave("percapita_income_Luxembourg_Hungary.png", width = 8, height = 6, bg="white")

  
