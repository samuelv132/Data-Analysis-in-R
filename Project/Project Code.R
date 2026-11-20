# 1. Loading the required packages
#install.packages("bdl")
#install.packages("purrr")
#install.packages("janitor")

library(tidyr)
library(bdl)
library(dplyr)
library(purrr)
library(janitor)

#Data Cleaning Procedurce
#1. Data Frame for each variable
#2. Concatenate all of the dataframes so that everything is under 1 dataframe

# 2. Definition of the fixed year range (2009-2025)
years <- 2009:2025

#All var taken on level 2 as level 3 is not available


#________________________________________________________________________
#Variable 1- Local Units Raw

df_local_units_raw <- get_data_by_variable(
  varId = c("386723","386718","386713","498851","386708","386703"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_local_units_raw <-df_local_units[,c(0:9)]
#View(df_local_units)

#Conversion to columns
df_local_units_clean <- df_local_units_raw %>%
  pivot_longer(
    cols = starts_with("val_"),
    names_to = "var_id",
    names_prefix = "val_",
    values_to = "values"
  ) %>%
  mutate(
    label = case_match(
      var_id,
      "386723"~"Total",
      "386718"~"Small (Total)",
      "386713"~"Micro",
      "498851"~"Small",
      "386708"~"Medium",
      "386703"~"Large",
      ))

df_local_units_clean <-df_local_units_clean %>% rename(company_size = label)
df_local_units_clean$metric_type <- "Local Units"
View(df_local_units_clean)

#________________________________________________________________________
#Variable 2- employed persons raw
df_employed_persons_raw <- get_data_by_variable(
  varId = c("386719","386714","386709","498847","386704","386699"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_employed_persons_raw <-df_employed_persons_raw[,c(0:9)]
View(df_employed_persons_raw)

#Conversion to columns
df_employed_persons_clean <- df_employed_persons_raw %>%
  pivot_longer(
    cols = starts_with("val_"),
    names_to = "var_id",
    names_prefix = "val_",
    values_to = "values"
  ) %>%
  mutate(
    label = case_match(
      var_id,
      "386719"~"Total",
      "386714"~"Small (Total)",
      "386709"~"Micro",
      "498847"~"Small",
      "386704"~"Medium",
      "386699"~"Large",
    ))

df_employed_persons_clean <-df_employed_persons_clean %>% rename(company_size = label)
df_employed_persons_clean$metric_type <- "Employed Persons"
View(df_employed_persons_clean)



#________________________________________________________________________
#Variable 3- Gross Wages/Sal

df_gross_wages_raw <- get_data_by_variable(
  varId = c("386720","386715","386710","498848","386705","386700"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_gross_wages_raw <-df_gross_wages_raw[,c(0:9)]
#View(df_gross_wages)

#Conversion to columns
df_gross_wages_clean <- df_gross_wages_raw %>%
  pivot_longer(
    cols = starts_with("val_"),
    names_to = "var_id",
    names_prefix = "val_",
    values_to = "values"
  ) %>%
  mutate(
    label = case_match(
      var_id,
      "386720"~"Total",
      "386715"~"Small (Total)",
      "386710"~"Micro",
      "498848"~"Small",
      "386705"~"Medium",
      "386700"~"Large",
    ))

df_gross_wages_clean <-df_gross_wages_clean %>% rename(company_size = label)
df_gross_wages_clean$metric_type <- "Gross Wages"
View(df_gross_wages_clean)

#______________________________________________________________________

#Variable 4- Total Revenues

df_total_revenues_raw <- get_data_by_variable(
  varId = c("386721","386716","386711","498849","386706","386701"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_total_revenues_raw <-df_total_revenues_raw[,c(0:9)]
#View(df_total_revenues)

#Conversion to columns
df_total_revenues_clean <- df_total_revenues_raw %>%
  pivot_longer(
    cols = starts_with("val_"),
    names_to = "var_id",
    names_prefix = "val_",
    values_to = "values"
  ) %>%
  mutate(
    label = case_match(
      var_id,
      "386721"~"Total",
      "386716"~"Small (Total)",
      "386711"~"Micro",
      "498849"~"Small",
      "386706"~"Medium",
      "386701"~"Large",
    ))

df_total_revenues_clean <-df_total_revenues_clean %>% rename(company_size = label)
df_total_revenues_clean$metric_type <- "Total Revenues"
View(df_total_revenues_clean)

#_______________________________________________________________________
#Concatenate the clean dataframes

df_clean_0 <- rbind(
  df_total_revenues_clean,df_local_units_clean,df_gross_wages_clean,df_employed_persons_clean)#concatenate
df_clean_1 <- df_clean_0[!(df_clean_0$company_size == "Small (Total)" & df_clean_0$year %in% 2015:2025), ] #Small total limit
df_clean_2 <- df_clean_1[!(df_clean_1$company_size == "Small" & df_clean_1$year %in% 2011:2014), ] # Small only limit
df_clean_2$company_size[df_clean_2$company_size == "Small (Total)"] <- "Small"
View(df_clean_2)



