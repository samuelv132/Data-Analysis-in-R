# 1. Loading the required packages
#Variable where the excel file will be stored
out_path <- "D:/Github R/Data Analysis in R project/Data-Analysis-in-R/Project/wage_kpis.xlsx"  # change to your path


library(tidyr)
library(bdl)
library(dplyr)
library(purrr)
library(janitor)


#Data Cleaning Procedurce
#1. Data Frame for each variable
#2. Concatenate all of the dataframes so that everything is under 1 dataframe

#All var taken on level 2 as level 3 is not available


#________________________________________________________________________
#Variable 1- Local Units Raw

df_local_units_raw <- get_data_by_variable(
  varId = c("386723","386718","386713","498851","386708","386703"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_local_units_raw <-df_local_units_raw[,c(0:9)]
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
#View(df_local_units_clean)

#________________________________________________________________________
#Variable 2- employed persons raw
df_employed_persons_raw <- get_data_by_variable(
  varId = c("386719","386714","386709","498847","386704","386699"), # Replace with your actual IDs
  unitLevel = "2",
  lang = "en"
)
df_employed_persons_raw <-df_employed_persons_raw[,c(0:9)]
#View(df_employed_persons_raw)

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
#View(df_employed_persons_clean)



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
#View(df_gross_wages_clean)

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
#View(df_total_revenues_clean)

#_______________________________________________________________________
#Concatenate the clean dataframes

df_clean_0 <- rbind(
  df_total_revenues_clean,df_local_units_clean,df_gross_wages_clean,df_employed_persons_clean)#concatenate
df_clean_1 <- df_clean_0[!(df_clean_0$company_size == "Small (Total)" & df_clean_0$year %in% 2015:2025), ] #Small total limit
df_clean_2 <- df_clean_1[!(df_clean_1$company_size == "Small" & df_clean_1$year %in% 2011:2014), ] # Small only limit
df_clean_2$company_size[df_clean_2$company_size == "Small (Total)"] <- "Small"
colnames(df_clean_2)[colnames(df_clean_2) == "name"] <- "region"

df_clean_2<-df_clean_2
View(df_clean_2)
#detach(df_clean_2)
#_______________________________________________________________________
#names(df_clean_2)
attach(df_clean_2)
# ?unique(metric_type)
# metric_type Employed Persons      Gross Wages      Local Units   Total Revenues 
# company_size Large Medium  Micro  Small  Total 
# return values col

#=================================================
#KPI ALL For all regions
#=================================================
#multiplied by 1000 to see actual values
Avg_Wage <- sum(values[metric_type == "Gross Wages"])*1000 / sum(values[metric_type == "Employed Persons"])
Avg_Wage_Micro <- sum(values[metric_type == "Gross Wages" & company_size == "Micro" ])*1000 / 
                  sum(values[metric_type == "Employed Persons" & company_size == "Micro" ])

Avg_Wage_Small <- sum(values[metric_type == "Gross Wages" & company_size == "Small" ])*1000 / 
                  sum(values[metric_type == "Employed Persons" & company_size == "Small" ])

Avg_Wage_Medium <- sum(values[metric_type == "Gross Wages" & company_size == "Medium" ])*1000 / 
                  sum(values[metric_type == "Employed Persons" & company_size == "Medium" ])

Avg_Wage_Large <- sum(values[metric_type == "Gross Wages" & company_size == "Large" ])*1000 / 
                  sum(values[metric_type == "Employed Persons" & company_size == "Large" ])

Avg_Wage_All <-
  data.frame(
  Avg_Wage_Micro = Avg_Wage_Micro,
  Avg_Wage_Small = Avg_Wage_Small,
  Avg_Wage_Medium = Avg_Wage_Medium, 
  Avg_Wage_Large = Avg_Wage_Large)


KPI_Micro_All <- (Avg_Wage_Micro-Avg_Wage_Medium)/Avg_Wage_Medium #all regions
KPI_Small_All <- (Avg_Wage_Small-Avg_Wage_Medium)/Avg_Wage_Medium #all regions
KPI_Medium_All <- (Avg_Wage_Medium-Avg_Wage_Medium)/Avg_Wage_Medium #all regions
KPI_Large_All <- (Avg_Wage_Large-Avg_Wage_Medium)/Avg_Wage_Medium #all regions

Avg_KPI_All <-
  data.frame(
    KPI_Micro_All = KPI_Micro_All,
    KPI_Small_All = KPI_Small_All,
    KPI_Medium_All = KPI_Medium_All,
    KPI_Large_All =KPI_Large_All)

View(Avg_Wage_All)
View(Avg_KPI_All)
#=================================================
#KPI split by table and company size
#=================================================


wage_kpi <- df_clean_2 %>%
  group_by(region, year, company_size, metric_type) %>%
  summarise(total = sum(values, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = metric_type, values_from = total) %>%
  mutate(avg_wage = `Gross Wages` * 1000 / `Employed Persons`) %>%
  select(region, year, company_size, avg_wage) %>%
  pivot_wider(names_from = company_size, values_from = avg_wage,
              names_prefix = "Avg_Wage_") %>%
  mutate(KPI_Micro = (Avg_Wage_Micro - Avg_Wage_Medium) / Avg_Wage_Medium,
         KPI_Small = (Avg_Wage_Small - Avg_Wage_Medium) / Avg_Wage_Medium,
         KPI_Medium = (Avg_Wage_Medium - Avg_Wage_Medium) / Avg_Wage_Medium,         
         KPI_Large = (Avg_Wage_Large - Avg_Wage_Medium) / Avg_Wage_Medium)

View(wage_kpi)

# Matrix view: regions as rows, years as columns (one per KPI)
kpi_micro_matrix <- wage_kpi %>%
  select(region, year, KPI_Micro) %>%
  pivot_wider(names_from = year, values_from = KPI_Micro) %>%
  tibble::column_to_rownames("region") %>%
  as.matrix()

KPI_reg_year_micro<- View(round(kpi_micro_matrix, 3),title = "KPI_Micro_Reg_Year" )
KPI_reg_year_micro_xl<- round(kpi_micro_matrix, 3)

kpi_small_matrix <- wage_kpi %>%
  select(region, year, KPI_Small) %>%
  pivot_wider(names_from = year, values_from = KPI_Small) %>%
  tibble::column_to_rownames("region") %>%
  as.matrix()

KPI_reg_year_Small<- View(round(kpi_small_matrix, 3), title = "KPI_Small_Reg_Year")
KPI_reg_year_Small_xl<- round(kpi_small_matrix, 3)

kpi_medium_matrix <- wage_kpi %>%
  select(region, year, KPI_Medium) %>%
  pivot_wider(names_from = year, values_from = KPI_Medium) %>%
  tibble::column_to_rownames("region") %>%
  as.matrix()

KPI_reg_year_Medium<- View(round(kpi_medium_matrix, 3), title = "KPI_Medium_Reg_Year")
KPI_reg_year_Medium_xl<- round(kpi_medium_matrix, 3)

kpi_large_matrix <- wage_kpi %>%
  select(region, year, KPI_Large) %>%
  pivot_wider(names_from = year, values_from = KPI_Large) %>%
  tibble::column_to_rownames("region") %>%
  as.matrix()

KPI_reg_year_Large<- View(round(kpi_large_matrix, 3), title = "KPI_Large_Reg_Year")
KPI_reg_year_Large_xl<- round(kpi_large_matrix, 3)
#======================================================
#Descriptive Stats by Metric Type and company size
#======================================================
metric_summaries <- df_clean_2 %>%
  group_by(metric_type, company_size) %>%
  summarise(
    n      = sum(!is.na(values)),
    mean   = mean(values, na.rm = TRUE),
    median = median(values, na.rm = TRUE),
    sd     = sd(values, na.rm = TRUE),
    min    = min(values, na.rm = TRUE),
    max    = max(values, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_split(metric_type)

# View each metric's table
View(data.frame(metric_summaries),title="Desc Stats")

#__________________________________________________________________________

detach(df_clean_2)


#__________________________________________________________________________

library(writexl)
library(openxlsx)
#combines 4 seperate tables into one
metric_combined <- bind_rows(
  mutate(metric_summaries[[1]], metric_type = "Revenues"),
  mutate(metric_summaries[[2]], metric_type = "Local Units"),
  mutate(metric_summaries[[3]], metric_type = "Gross Wages"),
  mutate(metric_summaries[[4]], metric_type = "Employed Persons")
)
#View(metric_combined)

sheets <- list(
  "Desc_Stats"    = metric_combined,
  Avg_Wage_All        = Avg_Wage_All,
  Avg_KPI_All         = Avg_KPI_All,
  KPI_reg_year_micro  = KPI_reg_year_micro_xl,
  KPI_reg_year_Small  = KPI_reg_year_Small_xl,
  KPI_reg_year_Medium = KPI_reg_year_Medium_xl,
  KPI_reg_year_Large  = KPI_reg_year_Large_xl
)


sheets <- lapply(sheets, to_df)
write.xlsx(sheets, file = out_path, colWidths = "auto",
           headerStyle = createStyle(textDecoration = "bold"))


?write.xlsx(sheets, file = out_path, colWidths = "auto")



