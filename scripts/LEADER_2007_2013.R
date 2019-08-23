library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)

source('scripts/gg_theme.R')


LEADER_Ex_Post_2007_2013 <- read_excel("C:/Users/emeador/OneDrive - SRUC/LEADER/data/Copy of Copy of 16 - LEADER - Ex Post 2007 - 2013.xlsx") %>% 
    clean_names() 



# We think "national total" is  Scottish Government's contribution.  It is off by 50 million approx. 



# number projects and budget for three datasets



leader_07.13 <- LEADER_Ex_Post_2007_2013 %>% 
    select(lag = lag_name, 
           end_date = project_end_date_dd_mm_yyyy, 
           ec_total = ec_total_36_3_percent, 
           national_total = national_total_63_7_percent, 
           total_value = total_value_of_claim_y2_z2_aa2_ab2) %>% 
    mutate(month = floor_date(end_date, 'month'))
    








lead_month_programmes <- leader_07.13 %>% 
    add_count(month)




leader_07.13 %>% 
    select_if(is.numeric) %>% 
    summary()

ggplot()+
    geom_point(data = leader_07.13, 
               aes(end_date, total_value))+
    
    leader_theme

    



ggplot()+
    geom_line(data = lead_month_programmes, 
               aes(month, n))+
    leader_theme























