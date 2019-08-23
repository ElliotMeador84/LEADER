library(tidyverse)
library(lubridate)
library(janitor)
library(scales)
library(xlsx)
source('scripts/gg_theme.R')

op_is_window <- Sys.info()[1] == "Windows"
if(op_is_window == T){
    source('C:/Users/emeador/Downloads/all_functions.R')
} else {
    source('~/Downloads/all_functions.R')
}


# Projects working

if(op_is_window == T){
    load('C:/Users/emeador/OneDrive - SRUC/LEADER/data/LEADER_projects_working.RData')
} else {
    print('Load LEADER DATA')
}




# Table remakes --------------

projects_working %>% 
    count(status, sort = T) %>% 
    print_all()


status_include <- 
    c('Live', 
      'Completed', 
      'Awaiting Start',
      'In Process',
      'Approved - Awaiting Signed Offer of Grant', 
      'Approved - Issue Offer of Grant', 
      'Approved', 
      'Approved with Conditions', 
      'Final Claim Made')


lag_cleaner <- function(x){
    x <- str_remove_all(x, 'Local Action Group')
    x <- str_remove_all(x, 'LEADER')
    x <- str_replace_all(x, '\\band\\b', '&')
    x <- str_squish(str_trim(x))
    x
}


eva_table <- list()

# Percent withdrawn


eva_table$percent_withdrawn <- projects_working %>% 
    select(lag = local_action_group, 
           status) %>% 
    mutate(withdrawn = case_when(status == 'Withdrawn'~1, T~0)) %>% 
    group_by(lag) %>% 
    summarise(percent_withdrawn = percent(mean(withdrawn, na.rm = T))) %>% 
    arrange(lag) %>% 
    mutate(lag = lag_cleaner(lag)) %>% 
    set_names('lag', 
              'Percent withdrawn')



projects_working %>%
  count(status, sort = T) %>% 
  print_all()


# Percent rejected
eva_table$percent_rejected <- projects_working %>% 
  select(lag = local_action_group, 
         status) %>% 
  mutate(rejected = case_when(status == 'Rejected'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_rejected = percent(mean(rejected, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            'Percent rejected')


# Argyll has a lot of 
projects_working %>% 
  filter(str_detect(local_action_group, 'Argyl')) %>% 
  count(status, sort = T) %>% 
  mutate(status = fct_reorder(status, n)) %>% 
  ggplot(aes(status, n))+
  geom_col()+
  coord_flip()+
  theme_minimal()+
  labs(title = 'Argyll Status Counts')

# Percent rework
eva_table$percent_rework <- projects_working %>% 
  select(lag = local_action_group, 
         status) %>% 
  mutate(rework = case_when(status == 'Rework'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_rework = percent(mean(rework, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            'Percent rework')






projects_working %>% 
  count(farm_diversification_enterprise_sub_type, sort = T)


 # Community
eva_table$percent_community <- projects_working %>% 
  filter(status %in% status_include) %>% 
  select(lag = local_action_group, 
         type = farm_diversification_enterprise_sub_type) %>% 
  mutate(community = case_when(type == 'Community'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_community = percent(mean(community, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            '*Community')



# Small Medium Enterprise - Micro <10 Staff
eva_table$percent_sme_micro <- projects_working %>% 
  filter(status %in% status_include) %>% 
  select(lag = local_action_group, 
         type = farm_diversification_enterprise_sub_type) %>% 
  mutate(sme_micro = case_when(type == 'Small Medium Enterprise - Micro <10 Staff'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_sme_micro = percent(mean(sme_micro, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            '*Small Medium Enterprise - Micro <10 Staff')




# Small Medium Enterprise - Small < 50 Staff
eva_table$percent_sme_small <- projects_working %>% 
  filter(status %in% status_include) %>% 
  select(lag = local_action_group, 
         type = farm_diversification_enterprise_sub_type) %>% 
  mutate(sme_small = case_when(type == 'Small Medium Enterprise - Small <50 Staff'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_sme_small = percent(mean(sme_small, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            '*Small Medium Enterprise - Small <50 Staff')



# Farm Diversification
eva_table$percent_farm_diversification <- projects_working %>% 
  filter(status %in% status_include) %>% 
  select(lag = local_action_group, 
         type = farm_diversification_enterprise_sub_type) %>% 
  mutate(farm_div = case_when(type == 'Farm Diversification'~1, T~0)) %>% 
  group_by(lag) %>% 
  summarise(percent_farm_div = percent(mean(farm_div, na.rm = T))) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            '*Farm Diversification')




# Number of applications
eva_table$number_applications <- projects_working %>% 
  count(lag = local_action_group) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            'Number of applications')


# Approval rate
eva_table$number_approvals <- projects_working %>% 
  filter(status %in% status_include) %>%
  count(lag = local_action_group) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag)) %>% 
  set_names('lag', 
            'Number of approvals')


eva_table$approval_rate <- number_applications %>% 
  bind_cols(select(number_approvals, -lag)) %>% 
  mutate(approval_rate = percent(`Number of approvals`/ `Number of applications`)) %>% 
  select(lag, `Approval rate` = approval_rate)



lag_df <- projects_working %>% 
  distinct(lag = local_action_group) %>% 
  arrange(lag) %>% 
  mutate(lag = lag_cleaner(lag))




eva_table

for (i in seq_along(eva_table)){
  lag_df <-   lag_df %>% 
    left_join(eva_table[[i]], by = 'lag')
}

lag_df <- lag_df %>% 
  rename('Local Action Group' = lag)

write.xlsx(lag_df,
           file = 'data_write/LEADER_data_table.xlsx', 
           sheetName = 'Eva_table_remake')

























