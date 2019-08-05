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


df_ls <- projects_working %>% 
    count(local_action_group, 
          farm_diversification_enterprise_sub_type, 
          sort = T)%>% 
    group_split(farm_diversification_enterprise_sub_type) %>% 
    map(., function(x){
        x %>% 
            select(1, 3)
    }) 


# merge into one table

table_remake <- projects_working %>% 
    distinct(local_action_group)

for (i in seq_along(df_ls)){
    table_remake <- table_remake %>% 
        left_join(df_ls[[i]], 
                  by = "local_action_group")
}


status_intrest <- c('Withdrawn', 
                    'Rejected', 
                    'Rework')

df_ls.i <- projects_working %>% 
    filter(status %in% status_intrest) %>% 
    count(local_action_group, status) %>% 
    group_split(status) %>% 
    map(., function(x){
        x %>% 
            select(1, 3)
    }) %>% 
    set_names('reject', 'rework', 'withdrawn')


for (i in seq_along(df_ls.i)){
    table_remake <- table_remake %>% 
        left_join(df_ls.i[[i]], 
                  by = "local_action_group")
}


new.names <- c('local_action_group',unique(projects_working$farm_diversification_enterprise_sub_type), 'reject', 'rework', 'withdrawn')


names(table_remake) <- new.names

table_remake

lag_s.p <- function(.df, .var) {
    .df %>%
        mutate(total = sum(!!ensym(.var), na.rm = T)) %>% 
        group_by(local_action_group) %>% 
        mutate(sum = sum(!!ensym(.var), 
                         na.rm = T), 
               per.tot = percent(sum/total)) %>% 
        select(local_action_group, sum, per.tot) %>% 
        distinct() }




# approved project costs

df <- projects_working %>% 
    group_by(local_action_group) %>% 
    summarise(approved_project_cost = 
                  sum(approved_project_cost, na.rm = T), 
              approved_grant = 
                  sum(approved_grant, na.rm = T), 
              match_funding = 
                  sum(match_funding, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(tot = approved_grant + match_funding) %>% 
    glimpse()






Eva_table_remake <- table_remake %>% 
    left_join(df) %>% 
    arrange(local_action_group) %>% 
    mutate_at(vars(names(.)), replace_na, 0) 



eva.names <- names(Eva_table_remake)


names(Eva_table_remake) <- 
    str_replace_all(eva.names, '_', ' ') %>% 
    str_to_title()




# Promoter_type -----------------
projects_full %>% 
    count(promoter_type, organisation_name, sort = T) %>% 
    arrange(promoter_type)

projects_full %>% 
    summarise(app_grant = mean(approved_grant, na.rm = T), 
              sd = sd(approved_grant, na.rm = T))


projects_full %>% 
    drop_na(approved_grant) %>% 
    ggplot(aes(approved_grant))+
    geom_histogram()+
    scale_x_continuous(labels = dollar)+
    facet_grid(promoter_type~.)



projects_full %>% 
    group_by(promoter_type) %>% 
    summarise(min  = min(approved_grant, na.rm = T), 
              max = max(approved_grant, na.rm = T), 
              mean = mean(approved_grant, na.rm = T), 
              median = median(approved_grant, na.rm = T), 
              n = n())





projects_full %>% 
    filter(status %in% c('Rejected', 'Live')) %>% 
    count(promoter_type, status, sort = T) %>% 
    arrange(promoter_type) %>% 
    na.omit() %>% 
    spread(status, n) %>% 
    mutate(ratio = Live/Rejected)





leader_2000_2006 <- read_csv('C:/Users/emeador/Downloads/Leader_2000_2000_data.csv', locale = locale(encoding = "windows-1252")) %>% 
    clean_names() %>% 
    mutate(leader_grant = parse_number(leader_grant))


promoter_type_ls <- projects_full %>% 
    select(organisation_name, promoter_type) %>% 
    mutate(organisation_name = str_to_lower(organisation_name)) %>% 
    group_split(promoter_type) %>% 
    map(., function(x){
        x %>% 
            pull(organisation_name) %>% 
            unique()
    }) %>% 
    set_names('sme', 'public', 'other', 'missing')


# project sponser is the organisation

# 2006 project that also appear in 2015

leader_2006_match <- 
    map2_df(promoter_type_ls, names(promoter_type_ls), function(x, y){
        leader_2000_2006 %>% 
            mutate(project_sponsor = str_to_lower(project_sponsor), 
                   leader_grant = parse_number(leader_grant)) %>% 
            filter(project_sponsor %in% x) %>% 
            group_by(project_sponsor) %>% 
            mutate(n = n(), 
                   mean_grant = mean(leader_grant, na.rm = T), 
                   promoter_type = y) %>% 
            ungroup() %>% 
            select(project_sponsor_2006 = project_sponsor, promoter_type, approved_grant_2006 = mean_grant, n_2006 = n)
    }) %>% 
    distinct()



match_2006_v <- leader_2006_match %>% 
    pull(project_sponsor_2006) %>% 
    unique()




leader_2016_match <- projects_full %>% 
    mutate(organisation_name = str_to_lower(organisation_name)) %>% 
    filter(organisation_name %in% match_2006_v) %>% 
    group_by(organisation_name) %>% 
    mutate(approved_grant_2016 = mean(approved_grant, na.rm = T), 
           n_2016 = n()) %>% 
    select(organisation_name, approved_grant_2016, n_2016) %>% 
    distinct()




match_project_type <- leader_2006_match %>% 
    left_join(leader_2016_match, 
              by = c('project_sponsor_2006' = 'organisation_name'))



View(match_project_type)



leader_2000_2006 %>% 
   glimpse()

# Write to file for Rob --------------

LEADER.table <- list(projects_working,leader_2000_2006, Eva_table_remake,match_project_type)

names(LEADER.table) <- c('LARCS+', 'LEADER_2000_2006', 'Eva_table_remake', 'project_matches')

unlink('data/LEADER_process.xlsx')
writexl::write_xlsx(LEADER.table, 
                    path = 'data/LEADER_process.xlsx', 
                    col_names = T)










