library(tidyverse)
library(janitor)
library(scales)
library(tor)
library(readxl)

source('scripts/gg_theme.R')

op_is_window <- Sys.info()[1] == "Windows"
if(op_is_window == T){
    source('C:/Users/emeador/Downloads/all_functions.R')
} else {
    source('~/Downloads/all_functions.R')
}

# All projects ---------------
if(op_is_window == T){
    load('C:/Users/emeador/OneDrive - SRUC/LEADER/data/projects_full.RData')
} else {
    print('Load LEADER DATA')
}


# 2006 - 2013 ---------------

programmes_2000_2006 <- list()

programmes_2000_2006 <- map(1:6, function(x) {
    read_excel("data/Import/Copy of 2000-2006+programmes+-+grant+amounts(1).xlsx",
               sheet = x) %>%
        clean_names()
})

names(programmes_2000_2006) <-
    c('East',
      'West_ERDF',
      'West_ESF',
      'Obj_3',
      'H_and_I',
      'LEADER_plus')

map2(programmes_2000_2006,
     names(programmes_2000_2006),
     function(x, y) {
         save(x,
              file = paste0('data/', y, '.RData'))
     })

load_rdata(path = 'data')









# Merge with postcodes --------------------------

load("~/R/Gender_Pay_UK/data/postcode_urban_rural.RData")
load("~/R/Gender_Pay_UK/data/scottish_postcode_centroids.RData")
load('data/post_code_SIMD.RData')

post_clean <- function(x) {
    x <- str_trim(x)
    x <- str_squish(x)
    x <- str_to_lower(x)
    x <- str_remove_all(x, ' ')

}


post_lookup <- postcode_urban_rural %>%
    rename(organisation_postcode = 1) %>%
    mutate(organisation_postcode = post_clean(organisation_postcode)) %>%   distinct() %>%
    select(organisation_postcode,
           contains('UR'))

post_centroids <- scottish_postcode_centroids  %>%
    rename(organisation_postcode = 1) %>%
    mutate(organisation_postcode = post_clean(organisation_postcode))


projects_working <- projects_full %>%
    mutate(organisation_postcode =
               post_clean(organisation_postcode)) %>%
    left_join(post_lookup) %>%
    left_join(post_centroids) %>%
    mutate(osnrth1m = as.numeric(osnrth1m)) %>%
    mutate_at(vars(contains('date')), list(dmy)) %>%
    left_join(post_code_SIMD %>%
                  mutate(organisation_postcode =
                             post_clean(Postcode)))

save(projects_working, file = 'data/projects_working.RData')


projects_working %>%
    write_csv('data/LEADER_projects_working.csv')











