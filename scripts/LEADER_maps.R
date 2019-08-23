library(tidyverse)
library(tmap)
library(sf)
library(glue)
library(xlsx)

################################
### 1. Import SIMD shapefile ###
################################



files <- list.files('C:/Users/emeador/Downloads/', full.names = T)


str_subset(files, 'SIMD')
"SG_SIMD_2016.zip" 


unzip("C:/Users/emeador/Downloads/SG_SIMD_2016.zip", 
      exdir = 'data')


SIMD_shp <- read_sf('data/SG_SIMD_2016.shp')



##############################
### 2. Import SG shapefile ###
##############################



files <- list.files('C:/Users/emeador/Downloads/', full.names = T)


str_subset(files, 'SG_DataZoneBdry')


unzip("C:/Users/emeador/Downloads/SG_DataZoneBdry_2001.zip" , 
      exdir = 'data')


sg_datazone_bnd_shp <- read_sf('data/SG_DataZone_Bdry_2001.shp')

class(sg_datazone_bnd_shp)
names(sg_datazone_bnd_shp)

sg_datazone_bnd_shp %>% 
    as_tibble() %>% 
    select(DZ_CODE) %>% 
    arrange(DZ_CODE)

#####################################
### 3. Import LEADER Lag datazone ###
#####################################



library(readxl)
Data_zone_LEADER_lookup_GIS_Map <- read_excel("C:/Users/emeador/OneDrive - SRUC/LEADER/data/Data_zone_LEADER_lookup_GIS_Map.xlsx") 


LEADER_lookup_GIS_Map <- 
    Data_zone_LEADER_lookup_GIS_Map %>% 
    rename(DataZone = DZ_CODE, 
           LEADER_area = LEADER_ARE)




################
### 4. MERGE ###
################


names(sg_datazone_bnd_shp)

sg_datazone_bnd_shp <- sg_datazone_bnd_shp %>% 
    rename(DataZone = DZ_CODE)



LEADER_LAG <- sg_datazone_bnd_shp %>% 
    left_join(LEADER_lookup_GIS_Map, by = 'DataZone')




leader_base <- tm_shape(LEADER_LAG)+
    tm_fill('LEADER_area')




tmap_save(leader_base, filename = 'pdf/leader_base.pdf', width = 8, height = 11)


##################################################
##### Merge mapping file with LARCS database #####
##################################################

downloads <- list.files('C:/Users/emeador/Downloads/', full.names = T)


str_subset(downloads, 'projects_full')


load('data/projects_full.RData')


mapping_lags <- LEADER_LAG %>% 
  as_tibble() %>% 
  select(LEADER_area) %>% 
  distinct() %>% 
  arrange(LEADER_area)


write.xlsx(mapping_lags, 
           file = 'data_write/map_larc_key.xlsx', 
           col.names = T, 
           sheetName = 'mapping_lags')


larcs_match <- projects_full  %>% 
  select(lag = local_action_group) %>% 
  distinct() %>% 
  arrange(lag)

  

write.xlsx(larcs_match, 
           file = 'data_write/map_larc_key.xlsx', 
           col.names = T, 
           sheetName = 'larcs_match', 
           append = T)


map_key <- read_csv('data/map_larc_key.csv')


golf_df <- projects_full %>% 
  left_join(map_key, by = 'local_action_group') 


###############################################################
### Run summary analysis before merge to keep file size low ###
###############################################################


echo_df <- golf_df %>% 
  select(larc_match, 
         local_action_group, 
         approved_grant, 
         approved_project_cost, 
         intervention_rate_percent, 
         match_funding, 
         status)

echo_df %>%
  count(status, sort = T)


romeo_df <- echo_df %>% 
  mutate(rejected = case_when(status == 'Rejected' ~ 1, 
                              T ~ 0), 
         reject = rejected*100) %>% 
  group_by(larc_match) %>% 
  summarise_if(is.numeric,list(~mean(., na.rm = T))) 


################################
### Create shapefile of LAGs ###
################################

# directions here


browseURL('https://philmikejones.me/tutorials/2015-09-03-dissolve-polygons-in-r/')


LEADER_LAG$area <- st_area(LEADER_LAG)

LEADER_BORDER <- LEADER_LAG %>%
  group_by(LEADER_area) %>% 
  summarise(area = sum(area))

LAG_sf <- LEADER_BORDER %>% 
  left_join(romeo_df, by = c('LEADER_area' = 'larc_match'))

#################
### Some MAPS ###
#################



tm_style('classic')


LEADER_union <- st_union(LEADER_LAG)

LEADER_union_sim <- st_simplify(LEADER_union)


LAG_centroid <- st_centroid(LEADER_LAG)



reject_base <- 
  tm_shape(LAG_sf)+
  tm_fill('reject', 
          n = 10,
          palette = '-magma', 
          textNA = 'Ineligible Areas', 
          colorNA = '#959595', 
          title = 'Percent of rejected\nprojects')+
  tm_shape(LAG_sf)+
   tm_borders('black')+
  tm_scale_bar()+
  tm_credits(str_wrap('Crown Copyright (c) and Database Right (2019)', 35), 
             position = c('left', 'bottom'))+
  tm_layout(title = 'Percent of reject projects by Local Action Group (LAG)\n 2014-2014', 
            legend.frame = T)


tmap_save(reject_base, 
          filename = 'png/reject_base.png', 
          width = 8, 
          height = 11)


dir('png')

downloads <- list.files('C:/Users/emeador/Downloads/', full.names = T)


















