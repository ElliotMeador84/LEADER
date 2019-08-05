library(tidyverse)
library(janitor)
library(scales)
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


# Scotland la fortify
if(op_is_window == T){
    load('C:/Users/emeador/OneDrive - SRUC/LEADER/data/scotland_la_fortify.RData')
} else {
    print('Load LEADER DATA')
}









num_projs_post_code <- projects_working %>%
    add_count(thematic_objective, organisation_postcode) %>%
    select(thematic_objective, oseast1m, osnrth1m, n) %>%
    na.omit() %>% 
    mutate_at(vars(oseast1m, osnrth1m, n),list(~as.numeric(.)))

(Thematic_points_map <- ggplot() +
        geom_polygon(
            data = scotland_la_fortify,
            aes(x = long, y = lat, group = group),
            fill = 'white',
            color = 'black',
            size = .1
        ) +
        geom_jitter(
            data = num_projs_post_code,
            aes(oseast1m, 
                osnrth1m, 
                size = n, 
                color = thematic_objective), 
            alpha = .85, 
            width = .5, 
            height = .5) +
        coord_equal() +
        scale_x_continuous(labels = scientific_format(digits = 1)) +
        scale_y_continuous(labels = scientific_format(digits = 1)) +
        scale_color_manual(values = 
                               Spectral_n(
                                   n_distinct(
                                       num_projs_post_code$thematic_objective)), 
                           labels = thematic_objective_label, 
                           name = 'Thematic Objective')+
        scale_size(range = c(2, 8), 
                   name = 'Total number of projects\nwithin each postcode')+
        guides(colour = guide_legend(override.aes = list(size=5)))+
        xlab('Longitude') +
        ylab('Latitude') +
        ggtitle(
            'Scottish LEADER: Projects by Objective Theme and Postcode',
            'LAG locations are determined by centriod locations of projects\nBoundaries are local authority districts'
        ) +
        theme_bw() +
        theme(panel.grid = element_blank()
        ))

ggsave(Thematic_points_map, 
       filename = 'png/Thematic_points_map.png', 
       width = 11, 
       height = 8)






