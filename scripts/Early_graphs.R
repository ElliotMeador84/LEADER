library(tidyverse)
library(lubridate)
library(scales)
source('~/all_functions.R')
source('~/R/LEADER/scripts/gg_theme.R')
options(scipen = 999)

load('~/R/LEADER/data/projects_working.RData')
load('~/R/LEADER/data/projects_full.RData')
load("~/R/Gender_Pay_UK/data/SG_8_fold_rural_urban_labels_df.RData")

projects_pross <- projects_full %>%
    mutate_at(vars(contains('date')), list(dmy))


thematic_objective_label <- function(x) {
    x <- str_squish(x)
    x <- str_replace_all(x, '\\band\\b', '&')
    x <- str_replace_all(x, '\\blags\\b', 'LAGs')
    x <- str_replace_all(x, '\\beu\\b', 'EU')
    x <- str_replace_all(x, '\\buk\\b', 'UK')
    x <- str_remove_all(x, "\\([^()]*\\)")
    x <- str_to_sentence(x)
    x <- str_wrap(x, 60)
    return(x)
}

# Thematic Objective & Duration -----------------



dates_df <- projects_pross %>%
    select(thematic_objective,
           contains('estimated'))


(
    thematic_duration_g <- dates_df %>%
        mutate(
            duration = as.numeric(estimated_end_date -
                                      estimated_start_date) / 7,
            duration = round(duration, 0)
        ) %>%
        add_count(thematic_objective) %>%
        filter(duration != 2035.,
               n >= 10) %>%
        mutate(thematic_objective = fct_reorder(thematic_objective,
                                                duration)) %>%
        na.omit() %>%
        ggplot(aes(thematic_objective, duration)) +
        geom_boxplot(fill = 'skyblue') +
        scale_x_discrete(labels = thematic_objective_label) +
        scale_y_continuous(expand = c(0.025, 0.025)) +
        coord_flip() +
        leader_theme +
        theme(legend.position = 'none') +
        labs(title = 'LEADER Thematic Objective &\nProject Duration',
             y = '# Weeks',
             x = NULL,
             caption = 'Points are statistical outliers.\nVertical lines on boxplots are median values.\nLower/upper hinges correspond to the first and third quartiles.')
)


ggsave(
    thematic_duration_g,
    filename = 'png/thematic_duration_g.png',
    width = 11,
    height = 8
)

# Thematic Objective & Approved Grant -------

projects_pross %>%
    select(contains('start'),
           contains('end')) %>%
    summarise_all(list( ~ min(., na.rm = T),
                        ~ max(., na.rm = T))) %>%
    gather(key, value) %>%
    mutate(id = row_number())


cost_df <- projects_pross %>%
    select(thematic_objective,
           contains('approved'))



thematic_obj_order <- cost_df %>%
    add_count(thematic_objective) %>%
    filter(n >= 10) %>%
    group_by(thematic_objective) %>%
    summarise(median = median(approved_grant, na.rm = T)) %>%
    arrange(desc(median)) %>%
    na.omit() %>%
    pull(thematic_objective) %>%
    rev()

pound_format <- function(x) {
    str_c('£', comma(x))
}

(
    funding_thematic_objective <- cost_df %>%
        add_count(thematic_objective) %>%
        filter(n >= 10) %>%
        na.omit() %>%
        mutate(thematic_objective = fct_relevel(thematic_objective,
                                                thematic_obj_order)) %>%
        ggplot(aes(thematic_objective, approved_grant)) +
        geom_boxplot(fill = 'skyblue') +
        scale_x_discrete(labels = thematic_objective_label) + #
        scale_y_continuous(expand = c(0.025, 0.025),
                           labels = pound_format) +
        coord_flip() +
        leader_theme +
        labs(
            title = 'LEADER Thematic Objective & Approved\n Grant Size',
            subtitle = 'Projects from 1 April 2016',
            y = NULL,
            x = NULL,
            caption = 'Points are statistical outliers.\nVertical lines on boxplots are median values.\nLower/upper hinges correspond to the first and third quartiles.'
        )
)

ggsave(
    funding_thematic_objective,
    filename = 'png/funding_thematic_objective.png',
    width = 11,
    height = 8
)

# Thematic Objective & Approved Project Cost -------------
cost_df <- projects_pross %>%
    select(thematic_objective,
           contains('approved'))


pound_format <- function(x) {
    str_c('£', comma(x))
}

(
    project_cost_thematic_objective <-
        cost_df %>%
        add_count(thematic_objective) %>%
        filter(n >= 10) %>%
        na.omit() %>%
        mutate(
            thematic_objective = fct_reorder(thematic_objective,
                                             approved_project_cost, median)
        ) %>%
        ggplot(aes(
            thematic_objective, approved_project_cost
        )) +
        geom_boxplot(fill = 'skyblue') +
        scale_x_discrete(labels = thematic_objective_label) + #
        scale_y_log10(expand = c(0.025, 0.025),
                      labels = pound_format) +
        coord_flip() +
        leader_theme +
        labs(
            title = 'LEADER Thematic Objective &\nApproved Project Cost',
            subtitle = 'Projects from 1 April 2016',
            caption = 'Points are statistical outliers.\nVertical lines on boxplots are median values.\nLower/upper hinges correspond to the first and third quartiles.',
            y = 'Approved Costs\nNOTE: Log Scale',
            x = NULL
        )
)


ggsave(
    project_cost_thematic_objective,
    filename = 'png/project_cost_thematic_objective.png',
    width = 11,
    height = 8
)

# Project Grant vs Total Cost --------------------------

cost_df <- projects_pross %>%
    select(thematic_objective,
           contains('approved'))

pound_format <- function(x) {
    str_c('£', comma(x))
}



breaks <- c(seq(100000, 500000, 100000), 1000000, 1500000, 2000000)


(
    prop_granted <- cost_df %>%
        mutate(prop = approved_grant / approved_project_cost) %>%
        summarise(proportion = percent(mean(prop, na.rm = T)),
                  sd = sd(prop, na.rm = T)) %>%
        pull(proportion)
)



(
    LEADER_contribution <-
        cost_df %>%
        mutate(prop = approved_grant / approved_project_cost) %>%
        ggplot(
            aes(
                approved_project_cost,
                prop,
                color = thematic_objective,
                group = 1
            )
        ) +
        geom_smooth(
            show.legend = F,
            method = 'loess',
            color = Blues[5],
            fill = Blues[3]
        ) +
        geom_jitter(
            show.legend = F,
            size = 2.5,
            width = .1,
            height = .1
        ) +
        scale_x_continuous(labels = pound_format) +
        scale_y_continuous(labels = percent) +
        scale_color_manual(values = Spectral_n(15)) +
        leader_theme +
        theme(axis.title.y = element_text(angle = 0, vjust = .5)) +
        labs(
            title = 'LEADER\'s Contribution to Project Costs',
            subtitle = ' As project cost increases, LEADER\'s contribution decreases\n Points are coloured by thematic objective',
            x = 'Total Project Cost',
            y = 'Percent Paid\nby LEADER',
            caption = 'Line shows moving average (LOESS curve).'
        ) +
        theme(axis.text = element_text(size = 15)) +
        annotate(
            geom = 'label',
            x = 1.5e+6,
            y = .5,
            label = paste('LEADER pays about',
                          prop_granted,
                          '\nof ALL project costs')
        )
)

ggsave(
    LEADER_contribution,
    filename = 'png/LEADER_contribution.png',
    width = 11,
    height = 8
)



# Project size overtime by rural urban classification -------



cost_time_df <-
    projects_working %>%
    select(contains('approved'),
           estimated_start_date,
           contains('8')) %>%
    mutate(start_year = year(estimated_start_date)) %>%
    select(project.size = 1,
           eight_fold = 4,
           year = 5) %>%
    na.omit()



pound_format <- function(x) {
    str_c('£', comma(x))
}


label_df <- SG_8_fold_rural_urban_labels_df %>%
    select(eight_fold = 1, 
           label = 2)



(project_size_ru_ur_time <- cost_time_df %>%
        group_by(year, eight_fold) %>%
        summarise(total = sum(project.size, na.rm = T)) %>%
        left_join(label_df) %>% 
        mutate(
            label = fct_reorder(label, total, head, n = 1, .desc = T)
        ) %>%
        ggplot(aes(year,
                   total,
                   color = label,
                   group = label)) +
        geom_line(size = 1.5,
                  position = position_dodge(width = .15)) +
        geom_point(size = 5,
                   position = position_dodge(width = .15)) +
        leader_theme +
        scale_y_continuous(labels = pound_format) +
        scale_color_manual(values = Spectral_n(8)) +
        labs(
            title = 'Scottish LEADER: Total Project Size (£\'s) Over Time &\nRural-Urban 8-Fold Classification',
            subtitle = '',
            x = 'Estimated project start year',
            y = 'Total\nPounds',
            color = 'Rural-Urban Classification', 
            caption = 'NOTE: Analysis took place in summer 2019 (year is not complete).\nPoints are dodged to avoid overlap.'
        )+
        theme(axis.title.y = element_text(angle = 0, vjust = .5),
              axis.text = element_text(size = 15))
)


ggsave(
    project_size_ru_ur_time,
    filename = 'png/project_size_ru_ur_time.png',
    width = 13,
    height = 8
)






# LEADER proportion overtime by rural urban classification -------



prop_time_df <-   projects_working %>%
    select(contains('approved'),
           estimated_start_date,
           contains('8')) %>% 
    mutate(start_year = year(estimated_start_date), 
           prop = approved_grant/approved_project_cost) %>%   select(prop, eight_fold = 4, year = start_year) %>% 
    na.omit()



pound_format <- function(x) {
    str_c('£', comma(x))
}


label_df <- SG_8_fold_rural_urban_labels_df %>%
    select(eight_fold = 1, 
           label = 2)

(proportion_leader_ru_ur <- prop_time_df %>%
        group_by(year, eight_fold) %>%
        summarise(ave = mean(prop, na.rm = T)) %>%
        left_join(label_df) %>% 
        mutate(
            label = fct_reorder(label, ave, head, n = 1, .desc = T)
        ) %>%
        ggplot(aes(year,
                   ave,
                   color = label,
                   group = label)) +
        geom_line(size = 1.5,
                  position = position_dodge(width = .15)) +
        geom_point(size = 5,
                   position = position_dodge(width = .15)) +
        leader_theme +
        scale_y_continuous(labels = percent) +
        scale_color_manual(values = Spectral_n(8)) +
        labs(
            title = 'Scottish LEADER: Percent Contributed by LEADER Over Time &\nRural-Urban 8-Fold Classification',
            subtitle = '',
            x = 'Estimated project start year',
            y = 'Percent of total cost\npaid by LEADER',
            color = 'Rural-Urban Classification', 
            caption = 'NOTE: Analysis took place in summer 2019 (year is not complete).\nPoints are dodged to avoid overlap.'
        )+
        theme(axis.title.y = element_text(angle = 0, vjust = .5),
              axis.text = element_text(size = 15))
)


ggsave(
    proportion_leader_ru_ur,
    filename = 'png/proportion_leader_ru_ur.png',
    width = 13,
    height = 8
)


projects_working %>% 
    filter(UR8_2013_2014 == 8) %>% 
    arrange(desc(approved_project_cost)) %>% 
    View()







projects_working %>% 
    count(farm = str_detect(organisation_name, 'Farm'))

projects_working %>%
    select(status, 
           thematic_objective,
           local_action_group,
           approved_project_cost) %>% 
    gather(key, 
           value, 
           -thematic_objective, 
           -local_action_group) %>% 
    count(key)

projects_working %>% 
    count(thematic_objective)



projects_working %>% 
    count(organisation_name) %>% 
    filter(n > 1)




















View(projects_pross)

projects_pross %>% 
    count(state_aid_type, sort = T)

