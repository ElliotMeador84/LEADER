library(tidyverse)


op_is_window <- Sys.info()[1] == "Windows"
if(op_is_window == T){
    source('C:/Users/emeador/Downloads/all_functions.R')
} else {
    source('~/Downloads/all_functions.R')
}

# Category colors -----------
lags <- unique(projects_working$local_action_group)

lag_c <- Pastel2_n(length(lags))

names(lag_c) <- lags


# Themes ---------------------
leader_theme <- theme_minimal() +
    theme(
        axis.title = element_text(size = 12.5),
        axis.text = element_text(color = Greens[9],
                                 size = 7.5),
        plot.title = element_text(size = 17.5, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 4),
        plot.margin = margin(2, 2, 2, 2, 'cm'), 
        plot.caption = element_text(size = 10)
    )


leader_map <- function(x){
    x +
        coord_equal()+
        scale_x_continuous(labels = scientific_format(digits = 1))+
        scale_y_continuous(labels = scientific_format(digits = 1))+
        xlab('Longitude')+
        ylab('Latitude')+
        theme_bw()+
        theme(panel.grid = element_blank())
}




leader_dash_theme <- theme_minimal() +
    theme(
        axis.title = element_text(size = 15),
        axis.text = element_text(color = Greens[8],
                                 size = 15),
        plot.title = element_text(size = 20, hjust = 0),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linetype = 4),
        plot.margin = margin(2, 2, 2, 2, 'cm'),
        legend.position = 'none'
    )



# Project functions --------------------




thematic_objective_label <- function(x) {
    x <- str_squish(x)
    x <- str_replace_all(x, '\\band\\b', '&')
    x <- str_replace_all(x, '\\blags\\b', 'LAGs')
    x <- str_replace_all(x, '\\beu\\b', 'EU')
    x <- str_replace_all(x, '\\buk\\b', 'UK')
    x <- str_remove_all(x, "\\([^()]*\\)")
    x <- str_to_sentence(x)
    x <- str_wrap(x, 70)
    return(x)
}











