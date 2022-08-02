##### F1 Constructor Percentile Rankings #####
### By: Stephan Teodosescu ###
## July 2022 ##

library(tidyverse)
library(magick)
library(rvest) # for webscraping
library(polite)
library(gt) #for 538-themed tables
library(glue)
library(ggtext)
library(gganimate)
library(rlang)
library(RCurl)
library(ggimage) #for working with logos
library(gtExtras)
library(zoo)
library(janitor)
library(prismatic)
library(patchwork)
library(ggsci)
library(rsvg)
library(ggalt)
library(plotly)



# Custom ggplot theme (inspired by Owen Phillips at the F5 substack blog)
theme_custom <- function () { 
    theme_minimal(base_size=11, base_family="Titillium Web") %+replace% 
        theme(
            panel.grid.minor = element_blank(),
            plot.background = element_rect(fill = 'floralwhite', color = "floralwhite"),
            panel.background = element_rect(fill = "floralwhite", color = "floralwhite")
        )
}

# Table theme
gt_theme_538 <- function(data,...) {
    data %>%
        opt_table_font(
            font = list(
                google_font("Titillium Web"),
                default_fonts()
            )
        ) %>%
        tab_style(
            style = cell_borders(
                sides = "bottom", color = "transparent", weight = px(2)
            ),
            locations = cells_body(
                columns = TRUE,
                # This is a relatively sneaky way of changing the bottom border
                # Regardless of data size
                rows = nrow(data$`_data`)
            )
        )  %>% 
        tab_options(
            column_labels.background.color = "white",
            table.border.top.width = px(3),
            table.border.top.color = "transparent",
            table.border.bottom.color = "transparent",
            table.border.bottom.width = px(3),
            column_labels.border.top.width = px(3),
            column_labels.border.top.color = "transparent",
            column_labels.border.bottom.width = px(3),
            column_labels.border.bottom.color = "black",
            data_row.padding = px(3),
            source_notes.font.size = 12,
            table.font.size = 16,
            heading.align = "left",
            ...
        ) 
}

# create aspect ration to use throughout
asp_ratio <- 1.618

# Function for plot with logo generation
add_logo <- function(plot_path, logo_path, logo_position, logo_scale = 10){
    
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
        stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
        x_pos = 0.01 * plot_width
        y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
        x_pos = plot_width - logo_width - 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
        x_pos = 0.01 * plot_width
        y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
}


# upload csv from directory

constructor_percentiles <- read_csv("constructor_percentiles.csv")

# load data from ergast API

driver_standings <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/driver_standings.csv")
constructors <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/constructors.csv")
drivers <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/drivers.csv")
qualifying <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/qualifying.csv")
races <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/races.csv")
results <- readr::read_csv("/Users/Stephan/Desktop/R Projects/F1/f1db_csv/results.csv")


##### data processing #####

# create 2022 races dataframe
races_2022 <- races %>% 
    filter(year == 2022)

# join in results
races_2022 <- left_join(races_2022, results, by = "raceId")

#join in driver info
races_2022 <- left_join(races_2022, drivers, by = "driverId")

#join in constructor info
races_2022 <- left_join(races_2022, constructors, by = "constructorId")

#create percentile rankings
race_percentiles <- races_2022 %>%
    group_by(constructorRef) %>% 
    summarise(avg_finish = median(positionOrder)) %>% 
    mutate(percentile = 1-(avg_finish/20)) %>% 
    arrange(avg_finish) %>% 
    mutate(rank = row_number()) %>% 
    relocate(rank) %>% 
    drop_na()



## make qualifying results dataframe
qualifying_2022 <- qualifying %>% 
    left_join(constructors, by = "constructorId") %>% 
    left_join(races, by = "raceId") %>% 
    filter(year == 2022)

qualifying_percentiles <- qualifying_2022 %>% 
    group_by(constructorRef) %>% 
    summarise(avg_qual_finish = median(position)) %>% 
    mutate(qual_percentile = 1-(avg_qual_finish/20)) %>% 
    arrange(avg_qual_finish)

# join race and qualifying df
percentiles_df <- race_percentiles %>% 
    left_join(qualifying_percentiles, by = "constructorRef") %>% 
    mutate(delta_finish = avg_qual_finish - avg_finish,
           delta_percentile = qual_percentile - percentile) %>% 
    mutate(across(c(avg_finish, avg_qual_finish, delta_finish), ~ round(., 1))) %>%  # round numeric columns to two decimals 
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/F1/master/manufacturers/', constructorRef, '.png'))

# join in constructor names
percentiles_df <- percentiles_df %>%
    inner_join(constructors, by = "constructorRef") %>% 
    rename(constructor = name)



##### Data Visualization #####

# get latest race date
driver_results_2022 <- driver_standings %>% 
    left_join(races, by = "raceId") %>% 
    rename(driver_url = url) %>% 
    left_join(drivers, by = "driverId") %>% 
    filter(year == 2022)

latest_race <- max(driver_results_2022$date)


## Make dumbbell plots

# 1. Basic dumbbell plot
percentiles_df %>% 
    mutate(constructor = fct_reorder(constructor, -avg_finish)) %>%
    ggplot() +
    geom_segment(aes(x = avg_qual_finish, xend = avg_finish, y = constructor, yend = constructor), color = "#b2b2b2", size = 2) +
    geom_dumbbell(aes(x = avg_qual_finish, xend = avg_finish, y = constructor), 
                  color = NA, size_x = 5, size_xend = 5, colour_x = "#2E86C1", colour_xend = "#CB4335") +
    labs(title = "**Ferrari is underperforming its grid positioning**", 
         subtitle = glue("Difference between each F1 team's median <span style = 'color:#2E86C1'>**grid starting position**</span> and <span style = 'color:#CB4335'>**race finish**</span>. Data as of **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu", 
         x = "Average Position",
         y = "") +
    geom_text(data=percentiles_df, aes(x=avg_finish, y=constructor, label=avg_finish),
              color="#CB4335", size=2.75, vjust=2.5, family="Titillium Web") +
    geom_text(data=percentiles_df, aes(x=avg_qual_finish, y=constructor, label=avg_qual_finish),
              color="#2E86C1", size=2.75, vjust=2.5, family="Titillium Web") +
    scale_x_reverse() +
    theme_custom() +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold',
                                    size = 20,
                                    hjust = 0.5),
          plot.subtitle = element_text(
              margin = margin(t = 3, b = 2, unit = "mm"),
              hjust = 0.5
          )) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

ggsave("Construrctors Dumbbell Plot.png")

# Add logo to plot
basic_dumbbell_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/F1/Constructors Dumbbell Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/F1/F1_logo5.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 10
)

# save the image and write to working directory
magick::image_write(basic_dumbbell_with_logo, "Constructors Dumbbell Plot with Logo.png")



## Create boxplots to show distribution of finishes

# 2a. Race Finishes Boxplot
races_2022 %>% 
    drop_na() %>% 
    mutate(constructorRef = fct_reorder(constructorRef, positionOrder)) %>%
    ggplot(aes(constructorRef, positionOrder, group = constructorRef)) + 
    geom_boxplot(color = "#CB4335", fill="#CB4335", alpha=0.2) +
    #scale_x_continuous(breaks = 1:10) +
    theme_custom() +
    labs(x = "", y = "Race Finish Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#CB4335'>**race finishes**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())


# 2b. Race Finishes Boxplot with logos as x-axis

# Define function to link to images
link_to_img <- function(x, width = 30) {
    glue::glue("<img src='{x}' width='{width}'/>")
}

races_2022 %>% 
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/F1/master/manufacturers/', constructorRef, '.png')) %>%
    mutate(logos = link_to_img(constructor_logo)) %>% 
    drop_na() %>% 
    #mutate(constructorRef = fct_reorder(constructorRef, positionOrder)) %>%
    ggplot(aes(x = reorder(logos, constructorRef), y = positionOrder, group = constructorRef)) + 
    geom_boxplot(color = "#CB4335", fill="#CB4335", alpha=0.2) +
    theme_custom() +
    labs(x = "", y = "Race Finish Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#CB4335'>**race finishes**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(axis.text.x = element_markdown(margin = margin(r = -25, unit = "pt"))) +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())
    


# 2c. Race Finishes boxplot ggplotly
races_boxplot <- races_2022 %>% 
    drop_na() %>% 
    mutate(constructorRef = fct_reorder(constructorRef, positionOrder)) %>%
    ggplot(aes(constructorRef, positionOrder, group = constructorRef)) + 
    geom_boxplot(color = "#CB4335", fill="#CB4335", alpha=0.2) +
    #scale_x_continuous(breaks = 1:10) +
    theme_custom() +
    labs(x = "", y = "Race Finish Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of race finishes for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

plotly::ggplotly(races_boxplot)



# 3a. Qualifying Finishes Boxplot
qualifying_2022 %>% 
    drop_na() %>% 
    mutate(constructorRef = fct_reorder(constructorRef, position)) %>%
    ggplot(aes(constructorRef, position, group = constructorRef)) + 
    geom_boxplot(color = "#2E86C1", fill="#2E86C1", alpha=0.2) +
    #scale_x_continuous(breaks = 1:10) +
    theme_custom() +
    labs(x = "", y = "Grid Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#2E86C1'>**grid starting position**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())


# 3b. Qualifying Finishes Boxplot with constructor logos as x-axis
qualifying_2022 %>% 
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/F1/master/manufacturers/', constructorRef, '.png')) %>%
    mutate(logos = link_to_img(constructor_logo)) %>% 
    drop_na() %>% 
    mutate(constructorRef = fct_reorder(constructorRef, position)) %>%
    ggplot(aes(logos, position, group = constructorRef)) + 
    geom_boxplot(color = "#2E86C1", fill="#2E86C1", alpha=0.2) +
    #scale_x_continuous(breaks = 1:10) +
    theme_custom() +
    labs(x = "", y = "Grid Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#2E86C1'>**grid starting position**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(axis.text.x = element_markdown(margin = margin(r = -25, unit = "pt"))) +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

# 3c. Race Finishes boxplot ggplotly

qualifying_boxplot <- qualifying_2022 %>% 
    drop_na() %>% 
    mutate(constructorRef = fct_reorder(constructorRef, position)) %>%
    ggplot(aes(constructorRef, position, group = constructorRef)) + 
    geom_boxplot(color = "#2E86C1", fill="#2E86C1", alpha=0.2) +
    #scale_x_continuous(breaks = 1:10) +
    theme_custom() +
    labs(x = "", y = "Grid Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#2E86C1'>**grid starting position**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

plotly::ggplotly(qualifying_boxplot)


# 4. Combined Race and Qualifying box plots

# create unique identifier column ("key") in existing races and qualifying dfs for join condition
races_df <- races_2022 %>% 
    mutate(key = str_c(driverId, raceId))

qualifying_df <- qualifying_2022 %>% 
    mutate(key = str_c(driverId, raceId))

# join and clean up resulting df
combined_2022 <- races_df %>% 
    inner_join(qualifying_df, by = "key") %>% 
    rename(position_qualifying = position.y,
            position_race = positionOrder) %>% 
    relocate(position_race, position_qualifying)

# clean and pivot longer for boxplot

combined_2022 <- combined_2022 %>% 
    select(position_race, position_qualifying, driverRef, constructorRef.x) %>% 
    rename(constructorRef = constructorRef.x) 

combined_long <- combined_2022 %>% 
    pivot_longer(position_race:position_qualifying,
                 names_to = "type",
                 names_prefix = "position_",
                 values_to = "position")

# create faceted box plots
df_colors <- c("race" , "qualifying")

p1 <- combined_long %>% 
    mutate(type_color = case_when(type == "race" ~ "#CB4335",
                             type == "qualifying" ~ "#2E86C1"
                             )) %>% 
    ggplot(aes(x = constructorRef, y = position, fill = type, alpha = 0.2)) + 
    geom_boxplot() +
    facet_wrap(~constructorRef, scale="free") +
    geom_boxplot() +
    theme_custom() +
    scale_fill_manual(values=c("#CB4335", "#2E86C1")) +
    scale_y_reverse() +
    labs(x = "", y = "Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#CB4335'>**race finishes**</span> and <span style = 'color:#2E86C1'>**grid starting positions**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(legend.position = "none") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())



# add logos to each facet (not working)

combined_long <- combined_long %>% 
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/F1/master/manufacturers/', constructorRef, '.png'))

## Reference: https://github.com/tonyelhabr/sports_viz/blob/master/42-202122_new_players/01-main.R
p_bld <- ggplot_gtable(ggplot_build(p1))
grob_strip_index <- which(sapply(p_bld$grob, function(x) x$name) == "strip")
facet_id <- sapply(grob_strip_index, function(grb) {
    p_bld$grobs[[grb]]$grobs[[1]]$children[[2]]$children[[1]]$label
})
# p_bld$layout$z[grob_strip_index] <- 0 ## not sure what the point of this is...

for (i in 1:length(facet_id)) {
    id <- facet_id[i]
    url <-
        combined_long %>%
        filter(constructorRef == !!id) %>%
        pull(constructor_logo)
    lab <-
        grid::textGrob(
            id,
            x = unit(0, "npc"),
            gp = grid::gpar(
                col = "black",
                fontfamily = "Titillium Web",
                fontface = "bold",
                fontsize = 8
            ),
            hjust = 0
        )
    img <-
        grid::rasterGrob(
            image = magick::image_read(url),
            # just = 'right',
            hjust = 1,
            x = unit(1, "npc"),
            ## 1 and 0.75 is also fine
            vp = grid::viewport(height = 1, width = 0.75)
        )
    tot_tree <- grid::grobTree(lab, img)
    p_bld$grobs[[grob_strip_index[i]]] <- tot_tree
}

p1 <- cowplot::ggdraw(p_bld)


# 5. Grouped boxplot
combined_long %>% 
    mutate('constructor_logo' = paste0('https://raw.githubusercontent.com/steodose/F1/master/manufacturers/', constructorRef, '.png')) %>%
    mutate(logos = link_to_img(constructor_logo)) %>% 
    arrange(desc(position)) %>% 
    mutate(constructorRef = fct_reorder(constructorRef, position)) %>%
    ggplot(aes(x = logos, y = position, fill = type, alpha = 0.2)) + 
    geom_boxplot() +
    scale_fill_manual(values=c("#2E86C1", "#CB4335")) +
    theme_custom() +
    theme(plot.title.position = 'plot', 
          plot.title = element_text(face = 'bold',
                                    size = 20,
                                    hjust = 0.5),
          plot.subtitle = element_text(
              margin = margin(t = 3, b = 2, unit = "mm"),
              hjust = 0.5
          )) +
    scale_y_reverse() +
    labs(x = "", y = "Position",
         title = "Ferrari's Strategy: Go Big or Go Home",
         subtitle = glue("Distribution of <span style = 'color:#2E86C1'>**grid starting positions**</span> and <span style = 'color:#CB4335'>**race finishes**</span> for each F1 team in 2022. Data thru **{latest_race}** races."),
         caption = "Data: Ergast API\nGraphic: @steodosescu") +
    theme(axis.text.x = element_markdown(margin = margin(r = -25, unit = "pt"))) +
    theme(legend.position = "none") +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    theme(plot.title = element_markdown()) +
    theme(plot.subtitle = element_markdown())

ggsave("Constructors Boxplot Plot.png")

# Add F1 logo to plot
boxplot_with_logo <- add_logo(
    plot_path = "/Users/Stephan/Desktop/R Projects/F1/Constructors Boxplot Plot.png", # url or local file for the plot
    logo_path = "/Users/Stephan/Desktop/R Projects/F1/F1_logo5.png", # url or local file for the logo
    logo_position = "top left", # choose a corner
    # 'top left', 'top right', 'bottom left' or 'bottom right'
    logo_scale = 10
)

# save the image and write to working directory
magick::image_write(boxplot_with_logo, "Constructors Boxplot Plot with Logo.png")


## 5. Race Results Table

# pivot wider and clean up df
races_table_wider <- races_2022 %>% 
    select(round, name.x, date, driverId, constructorId, positionOrder, driverRef, forename, surname, name.y) %>%
    rename(race = name.x, constructor = name.y) %>%
    mutate(driver = str_c(forename, surname, sep = " ")) %>% 
    select(-c(round, date, constructorId, driverId, driverRef:constructor)) %>% 
    pivot_wider(names_from = race,
                values_from = driver) 

# create gt table
race_results_table <- races_table_wider %>%
    gt() %>% 
    cols_label(
        positionOrder = ""
    ) %>% 
    data_color(
        columns = everything(),
        colors = scales::col_factor(
            palette = c("#FF9B00", "#FF9B00", "#FF2800", "#FF2800", "#00D2BE", "#00D2BE", "#f98e1d", "#005BA9", "#981E32", "#005BA9", "#F62039", "#f98e1d", "#00293F", "#00352F","#F62039", "#00293F", "#981E32", "#00352F", "#00A3E0", "#00A3E0", "#00352F"), 
            domain = c('Max Verstappen', 'Sergio Pérez', 'Charles Leclerc', 'Carlos Sainz', 'Lewis Hamilton', 'George Russell', 'Lando Norris', 'Esteban Ocon', 'Valtteri Bottas', 'Fernando Alonso', 'Kevin Magnussen', 'Daniel Ricciardo', 'Pierre Gasly', 'Sebastian Vettel', 'Mick Schumacher', 'Yuki Tsunoda', 'Guanyu Zhou', 'Lance Stroll', 'Alexander Albon', 'Nicholas Latifi', 'Nico Hülkenberg'),
            na.color = "white",
            ordered = TRUE
        )
    ) %>% 
    fmt_missing(
        columns = everything(),
        rows = everything(),
        missing_text = "---"
    ) %>% 
    gt_theme_538() %>%
    tab_header(title = md("**2022 Forumula 1 World Championship**"),
               subtitle =glue("Race results as of {latest_race}.")) %>%
    tab_source_note(
        source_note = md("Data: Ergast API<br>Table: @steodosescu"))

race_results_table %>%
    gtsave("Race Results Table.png")
