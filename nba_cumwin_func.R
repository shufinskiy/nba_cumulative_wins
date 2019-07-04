library(data.table)
library(tidyverse)
library(gganimate)
library(viridis)
standings <- fread("./data/standings.csv")

nba_cumulative_wins <- function(table, elements, first_season, last_season){

    table1 <- table[TeamCity == "New Orleans" & TeamName == "Hornets", TeamName := "Pelicans"][
    TeamCity == "New Orleans/Oklahoma City" & TeamName == "Hornets", TeamName := "Pelicans"][
      TeamName == "Bobcats", TeamName := "Hornets"][
        TeamName == "SuperSonics", TeamName := "Thunder"]

  select_teams <- unique(table1$TeamName)
  select_div <- unique(table1$Division)
  select_conf <- unique(table1$Conference)
  select_nba <- "NBA"
  
  table1 <- if(elements %in% select_teams){
    table1[TeamName %in% elements]
  } else if (elements %in% select_div){
    table1[Division %in% elements]
  } else if(elements %in% select_conf){
    table1[Conference %in% elements]
  } else if(elements == "NBA"){
    table1
  } else {
    NULL
  }
  
  table1 <- table1[SeasonID >= as.numeric(paste(2, first_season, sep = "")) & SeasonID <= as.numeric(paste(2, last_season, sep = ""))][
    ,.(TeamName, WINS)][
      , CumWins := cumsum(WINS), by = "TeamName"][
        ,year := rep(seq(first_season, last_season), each = length(unique(table1$TeamName)))][
          , cumrank := frank(-CumWins, ties.method = "random"), by = "year"][
            , value_rel := CumWins/CumWins[cumrank==1], by = "year"]
  
  table_color <- data.table(TeamName = c("Hawks", "Celtics", "Nets", "Hornets", 
                                         "Bulls", "Cavaliers", "Mavericks", 
                                         "Nuggets", "Pistons", "Warriors", "Rockets", 
                                         "Pacers", "Clippers", "Lakers", "Grizzlies",
                                         "Heat", "Bucks", "Timberwolves", "Pelicans", 
                                         "Knicks", "Thunder", "Magic", "76ers", "Suns", 
                                         "Trail Blazers","Kings", "Spurs", "Raptors", 
                                         "Jazz", "Wizards"),
                            TEAM_color = c("#E03A3E", "#007A33", "#000000", "#1D1160", "#CE1141", "#6F263D",
                                           "#00538C", "#0E2240", "#C8102E", "#006BB6", "#CE1141", "#002D62",
                                           "#C8102E", "#552583", "#5D76A9", "#98002E", "#00471B", "#0C2340",
                                           "#0C2340", "#006BB6", "#007AC1", "#0077C0", "#006BB6", "#1D1160",
                                           "#E03A3E", "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C"))
  
  elements1 <- if (elements == "NBA"){
    c("Hawks", "Celtics", "Nets", "Hornets", 
      "Bulls", "Cavaliers", "Mavericks", 
      "Nuggets", "Pistons", "Warriors", "Rockets", 
      "Pacers", "Clippers", "Lakers", "Grizzlies",
      "Heat", "Bucks", "Timberwolves", "Pelicans", 
      "Knicks", "Thunder", "Magic", "76ers", "Suns", 
      "Trail Blazers","Kings", "Spurs", "Raptors", 
      "Jazz", "Wizards")
  } else if (elements == "West") {
    c("Mavericks","Nuggets", "Warriors", "Rockets", 
      "Clippers", "Lakers", "Grizzlies","Timberwolves", 
      "Pelicans", "Thunder", "Suns", "Trail Blazers","Kings", "Spurs", 
      "Jazz")
  } else if (elements == "East") {
    c("Hawks", "Celtics", "Nets", "Hornets", 
      "Bulls", "Cavaliers","Pistons", "Pacers",
      "Heat", "Bucks", "Knicks", "Magic", "76ers",
      "Raptors", "Wizards")
  } else if (elements == "Pacific") {
    c("Warriors", "Clippers", "Lakers", "Suns", "Kings")
  } else if (elements == "Southeast") {
    c("Magic", "Hornets", "Heat", "Hawks", "Wizards")
  } else if (elements == "Southwest") {
    c("Mavericks", "Grizzlies", "Pelicans", "Rockets", "Spurs")
  } else if (elements == "Central") {
    c("Bucks", "Pacers", "Pistons", "Bulls", "Cavaliers")
  } else if (elements == "Atlantic") {
    c("Knicks", "Nets", "Celtics", "Raptors", "76ers")
  } else if (elements == "Northwest") {
    c("Nuggets", "Trail Blazers", "Jazz", "Thunder", "Suns")
  } else {
    elements
  }
  
  table_elements1 <- data.table(TeamName = elements1)
  
  table_color <- table_color[order(TeamName)]
  inner_table_color <- inner_join(table_color, table_elements1)
  
  cols <- inner_table_color[, "TEAM_color"]
  
  gg <- ggplot(table1, aes(cumrank, group = TeamName, fill = as.factor(TeamName),
                           color = as.factor(TeamName))) + 
    geom_tile(aes(y = CumWins/2,
                  height = CumWins,
                  width = 0.7), color = NA, alpha = 0.8) +
    geom_text(aes(y = 0, label = paste(TeamName, " ")), vjust = 0.2, hjust = 1, size = 6) +
    geom_text(aes(y = CumWins, label = paste0(" ",round(CumWins))), hjust = 0, size = 7) +
    coord_flip(clip = "off", expand = FALSE) +
    scale_fill_manual(values = cols) +
    scale_color_manual(values = cols) +
    scale_y_continuous(labels = scales::comma) +
    scale_x_reverse() +
    guides(color = FALSE, fill = FALSE) +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.grid.major.x = element_line( size=.1, color="grey" ),
          panel.grid.minor.x = element_line( size=.1, color="grey" ),
          plot.title=element_text(size=25, hjust=0.5, face="bold", colour="black", vjust=-1),
          plot.subtitle = element_text(size = 15),
          plot.caption =element_text(size=15, hjust=0.5, color="black"),
          plot.background=element_blank(),
          plot.margin = margin(2,2, 2, 4, "cm"))
  
  anim <- gg + transition_states(year, transition_length = 4, state_length = 1) +
    view_follow(fixed_x = TRUE)  +
    labs(title = paste("Cumulative Wins by teams in seasons", 
                       first_season, "-", last_season, sep = " "),
         subtitle = paste(if (elements %in% select_div ){
           paste(elements, "Division", sep = " ")
         } else if (elements %in% select_conf ){
           paste("Conference", elements, sep = " ")
         }, "Season: {closest_state}", sep = " "),
         caption  = "Telegram: @NBAatlantic, Twitter: @vshufiskiy\nData sourse: stats.nba.com")
  
  animate(anim, nframes = (last_season - first_season + 1) * (length(unique(table1$TeamName)) + 20),
          fps = 20,  width = 1200, height = 1000, 
          renderer = gifski_renderer(paste(elements[1], "cumwins.gif", sep = "_")))
}

nba_cumulative_wins(standings, "NBA", 2004, 2018)
