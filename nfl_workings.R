library(tidyverse)

# Reading in data
file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game)

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays)

# Re-creating python code from:
# https://nbviewer.jupyter.org/github/mike-curry00/nfl-big-data-bowl/blob/master/Play%20Animation.ipynb?flush_cache=true
tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum)
tracking.example.merged$team[tracking.example.merged$team == "away"] <- tracking.example.merged$visitorTeamAbbr
tracking.example.merged$team[tracking.example.merged$team == "home"] <- tracking.example.merged$homeTeamAbbr

file.players <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players.df <- read_csv(file.players)

# pull out receivers/defenders
target <- c('RB','WR','TE')
receivers.df <- players.df %>% filter(PositionAbbr %in% target)
receiver.ids <- receivers.df$nflId
receivers <- tracking.example.merged %>% filter(nflId %in% receiver.ids & possessionTeam == team)
defenders <- tracking.example.merged %>% filter(!nflId %in% receiver.ids & possessionTeam != team) %>% filter(possessionTeam != "ball")

# calc min distance from each receiver from list of defender coordinates

defenders %>% group_by(playId, frame.id)



# need to tag frame where QB started scramble
# for frames between scramble & ball release, how much distance did receivers gain?
