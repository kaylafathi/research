# Title: Lab06
# Description: This lab covers topics like more complex file structure, exporting R output, exporting plot images, and working with dplyr pipelines. We are meant to get to know the pipe operator %>% and practice chain dplyr operations with the pipes
# Inputs: nba2018-players.csv
# Outpots: Plot images, Script file, and Tables
# Author: Kayla Fathi
# Date: 10-04-2018

# mkdir lab06
# cd lab06
# touch README.md
# mkdir data
# mkdir code
# mkdir output
# mkdir images
# ls
# cd data
# curl -O https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2018/master/data/nba-players.csv
# ls 
# wc nba2018-players.csv
# head nba2018-players.csv
# tail nba2018-players.csv

#packages
library(readr)
library(dplyr)
library(ggplot2)
players <- read_csv('../data/nba2018-players.csv', col_names = TRUE)
warriors <- filter(players, team == 'GSW')
warriors <- arrange(warriors, salary)
warriors
# directions say to put it folder directory but I put it in output as I think that was a misspelling 
write.csv(warriors, file = '../data/warriors.csv', row.names = FALSE)

lakers <- filter(players, team == "LAL")
lakers <- arrange(lakers, desc(experience))
lakers
write.csv(lakers, file = '../data/lakers.csv', row.names = FALSE)

# Exporting some R output

sink('../output/data-structure.txt')
str(players)
sink()

sink('../output/summary-warriors.txt')
summary(warriors)
sink()

sink('../output/summary-lakers.txt')
summary(lakers)
sink()

# Exporting some "base" graphs

png(filename = "../images/scatterplot-height-weight.png")
plot(players$height, players$weight, pch = 20, xlab = "Height", ylab = "Weight")
dev.off()

png(filename = "../images/scatterplot-height-weight-highres.png", res = 100)
plot(players$height, players$weight, pch = 20, xlab = "Height", ylab = "Weight")
dev.off()

jpeg(filename = "../images/histogram-age.jpeg", width = 600, height = 400)
hist(players$age, xlab = 'Age')
dev.off()

pdf(file = "../images/histogram-age.pdf", width = 7, height = 5)
hist(players$age, xlab = 'Age')
dev.off()

# Exporting some ggplots
gg_pts_salary <- ggplot(data = players) + geom_point(aes(x = points, y = salary))
ggsave(filename = "../images/points_salary.pdf", width = 7, height = 5)

gg_ht_wt_position <- ggplot(data = players, aes(x = height, y = weight)) +
  geom_point() +
  facet_wrap(~ position)
ggsave(filename = "../images/height_weight_by_position.pdf", width = 6, height = 4)

# More 'dplyr'

players %>%
  filter(team == 'LAL')%>%
  select(player)

players %>% 
  filter(team == 'GSW' & position == 'PG')%>%
  select(player, salary)

players %>%
  filter(experience > 10 & salary <= 10000000)%>%
  select(player, age, team)

players %>%
  filter(experience == 0 & age == 20)%>%
  select(player, team, height, weight)%>%
  slice(1:5)

gsw_mpg <- data.frame(
      players %>%
        mutate(min_per_game = minutes/games)%>%
                        filter(team == 'GSW')%>%
                        select(player, experience, min_per_game)%>%
                        arrange(desc(min_per_game)))

gsw_mpg

players%>%
  group_by(team)%>%
  summarize(avg_points3 = mean(points3))%>%
  arrange(avg_points3)%>%
  slice(1:5)

players%>%
  filter((position == 'PF') & (experience >= 5 & experience <= 10))%>%
  summarize(mean(age))

players%>%
  filter((position == 'PF') & (experience >= 5 & experience <= 10))%>%
  summarize(sd(age))
