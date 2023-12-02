#Advent of Code 2023
#Day2
#Puzzle 1

library(tidyverse)
library(readxl)


daten<-read_excel("./R/AoC_2.xlsx", col_names=FALSE)

daten<-daten%>%
rename(A = 1)

#Each game ID has sets of cube combinations separated by ;
#Each set has a randomly drawn number of green, red and blue cubes (finite count)

#Which game IDs are impossible to draw if there are only
#12 red, 13 green, and 14 blue cubes

#first I'll separate these to make the table easier to handle

test<-daten%>%
separate(A, c('Game', 'Cubes'), sep= ":") %>%
separate(Cubes, c('Set_1', 'Set_2', 'Set_3', 'Set_4',
 'Set_5', 'Set_6'), sep= ";") %>%
separate(Set_1, c('Set A', 'Set B', 'Set C'), sep= ",") %>%
separate(Set_2, c('Set D', 'Set F', 'Set G'), sep= ",") %>%
separate(Set_3, c('Set H', 'Set I', 'Set J'), sep= ",") %>%
separate(Set_4, c('Set K', 'Set L', 'Set M'), sep= ",") %>%
separate(Set_5, c('Set N', 'Set O', 'Set P'), sep= ",") %>%
separate(Set_6, c('Set Q', 'Set R', 'Set S'), sep= ",")


#pivot table
pivot<-test%>%
pivot_longer(!Game, names_to = "Sets", values_to = "Cubes")

colour<-pivot %>%
separate(Cubes, c('Empty', 'Count', 'Colour'), sep= " ") %>%
separate(Game, c('Emptyy', 'Game'), sep= " ") %>%
select(Game, Count, Colour) %>%
drop_na()


colour$Game <- as.numeric(colour$Game)
colour$Count <- as.numeric(colour$Count)

impossible_r<-colour%>%
filter(Colour == "red" & Count > 12) %>%
select(Game)

impossible_g<-colour%>%
filter(Colour == "green" & Count > 13) %>%
select(Game)

impossible_b<-colour%>%
filter(Colour == "blue" & Count > 14) %>%
select(Game)

impossible_all<-bind_rows(impossible_r, impossible_g, impossible_b)

impossible<-distinct(impossible_all, Game)

#the impossible value now need to be removed from the colour dataset

possible<-colour[!(colour$Game %in% impossible$Game),]

possible_games<-distinct(possible, Game)

sum(possible_games$Game)

#=2237


#Puzzle 2

#what is the lowest possible number of each cube in each set to make it possible,
#i.e. the lowest red, the lowest green, and the lowest blue in a game
#multiply these 3 lowerst values together for each game
#then add these up


#I'll start this from the colour dataframe since it's already neat

red<-colour%>%
filter(Colour == "red") %>%
group_by(Game) %>%
slice_max(n = 1, Count) %>%
select(Game, Count)

green<-colour%>%
filter(Colour == "green") %>%
group_by(Game) %>%
slice_max(n = 1, Count) %>%
select(Game, Count)

blue<-colour%>%
filter(Colour == "blue") %>%
group_by(Game) %>%
slice_max(n = 1, Count) %>%
select(Game, Count)

red_unique<-distinct(red, Game, Count)
blue_unique<-distinct(blue, Game, Count)
green_unique<-distinct(green, Game, Count)

all<-red_unique%>%
left_join(blue_unique, by='Game') %>%
left_join(green_unique, by='Game')


multiply<-all%>%
mutate(power = Count.x * Count.y * Count)

sum(multiply$power)

#=66681

