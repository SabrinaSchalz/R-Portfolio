#Squirrel Tidy Tuesday

library(tidyverse)
library(RColorBrewer)
library(scales)
library(gt)

daten <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-05-23/squirrel_data.csv')

#count individual squirrels
IDs<-daten %>%
rename(Squirrel_ID = 3) %>%
count(Squirrel_ID, sort = TRUE)
#highest count per ID is 2, not useful

daten<-daten %>%
rename(Runs = 29) 

daten<-daten %>%
rename(Tail_Flags = 25) %>%
rename(Tail_Twitches = 26) 


#pivot so that all behaviours go into one column
behaviour<-daten%>%
filter(Location == "Ground Plane") %>%
pivot_longer(
cols = Running:Foraging, names_to = "Behaviour", values_to = "B_Value")

#Approaching
B_Count<-behaviour %>%
count(Behaviour, B_Value, Approaches, sort = TRUE)

B<-B_Count %>%
filter(Approaches == "TRUE")

foraging<-B_Count %>%
filter(Behaviour == "Foraging") %>%
add_row(Behaviour = "Total", summarise(., across(where(is.numeric), sum))) %>%
mutate(Percentage = percent(n/2116))

foraging<-foraging%>%
rename(Foraging = B_Value) %>%
rename(Count = n) %>%
select(!Behaviour)


foraging %>%
gt() %>%
  tab_header(
    title = md("Foraging and Approaching the Observer"))

approach<-ggplot(data=B, aes(x=Behaviour, y=n, fill=B_Value)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("Observed Behaviour When Approaching") +
labs(fill = "Behaviour") + xlab("") + ylab("Count of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1)
approach

#Running from
#Approaching
R_Count<-behaviour %>%
count(Behaviour, B_Value, Runs, sort = TRUE)

R<-R_Count %>%
filter(Runs == "TRUE")

foraging<-B_Count %>%
filter(Behaviour == "Foraging") %>%
add_row(Behaviour = "Total", summarise(., across(where(is.numeric), sum))) %>%
mutate(Percentage = percent(n/2116))

foraging<-foraging%>%
rename(Foraging = B_Value) %>%
rename(Count = n) %>%
select(!Behaviour)


foraging %>%
gt() %>%
  tab_header(
    title = md("Foraging and Approaching the Observer"))

run<-ggplot(data=R, aes(x=Behaviour, y=n, fill=B_Value)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("Behaviour When Running From Observer") +
labs(fill = "Behaviour") + xlab("") + ylab("Count of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1)
run


#Ground behaviours in general
beh<-behaviour %>%
count(Behaviour, B_Value, sort = TRUE) %>%
filter(B_Value == "TRUE")

behav<-ggplot(data=beh, aes(x=Behaviour, y=n, fill=B_Value)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("Observed Behaviours on Ground") +
labs(fill = "Behaviour") + xlab("") + ylab("Count of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1)
behav


#Foraging specifically
foraging<-daten%>%
filter(Foraging == "TRUE") %>%
filter(Location == "Ground Plane")%>%
select(Foraging, Approaches, Indifferent, Runs)

#pivot so that all behaviours go into one column
forage<-foraging%>%
pivot_longer(
cols = Approaches:Runs, names_to = "Human", values_to = "H_Value")

f_count<-forage%>%
count(Human, H_Value)

forage<-ggplot(data=f_count, aes(x=Human, y=n, fill=H_Value)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("Response to Human When Foraging") +
labs(fill = "Present") + xlab("") + ylab("Count of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1)
forage


#Sounds and Signals
coms<-daten%>%
pivot_longer(
cols = Kuks:Tail_Twitches, names_to = "Communication", values_to = "C_Value")


coms_count<-coms%>%
count(Communication, C_Value, Approaches, Indifferent, Runs, sort = TRUE) %>%
filter(C_Value == "TRUE")

coms_ind<-ggplot(data=coms_count, aes(x=Communication, y=n, fill=Indifferent)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("") +
labs(fill = "Indifferent") + xlab("") + ylab("Count of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1) + theme(axis.text=element_text(size=8))
coms_ind

all<-daten%>%
filter(Location == "Ground Plane") %>%
select(Foraging, Kuks:Runs)

all<-all%>%
pivot_longer(
cols = Approaches:Runs, names_to = "Human", values_to = "Hu_Value")


all<-all%>%
pivot_longer(
cols = Foraging:Tail_Twitches, names_to = "Behaviour", values_to = "Be_Value")

all_count<-all%>%
count(Behaviour, Human, Hu_Value, Be_Value, sort = TRUE) %>%
filter(Be_Value == "TRUE") %>%
filter(Hu_Value == "TRUE")

all_count <- all_count %>%
add_row(Behaviour = "Total", summarise(., across(where(is.numeric), sum)))
all_count <- all_count %>%
mutate(Percentage = (n/1489))

all_count_graph<-all_count %>%
filter(Behaviour != "Total")

all_table<-all_count %>%
select(Behaviour, Human, n, Percentage) %>%
rename(Count = n) %>%
gt() %>%
  tab_header(
    title = md("Share of Co-Occuring Behaviours and Responses To Human"))

gtsave(all_table, filename = "table.png", , expand = 10)

all_g<-ggplot(data=all_count_graph, aes(x=Human, y=Percentage, fill=Behaviour)) +
geom_bar(stat="identity", position=position_dodge()) + ggtitle("Behaviour + Response To Human") +
labs(fill = "Behaviour") + xlab("") + ylab("Share of Observations") +
scale_fill_brewer(palette = "Set2", direction = - 1)
all_g

