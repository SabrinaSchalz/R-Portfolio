###TidyTuesday Week 2 2023 - Bird Feeder###

https://feederwatch.org/explore/raw-dataset-requests/
https://github.com/rfordatascience/tidytuesday/tree/master/data/2023/2023-01-10

library(tidyverse)

#download github csv's:

urlfile1="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv"
count<-read_csv(url(urlfile1))
urlfile2="https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv"
public<-read_csv(url(urlfile2))

#join both datasets by shared log id
data <- count %>%
inner_join(public, by = "loc_id")

#translation of species names
setwd("C:/Users/slyth/Documents/R")
tl<-read_csv("PFW-species-translation-table.csv")
data <- data %>%
inner_join(tl, by = "species_code")

#count of non-valid entries and reviewed entries, drop invalid
data %>%
count(valid, sort = TRUE)
data %>%
count(reviewed, sort = TRUE)
data <- data %>%
filter(valid == 1)

#count of species

species <- data %>%
count(american_english_name, sort = TRUE)

Crow <- data %>%
filter(american_english_name == "American Crow")

crow_count<-ggplot(data=Crow, aes(how_many)) +
	geom_bar() + xlab("individuals per sighting")
crow_count

#count of sightings with at least 50 crows
Crow %>%
filter(how_many >= 50)

#exploration whether there is anything interesting in the number of
#different feeders

#suet

C_suet <- Crow %>%
select(numfeeders_suet) %>%
replace(is.na(.), 0) %>%
count(numfeeders_suet)

C_suet <- C_suet %>%
	rename(Crow_Sightings = n)

total_suet <- data %>%
select(numfeeders_suet) %>%
replace(is.na(.), 0) %>%
count(numfeeders_suet)

total_suet <- total_suet %>%
	rename(Total_Sightings = n)

suet <- total_suet %>%
left_join(C_suet, by = "numfeeders_suet")

suet <- suet %>%
filter(numfeeders_suet < 20) %>%
replace(is.na(.), 0)

suet %>%
summarise(sum(Total_Sightings))

suet <- suet %>%
mutate(Total_Sightings_Per = (Total_Sightings/504838)*100)

suet %>%
summarise(sum(Crow_Sightings))

suet <- suet %>%
mutate(Crow_Sightings_Per = (Crow_Sightings/7533)*100)




#ground

C_ground <- Crow %>%
select(numfeeders_ground) %>%
replace(is.na(.), 0) %>%
count(numfeeders_ground)

C_ground <- C_ground %>%
	rename(Crow_Sightings = n)

total_ground <- data %>%
select(numfeeders_ground) %>%
replace(is.na(.), 0) %>%
count(numfeeders_ground)

total_ground <- total_ground %>%
	rename(Total_Sightings = n)

ground <- total_ground %>%
left_join(C_ground, by = "numfeeders_ground")

ground <- ground %>%
filter(numfeeders_ground < 20) %>%
replace(is.na(.), 0)

ground %>%
summarise(sum(Total_Sightings))

ground <- ground %>%
mutate(Total_Sightings_Per = (Total_Sightings/504965)*100)

ground %>%
summarise(sum(Crow_Sightings))

ground <- ground %>%
mutate(Crow_Sightings_Per = (Crow_Sightings/7533)*100)


#platform

C_platfrm<- Crow %>%
select(numfeeders_platfrm) %>%
replace(is.na(.), 0) %>%
count(numfeeders_platfrm, sort = TRUE)

C_platfrm<- C_platfrm%>%
	rename(Crow_Sightings = n)

total_platfrm<- data %>%
select(numfeeders_platfrm) %>%
replace(is.na(.), 0) %>%
count(numfeeders_platfrm, sort = TRUE)

total_platfrm<- total_platfrm%>%
	rename(Total_Sightings = n)

platfrm<- total_platfrm%>%
left_join(C_platfrm, by = "numfeeders_platfrm")

platfrm<- platfrm%>%
filter(numfeeders_platfrm< 20) %>%
replace(is.na(.), 0)

platfrm%>%
summarise(sum(Total_Sightings))

platfrm<- platfrm%>%
mutate(Total_Sightings_Per = (Total_Sightings/504904)*100)

platfrm%>%
summarise(sum(Crow_Sightings))

platfrm<- platfrm%>%
mutate(Crow_Sightings_Per = (Crow_Sightings/7533)*100)


####nice table for the ground dataset

library(gt)
library(webshot)

ground_table <- ground%>%
	rename(Crows = Crow_Sightings_Per) %>%
	rename(All = Total_Sightings_Per) %>%
	rename(Ground = numfeeders_ground) %>%
	select(Ground, All, Crows) %>%
	mutate_if(is.numeric, round, 1)


gt_tbl <- gt(ground_table) %>%
  tab_header(
    title = "Share of Sightings Per Number of Ground Feeders",
    subtitle = "American Crows"
  ) %>%
 tab_spanner(
    label = "Percentage",
    columns = c(All, Crows)
  ) %>%
  tab_spanner(
    label = "Number of Feeders",
    columns = Ground
  )

gt_tbl %>%
	gtsave("ground feeder crows.png", expand = 10)




#####Map
library(mapview)

coord <- Crow %>%
select(latitude, longitude)

crow_map<-mapview(coord, xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
