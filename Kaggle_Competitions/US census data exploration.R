#US Census Data Exploration
#Two open-source ZIP code data files, A and Z

library(tidyverse)

#read data tables
A<-read_csv("data_A.csv")
Z<-read_csv("data_Z.csv")


#join by "zip" in Z and "zcta5" in A

data<- inner_join(A, Z, by = c("zcta5" = "zip"))

#Columns needed:
	#Question A - zip codes, states
	#Question B - latitude and longitude
	#Question C - population density
	#Question D - city, land area

data<- data %>% 
	select(zcta5, arealand, city, state, latitude, longitude, p0010001)


#Which state has the most 5-digit ZIP codes,
#and how many such codes does it have?

QA<-data %>%
count(state)
QA %>%
arrange(desc(n))
#Answer: Texas has the most zip codes at 1935.


#5-digit ZIP code which is
#i) the most easterly,
#ii) excluding Alaska, the most westerly
#iii) the most northerly.

#East is positive longitudes, highest is most east
#West is negative longitudes, lowest is most west
#North is positive latitudes, highest is most north

data %>%
arrange(desc(longitude))

#Answer: most easterly state is Maine

No_Alaska<-data %>%
filter(state != "Alaska")
No_Alaska %>%
arrange(longitude)
#Answer: most westerly state excluding Alaska is Hawaii

No_Alaska %>%
arrange(desc(latitude))

#Answer: most northern state excluding Alaska is Minnesota


#5-digit ZIP code has the highest population density?

#Adding a new column with population density
data<- data %>%
mutate(data, population_density = p0010001/arealand)

#sort highest to lowest population density
data %>%
arrange(desc(population_density))

#Answer: Zip code 20052 has the highest population density (0.06/m2, or 67,143/km2).


#Which states share the same 3-digit zip codes

#Add a new column called zip3 as a copy of the 5-digit zip code
#and then keep only the first 3 characters of each zip code
data<- data %>%
mutate(data, zip3 = zcta5)
data$zip3<-substr(data$zip3, 1, 3)

#group by 3-digit zip code, then count number of states
zip_count<-data %>%
group_by(zip3) %>%
summarise(Unique_States = n_distinct(state))

#sort highest to lowest
zip_count<-zip_count %>%
arrange(desc(Unique_States))

#Answer: zip codes 063, 834, and 968 are matched to more than 1 state each.

zip_063<-data %>%
filter(zip3 == "063")

view(zip_063)

#matches to several cities in Conneticut, and one city in New York.

zip_834<-data %>%
filter(zip3 == "834")

view(zip_834)

#matches to several cities in Idaho, and one city in Wyoming.

zip_968<-data %>%
filter(zip3 == "968")

view(zip_968)

#matches to cities in Hawaii, as well as two military bases shown as
#N/A as state, but which are in fact based in Hawaii as well.


#create a new dataset with these columns
new<- data %>%
select(zip3, state, p0010001, arealand)


#exclude the places that match to several states
new <- new %>%
filter(zip3 != "968" & zip3 != "834"& zip3 != "063")

#sum of populations by zip3

zip3_sum <- new %>%
group_by(zip3) %>% 
  summarise(p0010001= sum(p0010001))

#population size below 20k, but above single digits
small<-filter(zip3_sum, between(p0010001, 10, 19999))

#Answer: 13 rows, so 13 unique 3-digit zip codes have a population below 20k
#The smallest zip code is 821, with 369 residents.

data %>%
filter(zip3 == "821")

#Zip code 821 belongs to Yellowstone National Park

#plot to visualize the variation in population density of the
‘small’ 3-digit ZIP codes

plot<-ggplot(small, aes(x = zip3, y = p0010001, size = p0010001, colour = p0010001)) + 
  geom_point() + ylab("Population Size") + xlab("Zip Code") + theme(legend.position = "none")

plot


####
Comparing 3-digit and 5-digit zip code population sizes
####
library(gridExtra)

#summing up population sizes by zip3 and zip5
z3_sum <- data %>%
group_by(zip3) %>% 
  summarise(p0010001= sum(p0010001))

z5_sum <- data %>%
group_by(zcta5) %>% 
  summarise(p0010001= sum(p0010001))

#plotting distribution for each zip3 and zip5

z3_plot<-ggplot(z3_sum, aes(x = p0010001)) + 
  geom_histogram() + xlab("Population Size") + ggtitle("3-Zip Code")

z3_plot

z5_plot<-ggplot(z5_sum, aes(x = p0010001)) + 
  geom_histogram()+ xlab("Population Size") + ggtitle("5-Zip Code")

z5_plot

grid.arrange(z3_plot, z5_plot, 
             ncol = 2, nrow = 1)

####
Outliers
####

#check for highest population 3-digit zip codes
z3_sum %>%
arrange(desc(p0010001))

z3_median<-z3_sum %>%
summarise(median = median(p0010001, na.rm = TRUE))

data %>%
filter(zip3=="770")

z5_sum <- data %>%
group_by(zcta5) %>% 
  summarise(p0010001= sum(p0010001))

z5_sum %>%
arrange(desc(p0010001))

z5_median<-z5_sum %>%
summarise(median = median(p0010001, na.rm = TRUE))

####
Top 100 zip codes in terms of population size
####


z3_100 <- z3_sum %>%
slice_max(p0010001, n = 100)

z5_100 <- z5_sum %>%
slice_max(p0010001, n = 100)


z3_100_plot<-ggplot(z3_100, aes(x = p0010001)) + 
  geom_histogram()+ xlab("Population Size") + ggtitle("Top 100 3-Zip Code")

z3_100_plot<-z3_100_plot + theme(axis.text=element_text(size=6))

z5_100_plot<-ggplot(z5_100, aes(x = p0010001)) + 
  geom_histogram()+ xlab("Population Size") + ggtitle("Top 100 5-Zip Code")

z5_100_plot

grid.arrange(z3_100_plot, z5_100_plot, 
             ncol = 2, nrow = 1)



####
Chicago vs Houston
####

Chicago<-data %>%
filter(city=="Chicago")

Chicago_plot<-ggplot(Chicago, aes(x = p0010001)) + 
  geom_histogram()+ xlab("Population Size") + ggtitle("Chicago")

Chicago_plot

Houston<-data %>%
filter(city=="Houston" & zip3=="770")

Houston_plot<-ggplot(Houston, aes(x = p0010001)) + 
  geom_histogram()+ xlab("Population Size") + ggtitle("Houston")

Houston_plot


grid.arrange(Chicago_plot, Houston_plot, 
             ncol = 2, nrow = 1)


Chicago_sum <- Chicago %>%
  summarise(p0010001= sum(p0010001))

Houston_sum <- Houston %>%
  summarise(p0010001= sum(p0010001))


