#Goodreads Rating Prediction


library(tidyverse)
library(corrr)
library(tidytext)
setwd("C:/Users/slyth/Documents/Uni/R")

daten<-read_csv("goodreads_train.csv")

#have a first look at what type of data I have
view(daten)

#this almost made it crash and I suspect it's because of the review text
#so I'll exclude that one for now

daten<-select(daten, -review_text)

#first I want to get an overview what rating might correlate with
#certain users may have preferences
#certain books will be more or less popular
#dates are in a terrible format but I don't think they will be too interesting
#number of votes and comments may also be relevant


#daten$user_id<-as.numeric(daten$user_id)
#daten$review_id<-as.numeric(daten$review_id)
#that turned them all into NAs

daten <- daten %>%     
  group_by(user_id) %>%
  dplyr::mutate(User_ID = cur_group_id())


daten <- daten %>%     
  group_by(review_id) %>%
  dplyr::mutate(Review_ID = cur_group_id())

cor <- daten %>% 
  select(User_ID, book_id, Review_ID, rating, n_votes, n_comments) %>% 
  correlate()
cor

#review is weakly correlated with book_id, but nothing else
#otherwise the number of comments is correlated with number of votes
#book id is weakly correlated with the number of comments
#so some books just get more attention in comments and votes
#but that doesn't help


#can I analyse whether certain words correlate with ratings?

daten<-read_csv("goodreads_train.csv")

text <- daten%>%
select(c(review_text, rating))

text1<-text %>%
filter(rating=="5")

text1<- text1 %>%
  unnest_tokens(word, review_text)

five<-text1 %>%
count(word, sort=TRUE)

text2<-text %>%
filter(rating=="1")

#find the most common words by rating?

text2 <- text2 %>%
  unnest_tokens(word, review_text)

one<-text2 %>%
count(word, sort=TRUE)

##

texta1<-text %>%
filter(rating=="0")

texta1<- texta1 %>%
  unnest_tokens(word, review_text)

zero<-texta1 %>%
count(word, sort=TRUE)

texta2<-text %>%
filter(rating=="2")

texta2<- texta2 %>%
  unnest_tokens(word, review_text)

two<-texta2 %>%
count(word, sort=TRUE)

texta3<-text %>%
filter(rating=="3")

texta3<- texta3 %>%
  unnest_tokens(word, review_text)

three<-texta3 %>%
count(word, sort=TRUE)

texta4<-text %>%
filter(rating=="4")

texta4<- texta4 %>%
  unnest_tokens(word, review_text)

four<-texta4 %>%
count(word, sort=TRUE)

##

names(one)[2] <- 'One'
names(five)[2] <- 'Five'
names(zero)[2] <- 'Zero'
names(two)[2] <- 'Two'
names(three)[2] <- 'Three'
names(four)[2] <- 'Four'


comp<-full_join(zero, one, by="word")
comp<-full_join(comp, two, by="word")
comp<-full_join(comp, three, by="word")
comp<-full_join(comp, four, by="word")
comp<-full_join(comp, five, by="word")

#replace NA with 0
comp<- comp %>%
replace(is.na(.), 0)

#how many times more does each word appear in five than in one?

comp<-mutate(comp, Total = Five + One + Zero + Two + Three + Four)

comp<-mutate(comp, Zero_Per = Zero / Total * 100)
comp<-mutate(comp, One_Per = One / Total * 100)
comp<-mutate(comp, Two_Per = Two / Total * 100)
comp<-mutate(comp, Three_Per = Three / Total * 100)
comp<-mutate(comp, Four_Per = Four / Total * 100)
comp<-mutate(comp, Five_Per = Five / Total * 100)


comp<-comp%>%
filter(Total > 20)

#highest share of one
comp<-arrange(comp, desc(One_Per))
view(comp)



#for each column find the highest share
per<-comp%>%
select(c(word, Zero_Per, One_Per, Two_Per, Three_Per, Four_Per, Five_Per))
per$Largest_Column<-colnames(per)[apply(per,1,which.max)]

words<-per%>%
select(c(word, Largest_Column))
write_csv(words, "rating_predictor_words.csv")

####
test<-read_csv("goodreads_test.csv")

test<-test %>%
select(c(review_id, review_text))

test<- test %>%
  unnest_tokens(word, review_text)

words<-read_csv("rating_predictor_words.csv")

test<-test%>%
left_join(words, by="word")

test<-test %>%
select(c(review_id, Largest_Column))

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "Zero_Per", 0))
test

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "One_Per", 1))


test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "Two_Per", 2))

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "Three_Per", 3))

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "Four_Per", 4))

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "Five_Per", 5))


#now I still have N/A in there probably, and conflicting rating predictions
#if some words appear in the same review

#replace N/A with median 3 ranking
test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "N&A", 4))

test %>%
count(Largest_Column)

#replace N/A with 0, word with median 3 ranking

test<-test %>%
mutate(Largest_Column = replace(Largest_Column, Largest_Column == "word", 3))

test$Largest_Column<-as.numeric(test$Largest_Column)
###
test<-test %>%
replace(is.na(.), 3)

test_pivot<-test %>%
  group_by(review_id) %>%
  summarise(median = median(Largest_Column))

test_pivot%>%
count(median)

#a few .5 values in there, rounding up
test_pivot<-test_pivot %>%
mutate(median = replace(median, median == 1.5, 2))

test_pivot<-test_pivot %>%
mutate(median = replace(median, median == 2.5, 3))

test_pivot<-test_pivot %>%
mutate(median = replace(median, median == 3.5, 4))

test_pivot<-test_pivot %>%
mutate(median = replace(median, median == 4.5, 4))

#This has almost entirely 4-ratings, which might be a bit over optimistic

write_csv(test_pivot, "goodreads_submission.csv")

#0.35 accuracy
#52/78
#top 20 are between 0.55 and 0.65 accuracy

