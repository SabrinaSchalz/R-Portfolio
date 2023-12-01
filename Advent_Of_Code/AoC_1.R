#Advent of Code 2023
#Day1
#Puzzle 1

#Extract numbers out of letter strings, then sum up the first digit and the last digit

library(tidyverse)
library(readxl)


daten<-read_excel("./R/AoC_1.xlsx", col_names=FALSE)

#function to replace input character with nothing
#then run the function through the alphabet
remove <- function(input) {
 daten[] <- lapply(daten, gsub, pattern=input, replacement='')
return(daten)
}

#
daten<-remove('a')
daten<-remove('b')
daten<-remove('c')
daten<-remove('d')
daten<-remove('e')
daten<-remove('f')
daten<-remove('g')
daten<-remove('h')
daten<-remove('i')
daten<-remove('j')
daten<-remove('k')
daten<-remove('l')
daten<-remove('m')
daten<-remove('n')
daten<-remove('o')
daten<-remove('p')
daten<-remove('q')
daten<-remove('r')
daten<-remove('s')
daten<-remove('t')
daten<-remove('u')
daten<-remove('v')
daten<-remove('w')
daten<-remove('x')
daten<-remove('y')
daten<-remove('z')

sum<-daten%>%
rename(A = 1)

#Get the count of characters in each row
sum$char_count <- nchar(sum$A)

#seperate the rows with less than 2 and more than 2 characters
#single digits count twice (so 7 is 77)

sum_one<-sum%>%
filter(char_count == 1)

sum_one$C<-sum_one$A
sum_one$D<- with(sum_one, paste0(A, C))


sum_two<-sum%>%
filter(char_count == 2)

sum_rest<-sum%>%
filter(char_count > 2)

#first digit and the last digit only

sum_rest$left <- str_sub(sum_rest$A, 1, 1)
sum_rest$right <- str_sub(sum_rest$A, -1, -1)

sum_rest$B<- with(sum_rest, paste0(left, right))

#convert char to numeric
sum_one$D <- as.numeric(sum_one$D)
sum_two$A <- as.numeric(sum_two$A)
sum_rest$B <- as.numeric(sum_rest$B)


sum_one%>%
summarize(sum = sum(D))

sum_two%>%
summarize(sum = sum(A))

sum_rest%>%
summarize(sum = sum(B))

18249+17400+18912
#=54561


2893+5778+45405

#Puzzle 2
#words of numbers ("nine") also count as that value, and overlapping words
#like "sevenine" count as 79.


daten<-read_excel("./R/AoC_1.xlsx", col_names=FALSE)


#if I first overwrite these, I can then clear the remaining normal letters
daten[] <- lapply(daten, gsub, pattern='twone', replacement='21')
daten[] <- lapply(daten, gsub, pattern='eightwo', replacement='82')
daten[] <- lapply(daten, gsub, pattern='eighthree', replacement='83')
daten[] <- lapply(daten, gsub, pattern='oneight', replacement='18')
daten[] <- lapply(daten, gsub, pattern='threeight', replacement='38')
daten[] <- lapply(daten, gsub, pattern='fiveight', replacement='58')
daten[] <- lapply(daten, gsub, pattern='nineight', replacement='98')
daten[] <- lapply(daten, gsub, pattern='sevenine', replacement='79')


daten[] <- lapply(daten, gsub, pattern='one', replacement='1')
daten[] <- lapply(daten, gsub, pattern='two', replacement='2')
daten[] <- lapply(daten, gsub, pattern='three', replacement='3')
daten[] <- lapply(daten, gsub, pattern='four', replacement='4')
daten[] <- lapply(daten, gsub, pattern='five', replacement='5')
daten[] <- lapply(daten, gsub, pattern='six', replacement='6')
daten[] <- lapply(daten, gsub, pattern='seven', replacement='7')
daten[] <- lapply(daten, gsub, pattern='eight', replacement='8')
daten[] <- lapply(daten, gsub, pattern='nine', replacement='9')


#sums
2893+5778+45405
#54076

