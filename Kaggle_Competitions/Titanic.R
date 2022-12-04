#Titanic Competition Attempt 1 - 0.775 accuracy

library(tidyverse)
library(corrr)
setwd("C:/Users/slyth/Documents/Uni/R")

daten<-read_csv("train_titanic.csv")

#have a first look at what type of data I have
head(daten)

#have a look at some first suspisions of trends.
#1. Survived by Pclass  might be interesting in a stacked barplot
#2. can check age in a boxplot
#3. sex may be interesting in a stacked barplot
#4. but I suspect this will interact with family status
#4a.e.g. fathers dying more
#4b. eg very young kids with parch=0 (no family with them) dying more
#4c.parents saving their kids first
#note: parch is the number of either parents or children on board
#so to get status of parent or child I should correlate this with age
#sibsp is number of siblings and spouses, again use age to get status
#5. cabin number might be relevant depending on where the lifeboats were
#or which side got hit by the iceberg

#survived vs Pclass
daten$Pclass<-as.factor(daten$Pclass)
daten$Survived<-as.factor(daten$Survived)
PC<-ggplot(daten, aes(x=Pclass, fill=Survived)) +
	geom_bar()
PC
#most of first class survived, approximately half of second, and a minority
#in third class

PC_count<-daten %>%
count(Pclass)

PC_count<-daten %>%
group_by(Survived)%>%
count(Pclass)

#first class survived: 136/216 (63%)
#second class survived: 87/184 (47%)
#third class survived: 119/491 (24%)


#age vs survived
Age<-ggplot(daten, aes(x=Survived, y=Age)) +
geom_boxplot()

Age
#no visible difference between groups


#Sex basic
Sex<-ggplot(daten, aes(x=Sex, fill=Survived)) +
	geom_bar()
#Most women survived, most men did not

#Sex and Age
Women<-daten %>%
filter(Sex=="female")

Women_Age<-ggplot(Women, aes(x=Survived, y=Age)) +
	geom_boxplot()
Women_Age

#Survivors slightly older, but not massively

Men<-daten %>%
filter(Sex=="male")

Men_Age<-ggplot(Men, aes(x=Survived, y=Age)) +
	geom_boxplot()
Men_Age

#Survivors slightly younger, but not massively

#Sex by Pclass
Women_PC<-ggplot(Women, aes(x=Pclass, fill=Survived)) +
	geom_bar()
#almost all first-class women survived
#almost all second-class women survived
#around half of third-class women survived

Men_PC<-ggplot(Men, aes(x=Pclass, fill=Survived)) +
	geom_bar()
#just under half of first-class men survived
#almost all second and third class men died

#Children
Children<-daten %>%
filter(Age<15)

Children_Age<-ggplot(Children, aes(x=Survived, y=Age)) +
	geom_boxplot()
Children_Age

#trend appearing, focusing in on young children

YoungChildren<-daten %>%
filter(Age<10)

YChildren_Age<-ggplot(YoungChildren, aes(x=Survived, y=Age)) +
	geom_boxplot()
YChildren_Age

#most children who survived were between 1 and 5 years old,
#most who died were between 2 and 8 years old.

#Elderly
#Barely anyone above 60 anyway
Old<-daten %>%
filter(Age>55)

Old_Age<-ggplot(Old, aes(x=Survived, y=Age)) +
	geom_boxplot()
Old_Age
#no trend

#group by age
Age_count<-daten %>%
group_by(Age)%>%
count(Survived)
Age_count

Age_Trend<-ggplot(Age_count, aes(x=Age, y=n, group=Survived, colour=Survived)) +
	geom_line() + ylim(0, 20) + xlim(0,20)
Age_Trend

Age_count_young<-Age_count %>%
filter(Age<10)
Age_count_young

Age_y<-ggplot(Age_count_young, aes(x=Age, y=n, fill=Survived)) +
	geom_bar(position="stack", stat="identity")

Age_y

a<-daten %>%
count(Age)

#Overall for children it's very mixed and no more than 10 kids per age group,
#so this is not producing fruitful predictions

#correlations matrix
daten$Survived<-as.numeric(daten$Survived)
daten$Pclass<-as.numeric(daten$Pclass)
daten$Age<-as.numeric(daten$Age)
cor <- daten %>% 
  select(Survived, Pclass, Age, SibSp, Parch) %>% 
  correlate()
cor
#only Pclass is correlated in this, which I also know from the other exploration
#Specifically Pclass+Sex

#Sex by Parent Status
#Assuming parents of minors will be at least 18 years old and no older than 50
#Given average age of 30 at childbirth at the time
#Passengers with parch =! 0 between 18 to 50 should get parent flag
#though this of course may also include childfree adults with their parents on board

#how many mothers were there even?
mothers<-daten %>%
filter(Sex =="female", Age>18, Parch==1)

#33 mothers

mothers %>%
count(Survived)

mothers %>%
count(Survived, Pclass)

#81% of mothers survived
#100% of mothers in first class survived (13 total)
#91% of mothers in second class survived (12 total)
#37% of mothers in third class survived (8 total)
#compared to women overall:

Women %>%
count(Survived, Pclass)
#96% of women in first class survived
#92% of women in second class survived
#50% of women in third class survived

#Trend matches and there are so few mothers, female+class still seems to be the most reliable

#Maybe for kids class is relevant too, i.e. your mother survives
#so you survive?

kids<-daten %>%
filter( Age>18)

kids<- kids %>%
count(Survived, Pclass)

#63% of kids in first class survived
#41% of kids in second class survived
#20% of kids in third class survived

#compared to all passengers

daten %>%
count(Survived, Pclass)

#63% in first class survived
#47% of second class survived
#24% of third class survived

#so the kids are surviving based on their class like everyone
#just women in the top two classes had an almost perfect chance
#and women in third class still had double the survival than their class

#what about kids on their own?
kids_alone<- daten %>%
filter(Age<18, Parch==0)
kids_alone %>%
count(Survived, Pclass)

#100% first class survived
#60% second class
#41% third class
# no effect


#Who were the first and second class women who did not survive?
dead_women<-daten %>%
filter(Sex=="female", Pclass !=3, Survived==0)
dead_women
#nothing obvious, only 9, two were a mother with her daughter, just unlucky

women3<-daten %>%
filter(Sex=="female", Pclass ==3)

women3%>%
count(SibSp, Survived)

#Parch 0: 59% survived
#Parch 1: 50%
#Parch 2: 32%
#Sib 0: 59%
#Sib 1: 45%

women3a<-women3%>%
count(Age, Survived)



#third class women survival by age
Age_W3<-ggplot(women3a, aes(x=Age, y=n, group=Survived, colour=Survived)) +
	geom_line() + xlim(0,35)
Age_W3

#died when over 30, but otherwise no clear trend

#is it worse to have more Parch or SibSp?

p<-daten%>%
count(Parch, Survived)

#pretty even split

s<-daten%>%
count(SibSp, Survived)

#3 or more seems to tend towards less survival

women3 %>%
count(SibSp, Survived)

#The iceberg hit the titanic around midnight, so you might expect
#people to be in bed at this time
#so some cabins might have been more unlucky than others
#but third class didn't have cabins

#the other way around, excluding the first+second class women,
#who survived? 50% of 3rd class women, and some men.

men<-daten %>%
filter(Sex=="male")

men%>%
count(Pclass, Survived)
#37% in first class
#15% in second class
#13% in third class


#how many survivors are not female 1+2?

daten %>%
count(Sex=="female", Pclass)
daten %>%
count(Survived==1)

#50% of survivors are covered by female 1+2
#so what's missing is the predictor for 3rd class women
#and first class men

women3<- daten%>%
filter(Sex=="female", Pclass==3)
cor <- women3 %>% 
  select(Survived, Age, SibSp, Parch) %>% 
  correlate()
cor
#negative correlation for sibsp and parch
#minor negative correlation for age

men1<-daten%>%
filter(Sex=="male", Pclass==1)

cor <- men1%>% 
  select(Survived, Age, SibSp, Parch) %>% 
  correlate()
cor


#negative correlation for age, none for SibSp and Parch
#men age survival range
men1$Survived<-as.factor(men1$Survived)
Men1_Age<-ggplot(men1, aes(x=Survived, y=Age)) +
	geom_boxplot()
Men1_Age

#all young first class men
#and then most until 40

men1young<-men1%>%
filter(Age<45)
men1young%>%
count(Survived)

#50% of first class men under 45 survived

men1young$Survived<-as.numeric(men1young$Survived)
cor <- men1young%>% 
  select(Survived, Age, SibSp, Parch) %>% 
  correlate()
cor

w<-women3%>%
count(Survived, SibSp, Parch)

#59% 0 parch
#50% 1 parch
#downhill from there

#59%
#44%

#0: 61%
#single women with 0 SibSp and 0 Parch

singlew<-women3%>%
filter(SibSp==0, Parch==0)

cor <- singlew%>% 
  select(Survived, Age) %>% 
  correlate()
cor

singlew %>%
count(Survived, Embarked)

e<-daten%>%
count(Survived, Embarked)
#C: 29%
#S: 33%
#Q: 39%


#so my main contestants are women who are either in first class, second class,
# or single (0 parch 0 sib), and men in first class younger than 15

potential_survivor1<-daten%>%
filter(Sex=="female", Pclass !=3)

potential_survivor2<-daten%>%
filter(Sex=="female", Pclass==3, Parch==0, SibSp==0)

potential_survivor3<-daten%>%
filter(Sex=="male", Pclass==1, Age<15)

potential_survivors<-bind_rows(potential_survivor1, potential_survivor2, potential_survivor3)

potential_survivors %>%
count(Survived)

#finding 77% in my data

#testing on test data:
daten<-read_csv("test_titanic.csv")
potential_survivor1<-daten%>%
filter(Sex=="female", Pclass !=3)

potential_survivor2<-daten%>%
filter(Sex=="female", Pclass==3, Parch==0, SibSp==0)

potential_survivor3<-daten%>%
filter(Sex=="male", Pclass==1, Age<15)

potential_survivors<-bind_rows(potential_survivor1, potential_survivor2, potential_survivor3)

write_csv(potential_survivors, "ps.csv")


#Score: 0.775
#Rank: 7,617 out of 13,319
