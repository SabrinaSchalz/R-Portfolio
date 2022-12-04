#crow survey

setwd("C:/Users/slyth/Documents//R")
library(ggplot2)
library(gridExtra)

#fill = TRUE because some rows are empty
#barplot showing strong like to strong dislike ratings
daten<-read.table("like MK crows.csv",header=TRUE,sep=";", fill = TRUE)
daten

order<- c("Strong like", "Like", "Neutral", "Dislike", "Strong dislike")

like<-ggplot(data=daten, aes(x=ï..Value, y=Opinion, fill=ï..Value)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("orange1", "chartreuse2", "khaki", "orangered2", "chartreuse4")) +
  geom_text(aes(label=Opinion), vjust=1.1, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE)

like


#barplot showing frequency of feeding crows
daten<-read.table("Feeding MK Lon.csv",header=TRUE,sep=";")
daten

order<-c("Daily", "Sev.t.p.week", "Weekly", "Monthly", "Never")

feed<-ggplot(data=daten, aes(x=ï..Category, y=Percentage, fill=ï..Category)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("steelblue4", "steelblue1", "lightskyblue2", "steelblue3", "steelblue2")) +
geom_text(aes(label=ï..Category), vjust=1.1, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Interactions") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

feed


#barplot showing frequency of approaching crows
daten2<-read.table("Approaching.csv",header=TRUE,sep=";")
daten2

order<-c("Always", "Most.o.t.", "Half.o.t.", "Sometimes", "Never")

approach<-ggplot(data=daten2, aes(x=ï..Frequency, y=Approaching, fill=ï..Frequency)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("palegreen4", "palegreen2", "palegreen3", "darkseagreen2", "palegreen1")) +
geom_text(aes(label=Approaching), vjust=1.1, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Approaching") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

approach

#combine feeding and approaching figure
grid.arrange(feed, approach,
          ncol = 2, nrow = 1)


#barplot showing frequency of scaring crows
daten3<-read.table("Scare.csv",header=TRUE,sep=";")
daten3

order<-c("Daily", "Sev.t.p.week", "Weekly", "Monthly", "Never")

scare<-ggplot(data=daten3, aes(x=ï..Frequency, y=Scare, fill=ï..Frequency)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("gold4", "gold2", "gold1", "lightgoldenrod1", "gold3")) + geom_text(aes(label=Scare), vjust=0.5, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Scaring Away") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

scare


#barplot showing frequency of chasing crows
daten4<-read.table("Chase.csv",header=TRUE,sep=";")
daten4

order<-c("Always", "Most.o.t.", "Half.o.t.", "Sometimes", "Never")

chase<-ggplot(data=daten4, aes(x=ï..Frequency, y=Chase, fill=ï..Frequency)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("red4", "red1", "red", "red2", "red3")) +
geom_text(aes(label=Chase), vjust=0.5, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Chasing Away") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

chase


#frequency chased by kids
daten5<-read.table("Chase_Kids.csv",header=TRUE,sep=";")
daten5

order<-c("Always", "Most.o.t.", "Half.o.t.", "Sometimes", "Never")

chasek<-ggplot(data=daten5, aes(x=ï..Frequency, y=Kids.chase, fill=ï..Frequency)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("yellow4", "yellow1", "yellow", "yellow3", "yellow2")) +
geom_text(aes(label=Kids.chase), vjust=0.5, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Chasing (Kids)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

chasek


#frequency chased by dogs
daten6<-read.table("Chase_Dogs.csv",header=TRUE,sep=";")
daten6

order<-c("Always", "Most.o.t.", "Half.o.t.", "Sometimes", "Never")

chased<-ggplot(data=daten6, aes(x=ï..Frequency, y=Dogs.chase, fill=ï..Frequency)) +
  geom_bar(stat="identity") + scale_fill_manual(values=c("orange4", "orange1", "orange", "orange3", "orange2")) +
geom_text(aes(label=Dogs.chase), vjust=0.5, color="black", size=3.5)+
  xlab("") + ylab("%")+ ylim(0, 100)+ scale_x_discrete(limits = order) + guides(fill = FALSE) + ggtitle("Chasing (Dogs)") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

chased

#Combine barplots for scaring and chasing
grid.arrange(scare, chase, chasek, chased,
          ncol = 2, nrow = 2)



#Barplot showing types of complaints, frequency of each
daten7<-read.table("Complaints MK.csv",header=TRUE,sep=";")
daten7

complaints<-ggplot(data=daten7, aes(x=reorder(ï..Value, Annoyed), y=Annoyed, fill=ï..Value)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette = "Set3") +
geom_text(aes(label=Annoyed), vjust=1.1, color="black", size=3.5)+ labs(fill = "Behaviours") +
  xlab("") + ylab("%")+ ylim(0, 100) + ggtitle("Complaints About Crow Behaviour") +
theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

complaints

complaints + scale_fill_brewer(palette = "Set3", breaks=c("Taking food", "Attacking humans", "Damage", "Getting too close", "Other", "Droppings", "Noise", "Scaring other birds", "None"))

#comparison of each behaviour in MK and London
daten<-read.table("Crow survey Lon MK.csv",header=TRUE,sep=";")
daten

order<-c("Approaching", "Feeding", "Scaring", "Chasing", "Kids Chasing", "Dogs Chasing")

comp<-ggplot(data=daten, aes(x=ï..Interaction, y=Percentage, fill=City)) +
  geom_bar(stat="identity", position = position_dodge()) + scale_fill_manual(values=c("cornflowerblue", "goldenrod2")) +
  xlab("") + ylab("%")+ ylim(0, 100) + scale_x_discrete(limits = order) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


comp


#points scored in survey shown on Likert Scale
daten<-read.table("Crow Points MK.csv",header=TRUE,sep=";")
daten

myColors <- c("darkblue", "blue", "darkgreen","green","yellow","orange", "red", "darkred", "black")

points<-ggplot(data=daten, aes(x=Group, y=N, fill=ï..Points)) +
  geom_bar(position = "stack", stat="identity", width = 0.2) + coord_flip() +
 xlab("") + ylab("%") +
theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) + labs(fill = "Points")


points individual

#raw point distribution
daten<-read.table("Crow Points Raw MK.csv",header=TRUE,sep=";")
daten

points<-ggplot(daten, aes(x=ï..Points, y=N)) + geom_bar(stat="identity") + xlab("Points") + ylab("N") + xlim(-30, 20)

points

#overlapping histogram of points scored in MK and London
daten<-read_csv("Crows Points Raw Combined.csv")
daten


points<-ggplot(daten, aes(x=Points, y=N, fill = Group)) +
geom_histogram(stat="identity", alpha = 0.4) + xlab("Points") + ylab("N") + xlim(-30, 20)

points


#tests
daten<-read.table("Crow Points Groups.csv",header=TRUE,sep=";")
daten
female<-daten$ï..Female
male<-daten$Male
male<-na.omit(male)
#shapiro has to be over p=0.05 to be normally distributed
shapiro.test(female)
t.test(female, male, paired = FALSE, alternative = "two.sided")


daten<-read.table("Crow Points MK Lon.csv",header=TRUE,sep=";")
daten

wilcox.test(points ~ group, data = daten)

points<-daten$ï..points
group<-daten$group
shapiro.test(points)
#has to be over 0.05 for variance homogenity
library(car)
leveneTest(daten$ï..points~daten$group=="30t50",center=median)
#One-Way ANOVA
res.aov <- aov(points ~ age, data = daten)
summary(res.aov)

#kruskal wallis when conditions for anova not met
kruskal.test(points ~ park, data = daten)

pairwise.wilcox.test(daten$ï..points, daten$group,
                 p.adjust.method = "BH")


#plot all
boxplot(young, middle, old)

daten2$group <- factor(daten2$group , levels=c("Daily", "Weekly", "Rarely"))

p <- ggplot(daten2, aes(x=group, y=ï..points)) + 
  geom_boxplot() + xlab("Notice crows") + ylab("Points")

daten$group <- factor(daten$group , levels=c("Under 30", "30 to 50", "Over 50"))

a <- ggplot(daten, aes(x=group, y=ï..points)) + 
  geom_boxplot() + xlab("Age") + ylab("Points")


grid.arrange(a, p,
          ncol = 2, nrow = 1)

