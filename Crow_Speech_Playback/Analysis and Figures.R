library(tidyverse)
options(scipen=999)
library(lme4)
library(gridExtra)

#Analysis
###London Playback Experiment###
daten<-read.table("Crows Language Ranking.csv",header=TRUE,sep=",", fileEncoding="UTF-8-BOM")

#Set variables
stimuli<-daten$Stimuli
speech<-daten$Speech
look<-daten$Look
walk<-daten$Walk
fly<-daten$Fly
any<-daten$Any
flight<-daten$Flight
group<-daten$Group
type<-daten$Type
ranking<-daten$Ranking
group<- as.factor(daten$Group)

#GLM
resp<- glm(ranking ~ stimuli, data = daten, family = poisson)
summary(resp)

###MK Playback Experiment###
daten<-read_csv("Crows Language S.csv")


#Set variables
stimuli<-daten$Stimuli
site<-daten$Site
flee<-daten$Flee

look<-daten$Look
walk<-daten$Walk
fly<-daten$Fly
any<-daten$Any


#GLM
resp<-glm(flee ~ stimuli, data = daten, family = poisson)


summary(resp)



#Boxplot Figures
daten<-read_csv("Figures.csv")

daten<- daten %>%
mutate(Response = factor(Response, levels=c("None", "Looking", "Walking", "Flying")))

daten<- daten %>%
mutate(Stimulus = factor(Stimulus, levels=c("Vietnamese", "English", "Parakeet", "Pigeon")))


Lon<- daten %>%
filter(Site == "London")

MK<- daten %>%
filter(Site == "Milton Keynes")

#hexcodes from colorBlindBlack8

l<-ggplot(data=Lon, aes(x=Stimulus, y=Count, fill = Response)) +
  geom_bar(stat="identity") + ggtitle("London") +
  scale_fill_manual(values=c("Sky Blue",
                             "#F0E442",
                             "#009E73",
                             "#D55E00"))
l

m<-ggplot(data=MK, aes(x=Stimulus, y=Count, fill = Response)) +
  geom_bar(stat="identity") + ggtitle("Milton Keynes") +
  scale_fill_manual(values=c("Sky Blue",
                             "#F0E442",
                             "#009E73",
                             "#D55E00"))
m

#Temporal Pattern Figures
temp<-read_csv("Temporal Pattern Figure.csv")

temp<- temp %>%
mutate(Response = factor(Response, levels=c("None", "Looking", "Walking", "Flying")))

Lon<- temp %>%
filter(Site == "London")

Lon1<- Lon%>%
filter(Section == 1)

Lon2<- Lon%>%
filter(Section == 2)

Lon3<- Lon%>%
filter(Section == 3)

MK<- temp %>%
filter(Site == "Milton Keynes")

MK1<- MK %>%
filter(Section == 1)

MK2<- MK %>%
filter(Section == 2)

MK3<- MK %>%
filter(Section == 3)


L1<- ggplot(Lon1, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
	axis.title.x=element_blank(), plot.margin=grid::unit(c(0,1,-2,1), "mm"))

L1<- L1+ theme(legend.position="top", aspect.ratio=1/8) + guides(colour=guide_legend(nrow=2)) + ggtitle("Milton Keynes")

L2<- ggplot(Lon2, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
	axis.title.x=element_blank(), plot.margin=grid::unit(c(-5,7,-10,7), "mm")
)

L2<- L2+ theme(legend.position = "none", aspect.ratio=1/8) 

L3<- ggplot(Lon3, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), plot.margin=grid::unit(c(-10,7,0,7), "mm"))
L3<-L3 + theme(legend.position = "none", , aspect.ratio=1/8)


grid.arrange(L1, L2, L3 , 
             ncol = 1, nrow = 3)


#MK
M1<- ggplot(MK1, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
	axis.title.x=element_blank(), plot.margin=grid::unit(c(0,1,-2,1), "mm"))

M1<- M1+ theme(legend.position="top", aspect.ratio=1/8) + guides(colour=guide_legend(nrow=2)) + ggtitle("Milton Keynes")

M2<- ggplot(MK2, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
	axis.title.x=element_blank(), plot.margin=grid::unit(c(-5,7,-10,7), "mm")
)

M2<- M2+ theme(legend.position = "none", aspect.ratio=1/8) 

M3<- ggplot(MK3, aes(x=Trial, y=Scale, fill=Response)) + 
  geom_point(aes(col=Response, shape=Group)) +
scale_colour_manual(values=c("Sky Blue",
                             "#E69F00",
                             "#009E73",
                             "#CC79A7")) + ylim(0,5) +
  labs(y="") + theme(legend.position="bottom") + theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), plot.margin=grid::unit(c(-10,7,0,7), "mm"))
M3<-M3 + theme(legend.position = "none", , aspect.ratio=1/8)


grid.arrange(M1, M2, M3 , 
             ncol = 1, nrow = 3)


