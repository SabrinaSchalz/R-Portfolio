
library(tidyverse)
library(gridExtra)


#Stage 1 Figure

s1<- tribble(
  ~Language, ~Outcome, ~Count,
  "Vietnamese", "Approach", 11,
  "English", "No Approach", 7,
  "Vietnamese", "No Approach", 5,
  "English", "Approach", 7,
)

approach<-ggplot(data=s1, aes(x=Language, y=Count, fill = Outcome)) +
  geom_bar(stat="identity") + ylab("Number of Trials") +
	scale_fill_manual(values=c("#D55E00",
                             "Sky Blue")) + ggtitle("Phase 1 Training") +
		theme(legend.position="bottom",
		legend.text=element_text(size=8),
		legend.title=element_text(size=10),
			legend.key.size = unit(0.5,"line"))

approach


e<- tribble(
  ~Eating, ~Count,
  "During", 10,
  "After", 2,
  "No", 4,
)


eat<-ggplot(data=e, aes(x=Eating, y=Count)) +
  geom_bar(stat="identity", fill="#009E73") + ylab("") + ylim(0,13)
eat


grid.arrange(approach, eat, 
             ncol = 2, nrow = 1)


#Stage 2 Figure

s2<- tribble(
  ~Language, ~Outcome, ~Count,
  "Vietnamese", "Flip", 21,
  "English", "No Flip", 3,
  "Vietnamese", "No Flip", 1,
  "English", "Flip", 13,
)


flip<-ggplot(data=s2, aes(x=Language, y=Count, fill = Outcome)) +
  geom_bar(stat="identity") + ylab("Number of Trials") +
	scale_fill_manual(values=c("#D55E00",
                             "Sky Blue")) + ggtitle("Phase 2 Training")

flip

