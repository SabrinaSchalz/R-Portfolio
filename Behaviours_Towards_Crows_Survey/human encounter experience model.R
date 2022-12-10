#learning about human encounter types

#code based on agent-based model, but it doesn't have any agents anymore
#it just has an experience tracker
#and an encounter event that produces point changes at certain odds
#that are representing the encounter regardless of encounter outcome
#over a certain number of iterations / time


#track experience points
points<-1;

#for each type of encounter directly take off or add the points
#sampling number to represent odds
encounter<-function(points){
	human_type<-sample(x = 1:100, replace = TRUE);
	if(human_type == 1 | human_type == 2) {
points<- points -10
} else if(human_type > 2 && human_type < 11) {
points <- points + 1
} else {
points <- points + 0.1
 }
return(points)
}

#setting London: =1 chaser (1%), >1, <8 feeder (6%), else neutral
#setting MK: 1 or 2 chaser (2%), >2, <11 (8%), else neutral


############iterations loop########10951
iterations <- 10951;
i <- 1;
iterations_log<- NULL
while(i < iterations){
{
      points <- encounter(points);
}
iterations_log[[i]] <- points;
i<-i + 1;
}

print(iterations_log)

#save and reset
capture.output(print(iterations_log), file = "crow20_MK.csv")
points<-1

#note: the excel file is an absolute mess and will need cleaning
#1. text to columns, delimited by space
#2. delete content in column A
#3. remove blank rows: select column B, find&select, go to special, blanks, delete
#4. pray that excel doesn't crash
#5. replace iteration numbers in column A
#6. Column headers: Iteration, Crow_1, Crow_2, Crow_3...

#plotting
setwd("C:/Users/slyth/Documents/Uni/R")
library(ggplot2)
library(gridExtra)
daten<-read.table("learning_London.csv",header=TRUE,sep=",")
daten2<-read.table("learning_MK.csv",header=TRUE,sep=",")

London<- ggplot(daten, aes(x=Iterations)) +
geom_line(aes(y = Crow_1), color = "lightgrey") +
geom_line(aes(y = Crow_2), color = "lightgrey") +
geom_line(aes(y = Crow_3), color = "lightgrey") +
geom_line(aes(y = Crow_4), color = "lightgrey") +
geom_line(aes(y = Crow_5), color = "lightgrey") +
geom_line(aes(y = Crow_6), color = "lightgrey") +
geom_line(aes(y = Crow_7), color = "lightgrey") +
geom_line(aes(y = Crow_8), color = "lightgrey") +
geom_line(aes(y = Crow_9), color = "lightgrey") +
geom_line(aes(y = Crow_10), color = "lightgrey") +
geom_line(aes(y = Crow_11), color = "lightgrey") +
geom_line(aes(y = Crow_12), color = "lightgrey") +
geom_line(aes(y = Crow_13), color = "lightgrey") +
geom_line(aes(y = Crow_14), color = "lightgrey") +
geom_line(aes(y = Crow_15), color = "lightgrey") +
geom_line(aes(y = Crow_16), color = "lightgrey") +
geom_line(aes(y = Crow_17), color = "lightgrey") +
geom_line(aes(y = Crow_18), color = "lightgrey") +
geom_line(aes(y = Crow_19), color = "lightgrey") +
geom_line(aes(y = Crow_20), color = "lightgrey") +
geom_line(aes(y = Median), color = "darkred") + ylab("Points") + ylim(-650,800) + xlab("Iterations London")

London

MK<- ggplot(daten2, aes(x=Iterations)) +
geom_line(aes(y = Crow_1), color = "lightgrey") +
geom_line(aes(y = Crow_2), color = "lightgrey") +
geom_line(aes(y = Crow_3), color = "lightgrey") +
geom_line(aes(y = Crow_4), color = "lightgrey") +
geom_line(aes(y = Crow_5), color = "lightgrey") +
geom_line(aes(y = Crow_6), color = "lightgrey") +
geom_line(aes(y = Crow_7), color = "lightgrey") +
geom_line(aes(y = Crow_8), color = "lightgrey") +
geom_line(aes(y = Crow_9), color = "lightgrey") +
geom_line(aes(y = Crow_10), color = "lightgrey") +
geom_line(aes(y = Crow_11), color = "lightgrey") +
geom_line(aes(y = Crow_12), color = "lightgrey") +
geom_line(aes(y = Crow_13), color = "lightgrey") +
geom_line(aes(y = Crow_14), color = "lightgrey") +
geom_line(aes(y = Crow_15), color = "lightgrey") +
geom_line(aes(y = Crow_16), color = "lightgrey") +
geom_line(aes(y = Crow_17), color = "lightgrey") +
geom_line(aes(y = Crow_18), color = "lightgrey") +
geom_line(aes(y = Crow_19), color = "lightgrey") +
geom_line(aes(y = Crow_20), color = "lightgrey") +
geom_line(aes(y = Median), color = "darkred") + ylab("") + ylim(-650,800) + xlab("Iterations Milton Keynes")
MK



grid.arrange(London, MK, ncol=2)
