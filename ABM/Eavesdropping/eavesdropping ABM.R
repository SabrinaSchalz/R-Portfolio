#Agent Based Model - Eavesdropping, Monitoring, and Hopping
#based on A. Bradley Duthie github template
#https://bradduthie.github.io/blog/individual-based-models-in-r/

#to play with patch size, you need to adjust starting location,
#max in movement,
#sampling range in long,
#the 2 spots in the "running code" section sampling for location,
#and the location reset underneath "running code"

#to add visual obstacles, change the accuracy ratio in long

#table discribing 2 individuals with 3 traits
id <- array(data = 0, dim =c(2,3));
colnames(id) <- c("x_loc", "y_loc", "points");
rownames(id) <- c("crow", "human");

#assign values to traits in [row #, column #] with value 21000 for crow and 0 for human.
id[1, 3] <- (n = 21000);
id[2, 3] <- (n = 1000000);

#starting location
#assign location by randomly sampling from 1:20 for a 20x20 patch
id[, 1] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);
id[, 2] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);
print(id);

#moving all individuals randomly by max 1 cell in any direction per iteration
#if individuals try to leave the square, they instead move back towards the center
movement <- function(id, x_loc = 1, y_loc = 2){
    movers   <- dim(id)[1]; # Get the number of individuals in inds
    move_dists   <- c(-1, 0, 1);  # Define the possible distances to move
    x_move       <- sample(x = move_dists, size = movers, replace = TRUE);
    y_move       <- sample(x = move_dists, size = movers, replace = TRUE);
    id[, x_loc] <- id[, x_loc] + x_move;
    id[, y_loc] <- id[, y_loc] + y_move;
    max <- 20
for(i in 1:movers){               # For each individual i in the array
        if(id[i, x_loc] > max){         # If it moved passed the maximum xloc
            id[i, x_loc] <- max - 1;    # Then move it back toward the centre
        }
        if(id[i, x_loc] < 1){            # If it moved below 1 on xloc
            id[i, x_loc] <- 2;           # Move it toward the centre (2)
        }
        if(id[i, y_loc] > max){         # If it moved passed the maximum yloc
            id[i, y_loc] <- max - 1;    # Then move it back toward the centre
        }
        if(id[i, y_loc] < 1){            # If it moved below 1 on yloc
            id[i, y_loc] <- 2;           # Then move it toward the centre (2)
        }
    } 
    return(id);
}

#if human and crow are on the same spot, the stop vector is changed to "2"
#triggering the while condition that runs the iteration to break
predation <- function(id, x_loc = 1, y_loc = 2, stop){
    individuals   <- dim(id)[1];
for(i in 1:individuals){
          xloc1 <- id[1, x_loc];
          yloc1 <- id[1, y_loc];
          xloc2 <- id[2, x_loc];
          yloc2 <- id[2, y_loc];
             stop <-if(xloc1 == xloc2 && yloc1 ==yloc2) {
		TRUE
		}else {
           FALSE }
return(stop)
}}


#crow forages on every uneven time step and gains points from it
#if it has less than 25k currently
foraging <- function(id, points = 3){
    individuals<- dim(id)[1];
	max <- 25000;
	for(i in 1:individuals){
if((ts %% 2) == 0 & id[1, points] < max) {
id[, points] <- id[, points] + sample(x = 10:65, size = dim(id)[1], replace = TRUE);
}else {
id[, points] <- id[, points]
 }

return(id)
}}

#crow eavesdrops while foraging, 20% accuracy
eavesdropping <- function(id){
    individuals   <- dim(id)[1];{
		x_eav <- sample(x = 1:5, size = 1, replace = TRUE);
if((ts %% 2) == 0) {
		found<-if(x_eav == 1) {
		TRUE
		}else {
           FALSE }}
return(found)
}}

#long-range scanning, takes a random x column to scan for H
#accuracy is the visual obstacles, 1:1 ==1 is 100% vision
	#1:4 with !=1 is 75%
	#1:4 with ==1 is 25% and so on
long <- function(id, x_loc = 1, y_loc = 2){
    individuals   <- dim(id)[1];
for(i in 1:individuals){
          xloc2 <- id[2, x_loc];
		long_range<- sample(x = 1:20, size = 1, replace = TRUE);
		accuracy<-sample(x = 1:1, size = 1, replace = TRUE);
if((ts %% 2) != 0 && found == FALSE && accuracy == 1) {
		found <- if(long_range == xloc2) {
		TRUE
		}else {
           FALSE }}
return(found)
}}

#scans immediate surroundings for H
scan <- function(id, x_loc = 1, y_loc = 2){
    individuals   <- dim(id)[1];
for(i in 1:individuals){
          xloc2 <- id[2, x_loc];
          yloc2 <- id[2, y_loc];
          a <- id[1, x_loc] + 2;
          b <- id[1, y_loc] + 2;
          c <- id[1, x_loc] - 2;
          d <- id[1, y_loc] - 2;
		x_scan <- sample(x = a:c, size = 1, replace = TRUE);
		y_scan <- sample(x = b:d, size = 1, replace = TRUE);
if((ts %% 2) != 0) {
		 flee2 <-if(x_scan == xloc2 && y_scan ==yloc2) {
		TRUE
		}else {
           FALSE }}
return(flee2)
}}


#monitoring, needs to be turned on my eavesdropping or long-range
monitor <- function(id, x_loc = 1, y_loc = 2){
    individuals   <- dim(id)[1];
for(i in 1:individuals){
          xloc2 <- id[2, x_loc];
          yloc2 <- id[2, y_loc];
          xloc1 <- id[1, x_loc];
          yloc1 <- id[1, y_loc];
          a <- xloc1 - xloc2;
          b <- yloc1 - yloc2;
          yloc1 <- id[1, y_loc];	
if((ts %% 2) != 0 && found == TRUE) {
		 flee3 <-if(a < 3 && a > -3 && b < 3 && b > -3) {
		TRUE
		}else {
           FALSE }}
return(flee3)
}}


#if either predation, scan or monitoring flight is triggered, points are taken off
points<-function(id, points = 3){
    individuals<- dim(id)[1];
	for(i in 1:individuals){
if(stop == TRUE) {
id[, points] <- id[, points] - sample(x = 1002:3502, size = dim(id)[1], replace = TRUE);
} else if(flee3 == TRUE || flee2 == TRUE) {
id[, points] <- id[, points] - sample(x = 12:67, size = dim(id)[1], replace = TRUE);
} else {
id[, points] <- id[, points]
 }
return(id)
}}


#maintenance point deduction per iteration, to stay alive
maint<-function(id, points = 3){
    individuals<- dim(id)[1];{
id[1, 3] <- id[1, 3] -1400}
return(id)
}

#check whether crow is still alive
alive<-function(id, points = 3){
    individuals<- dim(id)[1];{
if(id[1, 3] < 1){            
            dead <- TRUE;           
        } else {
	dead <- FALSE}
return(dead)
}}


############the model########

###reset settings###
time_steps <- 101;
ts <- 1;
time_log  <- NULL
reason_log  <- NULL
iterations<- 301;
i <- 1;
iterations_log<- NULL
found<-FALSE    
flee3 <- FALSE
flee2 <- FALSE

####run the model###
#to pause eavesdropping or long-range scanning, put # in front of their line
while(i < iterations){
while(ts < time_steps){
      id <- movement(id);
	stop <-predation(id);
	id <-foraging(id);
if(found == FALSE) {
	found <-eavesdropping(id)};
#if(found == FALSE) {
	#found <-long(id)};
flee3 <-monitor(id);
if(flee3 == FALSE) {
	flee2 <-scan(id);}
if(flee2 == FALSE && flee3 ==FALSE) {
	flee2 <-scan(id)};
if(flee3 == TRUE || flee2 == TRUE) {
	id[, 1] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE)};
reason<-if(stop == TRUE){
"predation";
}else{
"iteration ends";
}
	id <-points(id);
   time_log[[ts]] <- id;
if(stop == TRUE) {
break
} else {
    ts   <- ts + 1; 
}}
found<-FALSE    
flee3 <- FALSE
flee2 <- FALSE
iterations_log[[i]] <- id;
reason_log[[i]] <- reason;
i<-i + 1;
id<-maint(id)
dead<-alive(id)
if(dead == TRUE) {
break
} else {
id[, 1] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);
id[, 2] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);
ts <- 1;
time_log  <- NULL
}}

##########

print(iterations_log)

#reset points and location
id[1, 3] <- (n = 21000);
id[2, 3] <- (n = 1000000);
id[, 1] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);
id[, 2] <- sample(x = 1:20, size = dim(id)[1], replace = TRUE);


#save output as CSV into documents folder
capture.output(print(reason_log), file = "reason_test.csv")
capture.output(print(time_log), file = "points_test.csv")