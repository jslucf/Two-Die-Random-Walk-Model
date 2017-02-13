# 2/13/17
# Question: if you have two die, how many rolls will it take before at least one rolls a 4? Use 60 trials.
# Modeling the solution using a random walk.

#setup a dice
dice=1:6

#setup a vector to hold results of random walk
count_rolls=c()

#simulate 60 times
for(n in 1:60){
  
  #turn counter to 1 for first roll
  count_rolls[n] = 1
  
  #this is a test to see if a four was rolled on either dice. set to 0 before first random walk
  four=0
  
  #this is the random walk which will keep running as long as a 4 is not rolled
  while(four == 0){
    
    #roll a dice twice
    dice_roll = sample(dice, 2, replace=T)
 
    #if either dice is a 4, then end the random walk to start the next one
    if(dice_roll[1] == 4 | dice_roll[2] ==4){
      four=1
      
    #if neither is a 4, then add 1 to the count for this random walk and then roll again
    } else{
      count_rolls[n] = count_rolls[n]+1
      four=0
    }
  }
}
#check the results
count_rolls

#histogram of random walk
hist(count_rolls, breaks = 1:max(count_rolls))

#summary stats
summary(count_rolls)
