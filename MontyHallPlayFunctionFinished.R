#
# Monty Hall Play Function
#
# Accepts a 'switch' parameter to indicate 
# whether to switch doors or stay. (FALSE by default)
#
# Returns TRUE if outcome is a win, FALSE otherwise.
#
# Note: code starts on the next Line (please remove the # sign if you want to use the function)
#
# pot1 and increaseBet1 is the questions where you increase by 1.2 everytime in the streak
#
# pot2 and increaseBet2 is the questions where you increase by adding 5 everytime in the streak
#
# the if statement with parameters of which switch it is, is the question that asks filps everytime the streak ends

montyHall.play <- function(switch=FALSE, n){
  countTrue <- 0
  countFalse <- 0
  streak <- 0
  increaseBet1 <- 5
  increaseBet2 <- 5
  potStart <- 0
  pot1 <- 0
  pot2 <- 0

  for(x in c(1:n)){
    doors <- sample(c('Car', 'Goat', 'Goat'))
    choices <- 1:3
  
    choice <- round(runif(1,1,3))
    remaining <- choices[-choice]
    doors2 <- doors[remaining]
    i <- match('Goat', doors2)
    doors3 <- doors2[-i]
    final <- doors3[1]
  
    if(switch == TRUE){
      win <- final == 'Car'
    }
    else{
      win <- final == 'Goat'
    }

    if(win == TRUE){
      countTrue <- countTrue + 1
      streak <- streak + 1
      potStart <- potStart + 5
#      pot1 <- pot1 + increaseBet1
#      pot2 <- pot2 + increaseBet2

#      increaseBet1 <- increaseBet1 * 1.2
#      increaseBet2 <- increaseBet2 * streak
    }
    else if(win == FALSE){
      countFalse <- countFalse + 1
      potStart <- potStart - 5
      streak <- 0
#      increaseBet1 <- 5
#      increaseBet2 <- 5

#      if(switch == FALSE){
#	switch <- TRUE
#      }
#      else if(switch == TRUE){
#	switch <- FALSE
#      }
    }
    print(win)
    if(x == n){
      cat("Pot Size after 100 trials = ", potStart, "\n")
#      cat("Pot Size after 100 trials = ", pot1, "\n")
#      cat("Pot Size after 100 trials = ", pot2, "\n")
      cat("False (or loses) = ", countFalse)
      cat("\nTrue (or wins) = ", countTrue, "\n")
    }
  }
}