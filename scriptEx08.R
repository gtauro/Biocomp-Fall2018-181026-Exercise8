rm(list = ls())  # clears global environment

setwd("~/Biocomp-Fall2018-181026-Exercise8")  # sets working directory to exercise file

# QUESTION 1
theBigGame <- read.delim("UWvMSU_1-22-13.txt", header = TRUE, sep = "\t", dec = ".")  # reads in text file for basketball data as a dataset

badgerBoxScore = matrix()  # creates a matrix for the storage of UW scores
badgerCounter = 0  # works as a counter for the UW score being stored into matrix
spartanBoxScore = matrix()  # creates a matrix for the storage of MSU scores
spartanCounter = 0  # works as a counter for the MSU score being stored into matrix

for (score in 1:nrow(theBigGame)) {  # begins for loop that repeats itself over the number of rows present in theBigGame
  if (theBigGame[score, 2] == "UW"){  # sets conditional for UW -  if UW is present in "team" column, score is added to badgerCounter which is then inputted into badgerBoxScore
    badgerCounter = badgerCounter + theBigGame[score,3]
    badgerBoxScore[score] = badgerCounter
    spartanBoxScore[score] = spartanCounter
  } else {
    spartanCounter = spartanCounter + theBigGame[score,3]  # sets conditional for MSU -  if MSU is present in "team" column, score is added to spartanCounter which is then inputted into spartanBoxScore
    spartanBoxScore[score] = spartanCounter
    badgerBoxScore[score] = badgerCounter
  }
}

plot(theBigGame$time, badgerBoxScore, type = "l", lty = 1, col = "red", xlab = "Time", ylab = "Score", main = "1/22/13 - MSU @ UW")  # plots badgerBoxScore on a line graph
lines(theBigGame$time, spartanBoxScore, lty = 2, col = "black")  # adds spartanBoxScore as a line to the existing line graph
legend(5, 45, legend = c("UW", "MSU"), col = c("red", "black"), lty = 1:2)  # creates a legend to help distinguish MSU from UW lines

# QUESTION 2
randomNum = sample(1:100, 1, replace = T)  # generates a random number between 1 and 100 and sets it as variable randomNum

guess <- function(){  # creates function for user input and defines input as an integer
  integer <- readline(prompt="Guess: ")  # user input as variable integer
  response <- as.integer(integer)  # variable integer converted into actual integer
  
  return(response)  # returns generated integer
}

guessCheck <- function(response){  # creates function that checks user input to see if randomNum is higher or lower
  higher <- "\nHigher"  # computer message if higher
  lower <- "\nLower"  # computer message if lower
  congrats <- "\nCongratulations, human! You've won this time."  # computer message if correct
  
  while (response != randomNum){  # conditional loop for initiating higher or lower messages
    if (response < randomNum){  # if less than randomNum, system says that randomNum is higher
      cat(higher)
      response <- as.integer(guess())
    } else {  # if higher than randomNum, system says that randomNum is lower
      cat(lower)
      response <- as.integer(guess())
    }
  }
  
  return(cat(congrats))  # returns congratulations message if user input is correct
  
}

paste(cat("\nHello, mortal. Let's play a game. I'm thinking of a number between 1-100. If you can guess it, you win."), guessCheck(guess()), "Exercise Complete.")  # initializes computer message, nests guess() function in guessCheck() function to start user input cycle, ends with exercise completion message
