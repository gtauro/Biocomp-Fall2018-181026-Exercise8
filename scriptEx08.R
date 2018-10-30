rm(list = ls())  # clears global environment

setwd("~/Biocomp-Fall2018-181026-Exercise8")  # sets working directory to exercise file

# QUESTION 1
theBigGame <- read.delim("UWvMSU_1-22-13.txt", header = TRUE, sep = "\t", dec = ".")

badgerBoxScore = matrix()
badgerCounter = 0
spartanBoxScore = matrix()
spartanCounter = 0

for (score in 1:nrow(theBigGame)) {
  if (theBigGame[score, 2] == "UW"){
    badgerCounter = badgerCounter + theBigGame[score,3]
    badgerBoxScore[score] = badgerCounter
    spartanBoxScore[score] = spartanCounter
  } else {
    spartanCounter = spartanCounter + theBigGame[score,3]
    spartanBoxScore[score] = spartanCounter
    badgerBoxScore[score] = badgerCounter
  }
}

plot(theBigGame$time, badgerBoxScore, type = "l", lty = 1, col = "red", xlab = "Time", ylab = "Score", main = "1/22/13 - MSU @ UW")
lines(theBigGame$time, spartanBoxScore, lty = 2, col = "black")
legend(5, 45, legend = c("UW", "MSU"), col = c("red", "black"), lty = 1:2)

# QUESTION 2
randomNum = sample(1:100, 1, replace = T)

guess <- function(){
  integer <- readline(prompt="Guess: ")
  response <- as.integer(integer)
  
  return(response)
}

guessCheck <- function(response){
  higher <- "\nHigher"
  lower <- "\nLower"
  congrats <- "\nCongratulations, human! You've won this time."
  
  while (response != randomNum){
    if (response < randomNum){
      cat(higher)
      response <- as.integer(guess())
    } else {
      cat(lower)
      response <- as.integer(guess())
    }
  }
  
  return(cat(congrats))
  
}

paste(cat("\nHello, mortal. Let's play a game. I'm thinking of a number between 1-100. If you can guess it, you win."), guessCheck(guess()), "Exercise Complete.")
