# Programming Assignment #3 - Hospital Quality

dir <- "C:/Users/Enrique Velasco/Desktop/Coursera/Programming_Assignment_HospitalQuality"
setwd(dir)

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)

ncol(outcome)
nrow(outcome)
names(outcome)

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11], main = "Hospital 30 day Mortality Rates from Heart Attack",
                            xlab = "Days", 
                            ylab = "Mortal Heart Attacks Frecuency",
                            xlim = c(10, 24))

# Finding the best hospital in a state
# Should return a character vector with the best (lowest) 30 day mortality rate
# Outcomes: heart attack, heart failure, pneumonia 
# if an invalid outcome or state are prompt into BEST the function should stop
# and give the message "invalid outcome" or "invalid state"

BEST <- function(State_abb, Outcome_name){
  dir <- "C:/Users/Enrique Velasco/Desktop/Coursera/Programming_Assignment_HospitalQuality"
  setwd(dir)
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  T1<- data.frame("hospital name" = outcome1$Hospital.Name,
                  "heart attack" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                  "heart failure" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                  "pneumonia" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                  "state" = outcome1$State)
  
  # change outcome to lowercase
  outcome <- tolower(make.names(Outcome_name))
  
  # change variable name to prevent confusion
  chosen_state <- State_abb
  
  # Check state and outcome are valid, if not return warning message
  if (!chosen_state %in% unique(T1[["state"]])) {
    stop("Invalid state")
  }
  
  if (!outcome %in% c("heart.attack", "heart.failure", "pneumonia")) {
    stop("Invalid outcome")
  }
  
  # compute for the best hospital for the provided outcome
  T1 <- filter(.data = T1,state == chosen_state)
  T1 <- dplyr::select(.data = T1,outcome, hospital.name)
        colnames(x = T1)[1] = "outcome"
  T1 <- subset(x = T1, outcome != "Not Available")
  T1 <- arrange(.data = T1, as.numeric(T1$outcome),
                T1$hospital.name)
  
  unlist(T1[1,2])
  
}



    