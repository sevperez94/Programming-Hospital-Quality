rankhospital <- function(state_abb, outcome_name, rank){
  
  dir <- "C:/Users/Enrique Velasco/Desktop/Coursera/Programming_Assignment_HospitalQuality"
  setwd(dir)
  outcome1 <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  T1<- data.frame("hospital name" = outcome1$Hospital.Name,
                  "heart attack" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
                  "heart failure" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
                  "pneumonia" = outcome1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
                  "state" = outcome1$State)
  
  
  # change outcome to lowercase
  outcome <- tolower(make.names(outcome_name))
  
  # change variable name to prevent confusion
  chosen_state <- state_abb
  
  # Check state and outcome are valid, if not return warning message
  if (!chosen_state %in% unique(T1[["state"]])) {
    stop("Invalid state")
  }
  
  if (!outcome %in% c("heart.attack", "heart.failure", "pneumonia")) {
    stop("Invalid outcome")
  }
  
  # compute for the ranking of hospitals by state and outcome provided
  T1 <- filter(.data = T1,state == chosen_state)
  T1 <- dplyr::select(.data = T1,outcome, hospital.name)
  colnames(x = T1)[1] = "outcome"
  T1 <- subset(x = T1, outcome != "Not Available")
  T1 <- arrange(.data = T1, as.numeric(T1$outcome),
                T1$hospital.name)
  
  if (rank == "best"){
    rank <- min(c(1:length(T1[,1])))
  }
  if (rank == "worst"){
    rank <- max(c(1:length(T1[,1])))
  }
  
  T1$rank1 <- c(1:length(T1[,1]))
  
  if (!rank %in% unique(T1[,3])){
   
  }
  
  T1 <- T1[rank,]
  
  unlist(T1[1,2])
  
}
