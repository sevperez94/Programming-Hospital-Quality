rankall <- function(outcome_name, rank = "best"){
  
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
  
  if (!outcome %in% c("heart.attack", "heart.failure", "pneumonia")) {
    stop("Invalid outcome")
  }
  
  T1 <- dplyr::select(.data = T1, outcome, hospital.name, state)
  colnames(x = T1)[1] = "outcome"
  T1 <- subset(x = T1, outcome != "Not Available")
  T2 <- lapply(X = split(x = T1, f = T1$state), FUN = function(x){
    
              arrange(x, as.numeric(outcome))
  
    })
  
  T3 <- lapply(X = T2, function(x) cbind(x, rank1=seq_len(NROW((x)))))
  
  T4 <- bind_rows(T3)
  
  if (rank == "best"){
    rank <- 1
    T4 <- subset(x = T4, T4$rank1 == rank)
    T4 <- dplyr::select(.data = T4, "hospital.name", "state")
    T4
  }
 else if (rank == "worst") {
    T5 <- T4 %>%
      group_by(state) %>%
      filter(rank1 == max(rank1)) %>%
      dplyr::select(hospital.name, state)
    T5
  }
 else {
  totalstate <- unique(x = T4$state)
  full_state <- tibble(state = totalstate)
  T4 <- subset(x = T4, T4$rank1 == rank)
  T4 <- dplyr::select(.data = T4, "hospital.name", "state")
  T5 <- left_join(full_state, T4, by = "state")
  T5
 }
}
