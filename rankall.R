rankall <- function(outcome, num = "best") {
	## Read outcome data
	outcomes <- read.csv("outcome-of-care-measures.csv", 
			     colClasses = "character")
	
	## Check that state and outcome are valid
	
	#validate input
	valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
	if(!is.element(outcome, valid.outcomes)) 
		stop("invalid outcome")
	
	# Determine the col index
	index <- if(outcome == "heart attack")
		11
	else if (outcome == "heart failure")
		17
	else    23
	
	# Remove NA's
	outcomes[, index] <- as.numeric(outcomes[, index])
	outcomes <- outcomes[!is.na(outcomes[, index]), ]
	
	
	## For each state, find the hospital of the given rank
	hospital.names <- c()
	states <- c()
	
	for (state in unique(outcomes$State)) {
		# Retreive all hospitals of this state
		state.hospitals <- outcomes[outcomes$State == state, ]
		
		# Sort this subset and select the appropiate hsopital by rank
		sorted <- order(state.hospitals[, index], state.hospitals$Hospital.Name)
		state.hospitals <- state.hospitals[sorted, ]
		
		# formate num into an integer
		rank <- if(num == "best")
			1
		else if (num == "worst")
			nrow(state.hospitals)
		else {
			if (num > nrow(state.hospitals))
				NA
			else num
		}
		
		if (is.na(rank)) {
			hospital.names <- append(hospital.names, NA)
			states         <- append(states, state)
		} else {
			hospital.names <- append(hospital.names,
				state.hospitals[rank, "Hospital.Name"])
			
			states         <- append(states,
				state.hospitals[rank, "State"])
		}	
	}
	
	## Return a data frame with the hospital names and the
	## (abbreviated) state name
	
	result <- data.frame(hospital = hospital.names, state = states)
	rownames(result) <- result$state
	
	result
}