rankhospital <- function(state, outcome, num = "best") {
	## Read outcome data
	outcomes <- read.csv("outcome-of-care-measures.csv", 
			     colClasses = "character")
	
	## Check that state and outcome are valid
	
	#validate input
	valid.outcomes <- c("heart attack", "heart failure", "pneumonia")
	if(!is.element(outcome, valid.outcomes)) 
		stop("invalid outcome")
	
	if(!is.element(state, unique(outcomes$State)))
		stop("invalid state")
	
	# Determine the col index
	index <- if(outcome == "heart attack")
		11
	else if (outcome == "heart failure")
		17
	else    23
	
	# Remove NA's
	outcomes[, index] <- as.numeric(outcomes[, index])
	outcomes <- outcomes[!is.na(outcomes[, index]), ]
	
	
	# Retreive all hospitals in State 'state'
	state.hospitals <- subset(outcomes, state == State)
	
	# Sort by rate, handle ties with hospital names
	sorted <- order(state.hospitals[, index], state.hospitals$Hospital.Name)
	state.hospitals <- state.hospitals[sorted, ]
	
	# Return the hospital's name according to it's rank (num)
	num <- if(num == "best")
		1
	else if (num == "worst")
		nrow(state.hospitals)
	else {
		if (num > nrow(state.hospitals))
			return (NA)
		else num
	}
	
	state.hospitals[num, 2]
}