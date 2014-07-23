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
	outcomes.bystate <- split(outcomes, outcomes$State)
	
	sorted.outcomes  <- sapply(outcomes.bystate, function(x) {
		x <- x[order(x[, index], x$Hospital.Name), ]
		if (num == "best") {
			x[1, "Hospital.Name"]
		} else if (num == "worst") {
			x[nrow(x), "Hospital.Name"]
		} else {
			if (num > nrow(x))
				NA
			else
				x[num, "Hospital.Name"]
		}
	})
	result <- data.frame(hospital = sorted.outcomes)
	result$hospital <- as.character(result$hospital)
	result$state    <- rownames(result)
	
	result
}