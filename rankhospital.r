rankhospital <- function(state, outcome, num){

	
	oc <- read.csv("C:\\Coursera\\ComputingForDataAnalysis\\Assn3\\outcome-of-care-measures.csv")
	
	if (!(state %in% stateslist)) {
		stop("invalid state")
	}
	
	if (!(outcome %in% outcomelist)) {
		stop("invalid outcome")
	}
	
	outcomenums <- c(11,17,23)
	outcomes <- cbind(outcomelist,outcomenums,numtreated)
	
	oc.num <- as.numeric(outcomes[outcomelist==outcome,2])
	
	oc[oc[,oc.num]=="Not Available",oc.num] <- NA
	oc[,oc.num] <- as.numeric(as.character(oc[,oc.num]))
		
	oc.state <- oc[oc$State==state,]
	
	oc.ordered <- oc.state[order(oc.state[,oc.num],oc.state[,2],na.last=TRUE),][c(11,2)]
	
	oc.ct <- nrow(na.omit(oc.ordered[1]))
	
	if (num == "best") {
		as.character(oc.ordered[1,2])
	} else if (num == "worst") {
		as.character(oc.ordered[oc.ct,2])
	} else if (num <= oc.ct) {
		as.character(oc.ordered[num,2])
	} 	else {
		"NA"
	}
}
