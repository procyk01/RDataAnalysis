best <- function(state, outcome){

	# note = may have to calc %s, not just take them from the data.
	
	oc <- read.csv("C:\\Coursera\\ComputingForDataAnalysis\\Assn3\\outcome-of-care-measures.csv")
	
	
	if (!(state %in% stateslist)) {
		stop("invalid state")
	}
	
	if (!(outcome %in% outcomelist)) {
		stop("invalid outcome")
	}
	
	outcomenums <- c(11,17,23)
	#numtreated <- c(15,21,27)
	outcomes <- cbind(outcomelist,outcomenums,numtreated)
	
	oc.num <- as.numeric(outcomes[outcomelist==outcome,2])
	#oc.tr <- as.numeric(outcomes[outcomelist==outcome,3])
	
	oc[oc[,oc.num]=="Not Available",oc.num] <- NA
	oc[,oc.num] <- as.numeric(as.character(oc[,oc.num]))
	#oc[oc[,oc.tr]=="Not Available",oc.tr] <- NA
	#oc[,oc.tr] <- as.numeric(as.character(oc[,oc.tr]))
		
	oc.state <- oc[oc$State==state,]
	
	oc.ordered <- oc.state[order(oc.state[,oc.num],oc.state[,oc.num],na.last=TRUE),][c(11,2)]
	
	as.character(oc.ordered[1,2])
	
}
