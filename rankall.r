rankall <- function(outcome, num){

	
	oc <- read.csv("C:\\Coursera\\ComputingForDataAnalysis\\Assn3\\outcome-of-care-measures.csv")
	
	# if (!(state %in% stateslist)) {
		# stop("invalid state")
	# }
	
	
	
	if (!(outcome %in% outcomelist)) {
		stop("invalid outcome")
	}
	
	outcomenums <- c(11,17,23)
	outcomes <- cbind(outcomelist,outcomenums)
	oc.num <- as.numeric(outcomes[outcomelist==outcome,2])
	
	statelist <- sort(unique(oc[,7]))
	nstates <- length(statelist)
	
	oc[oc[,oc.num]=="Not Available",oc.num] <- NA
	oc[,oc.num] <- as.numeric(as.character(oc[,oc.num]))
			
	for (istate in 1:nstates){
		oc.state <- oc[oc$State==statelist[istate],]
		
		oc.ordered <- oc.state[order(oc.state[,oc.num],oc.state[,2],na.last=TRUE),][c(11,2)]
		
		oc.ct <- nrow(na.omit(oc.ordered[1]))
		
		if (num == "best") {
			hosp <- as.character(oc.ordered[1,2])
		} else if (num == "worst") {
			hosp <- as.character(oc.ordered[oc.ct,2])
		} else if (num <= oc.ct) {
			hosp <- as.character(oc.ordered[num,2])
		} 	else {
			hosp <- "NA"
		}
	
	
		z <- data.frame(hosp,statelist[istate])
		colnames(z) = c("hospital","state")

		if (istate > 1){
			z.multi <- rbind(z.multi,z)
		} else {
			z.multi <- data.frame(z)
		}
	}
	z.multi
}
