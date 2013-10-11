getmonitor <- function(id,directory,summarize=FALSE){
	
	if (as.numeric(id) < 10) {
		idBig=paste("00",as.character(id), sep="")
		}
	else if (as.numeric(id) <100) {
		idBig=paste("0",as.character(id), sep="")
		}
	else {
		idBig = as.character(id)
		}
	
	a <- read.csv(paste(".\\" , directory , "\\" , idBig, ".csv" , sep=""))

	if (summarize == TRUE) {
		print(summary(a))
	}

	a


}