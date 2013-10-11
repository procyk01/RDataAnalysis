corr <- function(directory, threshold=0){
	cor.coeffs <- numeric(0)
	
	for (fil in list.files(directory,pattern="*.csv")){
		a <- read.csv(paste(".\\" , directory , "\\" , fil, sep=""))
		a.fr <- data.frame(cbind(a$sulfate,a$nitrate))
		if (sum(complete.cases(a.fr)==TRUE) > threshold){
			cor.coeff <- cor(a.fr, use="complete.obs", method="pearson")
			cor.coeffs <- append(cor.coeffs,cor.coeff[2])
		}
	}
	cor.coeffs
}	
			
			
		
