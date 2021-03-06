pollutantmean <- function(directory, pollutant, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files
  
	## 'pollutant' is a character vector of length 1 indicating
	## the name of the pollutant for which we will calculate the
	## mean; either "sulfate" or "nitrate".
  
	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
  
	## Return the mean of the pollutant across all monitors list
	## in the 'id' vector (ignoring NA values)
	## NOTE: Do not round the result!
	
	# Solution:
	pollutants <- c() # Empty vector
	for(i in id){
		if(i<10) filename <- paste('00',i,'.csv', sep='')
		else if(i<100) filename <- paste('0',i,'.csv', sep='')
		else filename <- paste(i,'.csv', sep='')
		currentdata <- read.csv(paste(directory,'/',filename, sep=''))
		currentpollutant <- currentdata[, pollutant]
		currentpollutant <- currentpollutant[!is.na(currentpollutant)]
		pollutants <- c(pollutants, currentpollutant)
	}
	return(mean(pollutants))
}

# Testing function
testpollutantmean <- function() {
	test1 <- round(pollutantmean("specdata", "sulfate", 1:10), 3) == 4.064
	test2 <- round(pollutantmean("specdata", "nitrate", 70:72), 3) == 1.706
	test3 <- round(pollutantmean("specdata", "nitrate", 23), 3) == 1.281
	return (test1 & test2 & test3)
}
if(!testpollutantmean()) print("Test failed")
