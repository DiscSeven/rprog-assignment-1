complete <- function(directory, id = 1:332) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'id' is an integer vector indicating the monitor ID numbers
	## to be used
 
	## Return a data frame of the form:
	## id nobs
	## 1  117
	## 2  1041
	## ...
	## where 'id' is the monitor ID number and 'nobs' is the
	## number of complete cases
	nobs <- c()
	for(i in id) {
		filename <- paste(i, '.csv', sep='')
		if(i<10) filename <- paste('00', filename, sep='')
		else if(i<100) filename <- paste('0', filename, sep='')
		currentdata <- read.csv(paste(directory,'/',filename,sep=''))
		nobs <- c(nobs, sum(complete.cases(currentdata)))
	}
	return(data.frame(id=id, nobs=nobs))
}

testcomplete <- function() {
	print("Test1:")
	print(complete("specdata", 1))
	print("Test2:")
	print(complete("specdata", c(2, 4, 8, 10, 12)))
	print("Test3:")
	print(complete("specdata", 30:25))
	print("Test4:")
	print(complete("specdata", 3))
}
testcomplete()
