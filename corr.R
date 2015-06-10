corr <- function(directory, threshold = 0) {
	## 'directory' is a character vector of length 1 indicating
	## the location of the CSV files

	## 'threshold' is a numeric vector of length 1 indicating the
	## number of completely observed observations (on all
	## variables) required to compute the correlation between
	## nitrate and sulfate; the default is 0

	## Return a numeric vector of correlations
	## NOTE: Do not round the result!
	
	id <- 1:332
	corrs <- rep(0,length(id))
	j <- 1
	for(i in id) {
		filename <- paste(i, '.csv', sep='')
		if(i<10) filename <- paste('00',filename, sep='')
		else if(i<100) filename <- paste('0', filename, sep='')
		currentdata <- read.csv(paste(directory,'/',filename,sep=''))
		if(sum(complete.cases(currentdata))>threshold) {
			corrs[j] <- cor(currentdata[,"nitrate"],currentdata[,"sulfate"],use="complete.obs")
			j <- j + 1
		}
	}
	return(corrs)
}

corrtest <- function() {
	print("TEST 1")
	cr <- corr("specdata", 150)
	print(head(cr))
	print(summary(cr))
	print("---------------------")
	print("TEST 2")
	cr <- corr("specdata", 400)
	print(head(cr))
	print(summary(cr))
	print("---------------------")
	print("TEST 3")
	cr <- corr("specdata", 5000)
	print(summary(cr))
	print(length(cr))
	print("---------------------")
	print("TEST 4")
	cr <- corr("specdata")
	print(summary(cr))
	print(length(cr))
	print("---------------------")
}
corrtest()
