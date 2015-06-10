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
	j <- 0
	for(i in id) {
		filename <- paste(i, '.csv', sep='')
		if(i<10) filename <- paste('00',filename, sep='')
		else if(i<100) filename <- paste('0', filename, sep='')
		currentdata <- read.csv(paste(directory,'/',filename,sep=''))
		if(sum(complete.cases(currentdata))>threshold) {
			j <- j + 1 # We want j to be the length after completion
			corrs[j] <- cor(currentdata[,"sulfate"],currentdata[,"nitrate"],use="complete.obs")
		}
	}
	result <- head(corrs,j)
	return(result)
}

corrtest <- function() {
	print("TEST 1")
	cr <- corr("specdata", 150)
	print("Goal:")
	print("[1] -0.01896 -0.14051 -0.04390 -0.06816 -0.12351 -0.07589")
	print(head(cr))
	print("Goal:")
	print("-0.2110 -0.0500  0.0946  0.1250  0.2680  0.7630")
	print(summary(cr))
	print("---------------------")
	print("TEST 2")
	cr <- corr("specdata", 400)
	print("Goal:")
	print("[1] -0.01896 -0.04390 -0.06816 -0.07589  0.76313 -0.15783")
	print(head(cr))
	print("Goal:")
	print("-0.1760 -0.0311  0.1000  0.1400  0.2680  0.7630")
	print(summary(cr))
	print("---------------------")
	print("TEST 3")
	cr <- corr("specdata", 5000)
	print("Goal:")
	print(" ")
	print(summary(cr))
	print("Goal:")
	print("[1] 0")
	print(length(cr))
	print("---------------------")
	print("TEST 4")
	cr <- corr("specdata")
	print("Goal:")
	print("-1.0000 -0.0528  0.1070  0.1370  0.2780  1.0000")
	print(summary(cr))
	print("Goal:")
	print("[1] 323")
	print(length(cr))
	print("---------------------")
}
corrtest()
