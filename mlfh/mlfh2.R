# Download file from http://www.dropbox.com/s/c49jl0vi6bksbwh/01_heights_weights_genders.csv

# Load data and get summary for one column (heights)
data.file  <- file.path('01_heights_weights_genders.csv')
heights.weights  <- read.csv(data.file, header = TRUE, sep = ',')
heights <- with(heights.weights, Height)
summary(heights)

# Write my own mean function
my.mean <- function(x) {
	return(sum(x) / length(x))
}

# Test my.mean function
my.mean(heights)
mean(heights)

# Write my own median function
# Median essentially is the value of the "middle" observation. Process:
# (1) Sort vector
# (2a) For an even number of observations, index the two middle values
# (2b) For an uneven number of observations, index the middle value
# (3a) For an even number of observations, return the average value of the two observations indexed
# (3b) For an even number of observations, return the value of the indexed observation
my.median <- function(x) {
	sorted.x <- sort(x)

	if (length(x) %% 2 == 0)
	{
		indices  <- c(length(x) / 2, length(x) /2 +1)
		return(mean(sorted.x[indices]))
	}
	else
	{
		index <- ceiling(length(x) / 2)
		return(sorted.x[index])
	}
}

# Test my.median
my.median(heights)
median(heights)

# min / max
max(heights)
min(heights)
range(heights)

# get quantiles
quantile(heights, probs = seq(0, 1, by = 0.20))
