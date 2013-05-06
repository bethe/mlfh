### Machine Learning for Hackers - very first tutorial
### https://docs.google.com/file/d/0B4ZqvMfne52dSTk3R0tDTG1HZjA/edit

# load ggplot2
library(ggplot2)

# set working directory
setwd("~/mlfh/mlfh/mlfh/ch01")

# Load data (file at http://www.dropbox.com/s/xt4k3851)
ufo <- read.delim("ufo_awesome.tsv", sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")

# Inspect data
head(ufo, n=1)
summary(ufo)

# Add header row
names(ufo) <- c("DateOccurred", "DateReported", "Location", "ShortDescription", "Duration", "Long Description")

# Format Date Cells as Dates
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")

# Inspect cells that don't have the 8 character string date format of YYYYMMDD
head(ufo[which(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateOccurred)!=8),1])

# Isolate good rows
good.rows <- ifelse(nchar(ufo$DateOccurred)!=8 | nchar(ufo$DateOccurred)!=8, FALSE, TRUE)

# Check how many rows do not have proper date formats (725, though book says 371)
length(which(!good.rows))

# Ignore malformed rows and replace original dataset with remaining rows only (61,145 rows have proper format)
ufo <- ufo[good.rows,]

# Now apply proper date format
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

# tsv format is "City, State". Define function that splits up City and State in separate columns and identifies rows that do not conform
# - strsplit splits the string at the comma
# - tryCatch identifies error messages, so if no comma in location, it returns NA
# - gsub removes leading whitespace
# - length checks that all clean locations only have 2 characters for the state, in order to eliminate all but US UFO sightings

get.location <- function(l) {
	split.location <- tryCatch(strsplit(l,",")[[1]], error = function(e) return(c(NA,NA)))
	clean.location <- gsub("^ ", "", split.location)
	if (length(clean.location)>2) {
		return(c(NA,NA))
	}
	else {
		return(clean.location)
	}
}

# Create a new list with split & cleaned locations, applying the functions to column 'location'
city.state <- lapply(ufo$Location, get.location)

# Check formatting of location
head(city.state,n=2)

# Convert list into a two-column matrix
location.matrix <- do.call(rbind, city.state)

# add City and State columns to ufo from the newly created location.matrix; State abbreviations are made lower case for consistency
ufo <- transform(ufo, USCity = location.matrix[,1], USState = tolower(location.matrix[,2]), stringsAsFactors = FALSE)

# Remove foreign (i.e. Canadian) sightings from list by checking USState against a whitelist of states
# Create a vector of US States
us.states <-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il", "in","ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh", "nj","nm","nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt", "wa","wi","wv","wy")
# Match us.states against ufo$USStates and assign NA where there's no match to both States & Cities
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA

# Create subset of dataset with US sightings 
ufo.us <- subset(ufo, !is.na(USCity))

# Remove remaining entry with date (not happening in book instructions)
ufo.us <- subset(ufo.us, !is.na(DateOccurred))

# Check how many rows are left (51640)
nrow(ufo.us)

# Investigate summary statistics for UFO sightings dates
head(ufo.us, n=1)
summary(ufo.us$DateOccurred)
