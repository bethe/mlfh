### Machine Learning for Hackers - very first tutorial
### https://docs.google.com/file/d/0B4ZqvMfne52dSTk3R0tDTG1HZjA/edit

# load ggplot2
library(ggplot2)

# set working directory
setwd("~/mlfh/mlfh/mlfh/ch01")

# Load data (file at http://www.dropbox.com/s/xt4k3851)
ufo <- read.delim(, sep="\t", stringsAsFactors=FALSE, header=FALSE, na.strings="")
