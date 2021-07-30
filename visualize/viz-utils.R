#~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%
#~%  utilities for plotting and cleaning data  #~%
#~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%~%

options(scipen = 999)

# function to load all packages, installing if needed
# source: https://gist.github.com/stevenworthington/3178163
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# NREL color theme
nrel.cols <- list("#0084c9", "#00a5db", "#666d70", "#d1cec6", "#191919", "#568e14", "#7fba00", "#ffc61e", "#d88c02", "#ffffff", "#c6414c")
names(nrel.cols) <- c("blue", "lightblue", "gray", "lightgray", "black", "green", "lightgreen", "yellow", "orange", "white", "red")

# custom spectral color ramp
spec.cols <- c(nrel.cols$blue, "#70c1b3", "#f3ffbd", "#ed9589", "#ff1654")
my.cols <- colorRampPalette(spec.cols)

# cut functions for discrete x axis with pretty labels
pretty.cut <- function(x){
  levels(x) <- gsub("\\,", " to ", gsub("\\(|\\)|\\[|\\]", "", levels(x)))
  levels(x) <- gsub(" to Inf", "+", levels(x))
  return(x)}

pretty.cut.pct <- function(x){
  #x <- factor(x) # preserve order
  levels(x) <- gsub("\\(|\\)|\\[|\\]", "",  levels(x))
  levels(x) <- gsub("\\(|\\)|\\[|\\]", "",  levels(x))
  levels(x) <- paste0(as.numeric(unlist(tstrsplit(levels(x), ",")[1])) * 100, "% to ", 
                      as.numeric(unlist(tstrsplit(levels(x), ",")[2])) * 100, "%")
  levels(x) <- gsub(" to Inf%", "+", levels(x))
  return(x)}

# pretty round the limit for fixed coordinates based on max of pred and actual volume
pretty.round <- function(x, ratio, sigs){signif(max(x, na.rm = T) * ratio, sigs)}

# pretty significant figures with proper formatting after rounding. e.g. 99.99 to 3 sigfig = "100." not "100"
signif.pretty <- function(x, dig = 2, add.comma = T, format.percent = F){
  # TODO option to drop hanging period in any case, or only when when rounding to dig ends in a multiple of 10.
  if(format.percent){x <- x * 100}
  x.c <- formatC(signif(x, digits = dig), digits = dig, format = "fg", flag = "#", big.mark = ifelse(add.comma, ",", ""))
  x.r <- formatC(signif(x, digits = dig), digits = dig, format = "fg", flag = "#")
  if(add.comma & !format.percent){ # comma no %
    ifelse(grepl("\\.$", x.r) &  (nchar(as.integer(x.r)) == dig), paste0(x.c, "."), x.c) 
  } else if(add.comma & format.percent){ # comma w/ %
    paste0(ifelse(grepl("\\.$", x.r) &  (nchar(as.integer(x.r)) == dig), paste0(x.c, "."), x.c), "%") 
  } else if(!add.comma & format.percent) { # no comma w/ %
    paste0(ifelse(grepl("\\.$", x.r) & !(nchar(as.integer(x.r)) == dig), gsub("\\.", "", x.r), x.r), "%") 
  } else { # no comma or %
    ifelse(grepl("\\.$", x.r) & !(nchar(as.integer(x.r)) == dig), gsub("\\.", "", x.r), x.r) 
  }
}
