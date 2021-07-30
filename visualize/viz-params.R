#*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@
#*@  Define global parameters for plotting  *@
#*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@*@


# define a minimum threshold of observations to keep a station
# if the number of observations at a station is lower than this number, the entire station is dropped
drop.station.N <- 5 

# alpha and point size
point.alpha <- 0.25 
point.size <- 0.5

# for binned distribution plots, define the maximum percentile threshold at which the last bin is cut for
# e.g. 0.9 cuts the last bin at the 90th percentile of the data in a binned distribution plot
my.bar.threshold <- 0.975 

# Do you want to save individual day plots when outputting sample day plotting?
# if FALSE, only outputs grouped plots by FRC for typical/extreme days
save.indv <- T # True or False

# Do you want to save wider day & week plots (better for widescreen resolutions in .pptx)
# if TRUE, save both wide and non-wide
save.wide <- T # True or False

# the number of times to iterate over the section to generate a random selection
# of daily and weekly actual vs predicted volume plots for typical and extreme days
# this does not produce 5 or a multiple of 5 plots. increase if more sample plots are desired
day.week.plot.iterations <- 5 

# for bar distribution plots that bin volumes, the number of bins to use
vol.cut.bins <- 10
