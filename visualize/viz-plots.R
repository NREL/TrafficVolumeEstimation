#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
#~#~#  PRESENTATION PLOTTING  #~#~#
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

# Built with: R version 4.0.4 (2021-02-15) -- "Lost Library Book"

if(!require("here")){install.packages("here")}
source(here::here("visualize/viz-utils.R")) # load custom functions & schemes
source(here::here("visualize/viz-params.R")) # load global parameters for plotting

# list packages
packages <- c("ggplot2", "scales", "grid", "gridExtra", "cowplot", "corrplot", "RColorBrewer", "lubridate", "data.table", "here")
ipak(packages) # loads all packages, installing if needed 

options(scipen = 999) # turn off scientific printing

dir.create(here("visualize/output"), showWarnings = F) # create output directory

# define input varibale classes to avoid conflicts if necessary
my.classes <- c(StationId = "character", 
                Dir = "character", 
                Snow = "numeric", 
                FC = "character", 
                Volume = "numeric",
                DayOfWeek = "character") 

# get data paths, create output folder, import all data binded together, move raw data to ouput folder
filenames <- list.files(here("visualize","input"), full.names = T) # get full path filenames
filenames <- filenames[grep("sample_input", filenames, invert = T)] # ignore sample file
regions <- gsub(".csv", "", basename(filenames)) # list of all regions based on filenames
lapply(regions, function(r) dir.create(here("visualize", "output", r), showWarnings = F)) # create output folders for figures and data
ml <- rbindlist(lapply(filenames, function(x) fread(x, colClasses = my.classes[which(names(my.classes) %in% colnames(fread(x)))])), fill = T, idcol = "id") 
lapply(seq_along(regions), function(f) file.copy(filenames[f], here("visualize", "output", regions[f]))) # copy raw data to output folder

# mark filename and region for each dataset in master file
ml[, filename := factor(id, labels = basename(filenames))]
ml[, Region := factor(id, labels = gsub(".csv", "", basename(filenames)))]

# mark test or train based on filename
ml[grepl("train", ignore.case = T, filename), Test := 0] 
ml[grepl("test", ignore.case = T, filename), Test := 1] 

# if PredVolume is absent, create placeholder
if( !("PredVolume" %in% colnames(ml)) ){
  ml[, PredVolume := NA_real_]
}

# TomTom FRC converted to HPMS and factorize FRC for plotting
ml[FRC > 6, FRC := 6] # FORCE all FRC greater than 6 to be "Local"
ml[, FRC := factor(FRC, levels = 0:6, labels = c("Interstate", "Freeways & Expessways", "Principal Arterial", "Minor Arterial", "Major Collector", "Minor Collector", "Local"))]
       
# check for duplicate observations
ml[, .N, by = .(Region, StationId, Dir, Date, Hour)][N > 1][order(-N)]

# force mean to eliminate duplicate observations to the mean volume 
ml[, `:=`(Volume = mean(Volume, na.rm = T), # as.integer(round(mean(Volume, na.rm = T)))
          PredVolume = mean(PredVolume, na.rm = T),
          PenRate = mean(PenRate, na.rm = T)), by = .(Region, StationId, Dir, Date, Hour)]
ml <- unique(ml)

# if Denver, adjust segment IDs to be neg/pos for direction only
ml[grepl("Denver|Colorado", Region, ignore.case = T), Dir := ifelse(grepl("-", Dir) == T, "-", "+")]

# fix pen rate around undefined
ml[ProbeCount == 0 & Volume == 0, PenRate := 0] # instead of NaN
ml[ProbeCount > 0 & Volume == 0, PenRate := NA] # instead of Inf

# drop stations with too few observations
low.N.stations <- ml[, .N, by = .(Region, FRC, StationId)]
low.N.stations[N < drop.station.N, .(dropped.stations = length(unique(StationId)),
                         dropped.obs = sum(N)), by = Region]
ml <- ml[!(StationId %in% low.N.stations[N < drop.station.N, StationId])]

# calc error metrics (EMFR, MAE, MAPE)
ml[, mf := max(Volume), by = c("StationId", "Region")] # max flow by station and region
ml[, ae := abs(PredVolume - Volume)] # error at observation level
ml[, emfr := ae / mf] # absolute error to max flow ratio at observation level
ml[, ape := ae / Volume] # absolute percentage error at observation level
ml[, mf := NULL] # don't need
ml[!is.finite(ape), ape := NA][!is.finite(ae), ae := NA][!is.finite(emfr), emfr := NA] # prevent non-finite issues and allow for na.rm = T 

# calc CDFs (cumulative w/in region)
ml[, ecdf.PenRate := ecdf(PenRate)(PenRate), by = Region] # ecdf pen rate
ml[, ecdf.count := ecdf(ProbeCount)(ProbeCount), by = Region] # ecdf probe count
ml[, ecdf.volume := ecdf(Volume)(Volume), by = Region] # ecdf probe count


#############
# summaries #
#############

ml[, .(#raw.mean.error = mean(PredVolume - Volume, na.rm = T),
  #sum.error = sum(PredVolume - Volume, na.rm = T),
  N.obs = comma(.N),
  N.stations = length(unique(StationId)),
  #N.test = comma(sum(Test, na.rm = T)),
  #N.train = comma(.N - sum(Test, na.rm = T)),
  R2_b = 1 - sum((Volume - PredVolume)^2) / sum((Volume - mean(Volume, na.rm = T))^2), 
  #R2_c = cor(PredVolume, Volume) ^ 2, 
  min.date = min(Date, na.rm = T),
  max.date = max(Date, na.rm = T),
  R2 = round(summary(lm(Volume ~ PredVolume))$r.squared, 2),
  mean.volume = comma(mean(Volume, na.rm = T)),
  MAE = round(mean(ae, na.rm = T)),
  MAPE = signif.pretty(mean(ape, na.rm = T), format.percent = T),
  WAPE = signif.pretty(weighted.mean(ape, w = Volume, na.rm = T), format.percent = T),
  EMFR = signif.pretty(weighted.mean(emfr, w = Volume, na.rm = T), format.percent = T),
  mean.probe = comma(mean(ProbeCount, na.rm = T)),
  mean.predicted = comma(mean(PredVolume, na.rm = T))), by = .(Region)]

###################
# by volume plots #
###################

# cut volume function
my.pretty.cut <- function(x, thresh = 0.975, bins = 10) {
  pretty.cut(cut(x, include.lowest = T, dig.lab = nchar(pretty.round(quantile(x, thresh, na.rm = T, names = F), 1.0, 1)), 
                 breaks = unique(c(signif(seq(0, pretty.round(quantile(x, thresh, na.rm = T, names = F), 1.0, 1), 
                                              pretty.round(quantile(x, thresh, na.rm = T, names = F), 1.0, 1)/bins), 2), Inf))))
}

for(r in regions){
  
  # out figure dir
  fig.out <- here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r)))
  dir.create(dirname(fig.out), showWarnings = F) # create figure sub directory for region r
  
  # MAE by volume bin
  ggplot(ml[Region == r, .(mae.by.vol.cut = mean(ae, na.rm = T)), by = .(Region, vol.cut = my.pretty.cut(Volume, bins = 30))]) +
    geom_bar(aes(y = mae.by.vol.cut, x = vol.cut), stat = "identity", fill = nrel.cols$blue, alpha = 0.95) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
    labs(x = "Hourly Volume (veh/hr)", y = "Mean Absolute Error", title = "Mean Absolute Error by Hourly Volume") +
    theme_minimal() +
    theme(text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title = element_text(color = nrel.cols$black),
          axis.text = element_text(color = nrel.cols$black),
          axis.text.x = element_text(color = nrel.cols$black, angle = 90, hjust = 1), 
          plot.margin = margin(2, 2, 2, 2, "mm"))
  ggsave(paste0(fig.out, "_mae_by_adt.png"), width = 7, height = 5, units = "in", scale = 0.9, dpi = 300)
  
  # MAPE by volume bin
  ggplot(ml[Region == r, .(mape.by.vol.cut = mean(ape, na.rm = T)), by = .(Region, vol.cut = my.pretty.cut(Volume, bins = 30))]) +
    geom_bar(aes(y = mape.by.vol.cut, x = vol.cut), stat = "identity", fill = nrel.cols$blue, alpha = 0.95) +
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .15))) +
    labs(x = "Hourly Volume (veh/hr)", y = "Mean Absolute Percentage Error", title = "Mean Absolute Percentage Error by Hourly Volume") +
    theme_minimal() +
    theme(text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title = element_text(color = nrel.cols$black),
          axis.text = element_text(color = nrel.cols$black),
          axis.text.x = element_text(color = nrel.cols$black, angle = 90, hjust = 1), 
          plot.margin = margin(2, 2, 2, 2, "mm"))
  ggsave(paste0(fig.out, "_mape_by_adt.png"), width = 7, height = 5, units = "in", scale = 0.9, dpi = 300)
}

####################################################################
# Predicted vs Actual scatter plots by region and functional class #
####################################################################

# summaries by FRC
frc.sum <- ml[, .(N = .N,
                  N.test = sum(Test),
                  N.station = length(unique(StationId)),
                  mean_pen = mean(PenRate, na.rm = T), 
                  mean_probe = round(mean(ProbeCount, na.rm = T), 0), 
                  mean_vol = round(mean(Volume, na.rm = T), 0),
                  med_vol = round(quantile(Volume, 0.5, na.rm = T), 0),
                  mean_emfr = mean(emfr, na.rm = T),
                  mean_mae = mean(ae, na.rm = T)), by = c("FRC","Region")]

# list of FRC-regions for plotting, exclude combos without results
frc.regions <- frc.sum[is.finite(mean_emfr) & is.finite(mean_mae)]

# derive hex plot parameters for clean plotting
hex.par <- ml[, .(m = signif(max(pmax(.SD, na.rm = T), na.rm = T), 2)), .SDcols = c("Volume", "PredVolume"), by = Region]
hex.par[((m/100) %% 2) == 1, m := m + 100] # create even 100ths for max
hex.par[, b := signif(m / 5, 1)] # break size
hex.par[, binwidth := m / 40] # bins = 40

# iterate for each region
for(r in regions){

  dir.create(here("visualize", "output", r), showWarnings = F) # create figure subdirectory for region r
    
    # determine the binsize for automatically getting the log scaling for hex plot
    l.m.r <- ml[Region == r, .(vol_cut = cut(Volume, seq(0, hex.par[Region == r, m], hex.par[Region == r, binwidth])),
                               pred_cut = cut(PredVolume, seq(0, hex.par[Region == r, m], 
                                                              hex.par[Region == r, binwidth])))][!is.na(vol_cut) & !is.na(pred_cut), 
                                                                                                 .N, by = .(vol_cut, pred_cut)][, ceiling(log(max(N)))]
    hex.par[Region == r, l.m := l.m.r] # assign auto hexplot bin size
    
    # pred vs actual all FRC each region
    pred.actual.frc <- ggplot(data = ml[Region == r,]) + 
      geom_point(aes(x = as.numeric(Volume), y = PredVolume, color = FRC), alpha = point.alpha, shape = 19, size = point.size, stroke = 0) + 
      geom_abline(intercept = 0, slope = 1, alpha = 0.4) + 
      scale_x_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      scale_y_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      coord_fixed(ratio = 1) + 
      labs(y = "Predicted", x = "Actual") + 
      scale_color_manual(values = my.cols(length(ml[Region == r, unique(FRC)]))) +
      theme_bw() +
      theme(legend.position = c(0.5, -0.31), 
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.background = element_blank(),
            legend.spacing.y = unit(0, "mm"),
            text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            axis.text = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(0, 0, 12, 0), "mm")) +
      guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), nrow = 2))
    
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_frc.png")), pred.actual.frc, 
    #       width = 2.8, height = 3, units = "in", scale = 1, dpi = 300)
    
    # pred vs actual all (backup dot)
    pred.actual.frc.log <- ggplot(data = ml[Region == r & Volume != 0 & PredVolume != 0,]) + 
      geom_point(aes(x = as.numeric(Volume), y = PredVolume, color = FRC), alpha = point.alpha, shape = 19, size = point.size, stroke = 0) + 
      geom_abline(intercept = 0, slope = 1, alpha = 0.4) + 
      scale_x_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      scale_y_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      annotation_logticks(short = unit(.5,"mm"), mid = unit(1,"mm"), long = unit(2,"mm"), alpha = 0.4) +
      coord_fixed(ratio = 1) + 
      labs(y = "Predicted", x = "Actual") + 
      scale_color_manual(values = my.cols(length(ml[Region == r, unique(FRC)]))) +
      theme_bw() +
      theme(legend.position = c(0.5, -0.31), 
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.background = element_blank(),
            legend.spacing.y = unit(0, "mm"),
            text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            axis.text = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(0, 0, 12, 0), "mm")) +
      guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), nrow = 2))
    
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_frc_log.png")), pred.actual.frc.log, 
    #       width = 2.8, height = 3, units = "in", scale = 1, dpi = 300)
    
    # pred vs actual all (backup dot log)
    pred.actual.log <- ggplot(data = ml[Region == r & Volume != 0 & PredVolume != 0,]) + 
      geom_abline(intercept = 0, slope = 1, alpha = 0.4) + 
      geom_point(aes(x = as.numeric(Volume), y = PredVolume), alpha = 0.4, shape = 19, size = point.size, stroke = 0) + 
      scale_x_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      scale_y_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      annotation_logticks(short = unit(.5,"mm"), mid = unit(1,"mm"), long = unit(2,"mm"), alpha = 0.4) +
      coord_fixed(ratio = 1) + 
      labs(y = "Predicted", x = "Actual") + 
      #scale_color_manual(values = my.cols(length(ml[Region == r, unique(FRC)]))) +
      theme_bw() +
      theme(legend.position = c(0.5, -0.31), 
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.background = element_blank(),
            legend.spacing.y = unit(0, "mm"),
            text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            axis.text = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(0, 0, 12, 0), "mm"))
    
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_log.png")), pred.actual.log, 
    #       width = 2.8, height = 3, units = "in", scale = 1, dpi = 300)
    
    # pred vs actual all (backup dot)
    pred.actual <- ggplot(data = ml[Region == r,]) + 
      geom_abline(intercept = 0, slope = 1, alpha = 0.4) + 
      geom_point(aes(x = as.numeric(Volume), y = PredVolume), alpha = 0.4, shape = 19, size = point.size, stroke = 0) + 
      scale_x_continuous (expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      scale_y_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2))) + 
      coord_fixed(ratio = 1) + 
      labs(y = "Predicted", x = "Actual") + 
      #scale_color_manual(values = my.cols(length(ml[Region == r, unique(FRC)]))) +
      theme_bw() +
      theme(legend.position = c(0.5, -0.31), 
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.background = element_blank(),
            legend.spacing.y = unit(0, "mm"),
            text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            axis.text = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(0, 0, 12, 0), "mm"))
    
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual.png")), pred.actual, 
    #       width = 2.8, height = 3, units = "in", scale = 1, dpi = 300)
    
    #####
    
    # facet pred vs actual all
    
    # determine number of facet columns
    if(length(frc.regions[Region == r, FRC]) <= 3){
      col.f <- length(frc.regions[Region == r, FRC])
      
    } else if (length(frc.regions[Region == r, FRC]) == 4){
      col.f <- 2
      
    } else {
      col.f <- 3
    }
    
    # create lableing for N
    max.v <- pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2)
    n.labs <- data.table(v = rep(max.v * 0.875, length(frc.regions[Region == r, FRC])),
                         p = rep(max.v * 0.040, length(frc.regions[Region == r, FRC])),
                         FRC = unique(ml[Region == r, FRC]),
                         lab = paste("n =", prettyNum(ml[Region == r, .N, by = FRC][, N], big.mark = ",")))
    
    pred.actual.frc.facet <- ggplot(data = ml[Region == r,]) + 
      geom_point(aes(x = as.numeric(Volume), y = PredVolume), color = nrel.cols$blue, alpha = point.alpha, 
                 shape = 19, size = point.size, stroke = 0, show.legend = F) + 
      geom_abline(intercept = 0, slope = 1, alpha = 0.4) + 
      geom_text(data = n.labs, size = 2.5, aes(x = v, y = p, label = lab)) +
      scale_x_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2)), labels = comma) + 
      scale_y_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2)), labels = comma) + 
      coord_fixed(ratio = 1) + 
      facet_wrap(~FRC, ncol = col.f) +
      labs(y = "Predicted", x = "Actual") + 
      #scale_color_manual(values = c(nrel.cols$red, nrel.cols$blue, nrel.cols$green, nrel.cols$orange, nrel.cols$yellow)) +
      theme_bw() +
      theme(legend.position = c(0.5, -0.31), 
            legend.direction = "horizontal",
            legend.title = element_blank(),
            legend.text = element_text(size = 8),
            legend.background = element_blank(),
            legend.spacing.y = unit(0, "mm"),
            text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            axis.text = element_text(color = nrel.cols$black, size = 8),
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            plot.margin = unit(c(2, 2, 0, 0), "mm"))
    #guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), ncol = col.f))
    
    w.x <- ifelse(length(frc.regions[Region == r, FRC]) > 4, ceiling(length(frc.regions[Region == r, FRC]) / 3), 1)
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_frc_facet.png")), pred.actual.frc.facet, 
           width = 2.8 * col.f, height = 3 * w.x, units = "in", scale = 1, dpi = 300)
    
    
    # pred vs actual for each FRC and region
    for(f in frc.regions[Region == r, FRC]){
      
      pred.actual.frc.f <- ggplot(data = ml[Region == r & FRC == f,]) + 
        geom_point(aes(x = as.numeric(Volume), y = PredVolume), color = nrel.cols$blue, alpha = point.alpha, shape = 19, size = point.size, stroke = 0) + 
        geom_abline(intercept = 0, slope = 1, color = nrel.cols$gray, alpha = 0.75) + 
        annotate("text", size = 2.5,
                 x = pretty.round(ml[Region == r & FRC == f, .(Volume, PredVolume)], 1.05, 2) * 0.85,
                 y = pretty.round(ml[Region == r & FRC == f, .(Volume, PredVolume)], 1.05, 2) * 0.04, 
                 label = paste("n =", prettyNum(ml[Region == r & FRC == f, .N], big.mark = ","))) +
        scale_x_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r & FRC == f, .(Volume, PredVolume)], 1.05, 2))) + 
        scale_y_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r & FRC == f, .(Volume, PredVolume)], 1.05, 2))) + 
        coord_fixed(ratio = 1) + 
        labs(title = paste(f), y = "Predicted", x = "Actual") + 
        theme_bw() +
        theme(text = element_text(color = nrel.cols$black), 
              title = element_text(color = nrel.cols$black),
              axis.text = element_text(color = nrel.cols$black),
              plot.title = element_text(hjust = 0.5, vjust = 0.5, size = 10),
              plot.margin = unit(c(2, 5, 2, 2), "mm")) +
        guides(colour = guide_legend(override.aes = list(size = 2, alpha = 1), nrow = 2))
      
      ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_", gsub(" ", "_", f), "_pred_actual.png")), 
             pred.actual.frc.f, width = 2.8, height = 3, units = "in", scale = 1, dpi = 300)
      
    }
    
    ## pred vs actual all points one hexplot
    
    hex.breaks <- exp(seq(0, hex.par[Region == r, l.m], by = length(0:hex.par[Region == r, l.m]) / 5)) 
    hex.breaks.l <- unlist(lapply(hex.breaks, function(x) round(pretty.round(x, 1, 2)))) 
    # hex log scale labels will be count rounded to (intenger length - 1 char) sig fig
    
    pred.actual.all <- ggplot(data = ml[Region == r,]) + 
      stat_binhex(aes(x = Volume, y = PredVolume, fill = ..count.., alpha = log(..density..) ), bins = 40) + #  
      geom_abline(intercept = 0, slope = 1, alpha = 0.225) + 
      scale_x_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(Volume)], 1.05, 2)),
                         breaks = seq(0, hex.par[Region == r, m], hex.par[Region == r, b]), labels = comma) + 
      scale_y_continuous(expand = c(0,0), limits = c(0, pretty.round(ml[Region == r, .(PredVolume)], 1.05, 2)),
                         breaks = seq(0, hex.par[Region == r, m], hex.par[Region == r, b]), labels = comma) + 
      #scale_fill_gradientn("count", colours = my.cols(11)[3:11], limits = c(0, hex.par[Region == r, l.m])) +
      scale_fill_gradientn("Observations",  colours = my.cols(11)[3:11], 
                           trans = "log", breaks = hex.breaks, labels = comma(hex.breaks.l, accuracy = 1)) +
      scale_alpha(range = c(0.6, 1), guide = "none") + #
      annotate("text", size = 2.5,
               x = pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2) * 0.825,
               y = pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2) * 0.04, 
               label = paste("n =", comma(ml[Region == r, .N], accuracy = 1))) +
      labs(y = "Predicted Volume (veh/hr)", x = "Actual Volume (veh/hr)", title = "") + 
      theme_bw() +
      theme(legend.position = c(0.45, -0.225), 
            legend.key.height = unit(2, "mm"),
            legend.key.width = unit(7, "mm"),
            legend.title = element_text(size = 7.5, vjust = 0.75),
            legend.direction = "horizontal",
            legend.text = element_text(size = 7.5),
            legend.background = element_blank(),
            legend.spacing.y = unit(1, "mm"),
            axis.title.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")),
            axis.title = element_text(size = 9),
            plot.margin = unit(c(0, 5, 7.5, 2), "mm"),  # t r b l
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            axis.text = element_text(color = nrel.cols$black, size = 8),
            title = element_text(color = nrel.cols$black),
            text = element_text(color = nrel.cols$black, size = 9))
    # guides(alpha = guide_legend(nrow = 1))
    
    assign(paste0("pred.actual.all.", gsub(" ", "_", r)), pred.actual.all)
    
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_all_hex.png")), pred.actual.all,  
           width = 3, height = 3, units = "in", scale = 1, dpi = 600)
    
    # LOG X Y hex plot all
    hex.breaks <- exp(seq(0, hex.par[Region == r, l.m], by = length(0:hex.par[Region == r, l.m]) / 8)) 
    hex.breaks.l <- unlist(lapply(hex.breaks, function(x) if(nchar(floor(x)) == 1){round(pretty.round(round(x, 0), 1, 2))} else {round(pretty.round(x, 1, 2))})) 
    
    # hex log scale labels will be count rounded to (intenger length - 1 char) sig fig
    
    pred.actual.all.log <- ggplot(data = ml[Region == r & Volume != 0 & PredVolume != 0,]) + 
      stat_binhex(aes(x = Volume, y = PredVolume, fill = ..count.., alpha = log(..density..) ), bins = 40) + #  
      geom_abline(intercept = 0, slope = 1, alpha = 0.225) + 
      scale_x_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(Volume)], 1.05, 2)),
                    #breaks = seq(0, hex.par[Region == r, m], hex.par[Region == r, b]), 
                    labels = comma) + 
      scale_y_log10(expand = c(0,0), limits = c(1, pretty.round(ml[Region == r, .(PredVolume)], 1.05, 2)),
                    #breaks = seq(0, hex.par[Region == r, m], hex.par[Region == r, b]), 
                    labels = comma) + 
      #scale_fill_gradientn("count", colours = my.cols(11)[3:11], limits = c(0, hex.par[Region == r, l.m])) +
      scale_fill_gradientn("Observations",  colours = my.cols(11)[3:11], 
                           trans = "log", breaks = hex.breaks, labels = comma(hex.breaks.l, accuracy = 1)) +
      scale_alpha(range = c(0.6, 1), guide = "none") + #
      annotation_logticks(short = unit(.5,"mm"), mid = unit(1,"mm"), long = unit(2,"mm"), alpha = 0.4) +
      annotate("text", size = 2.5,
               x = pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2) * 0.25,
               y = 1.75, #pretty.round(ml[Region == r, .(Volume, PredVolume)], 1.05, 2) * 0.01, 
               label = paste("n =", comma(ml[Region == r, .N], accuracy = 1))) +
      labs(y = "Predicted Volume (veh/hr)", x = "Actual Volume (veh/hr)", title = "") + 
      theme_bw() +
      theme(legend.position = c(0.45, -0.225), 
            legend.key.height = unit(2, "mm"),
            legend.key.width = unit(7, "mm"),
            legend.title = element_text(size = 7.5, vjust = 0.75),
            legend.direction = "horizontal",
            legend.text = element_text(size = 6),
            legend.background = element_blank(),
            legend.spacing.y = unit(1, "mm"),
            axis.title.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")),
            axis.title = element_text(size = 9),
            plot.margin = unit(c(0, 5, 7.5, 1), "mm"),  # t r b l
            plot.title = element_text(hjust = 0.5, vjust = 0.5),
            axis.text = element_text(color = nrel.cols$black, size = 8),
            title = element_text(color = nrel.cols$black),
            text = element_text(color = nrel.cols$black, size = 9)) 
    #guides(alpha = guide_legend(nrow = 1))
    
    #assign(paste0("pred.actual.all.log.", gsub(" ", "_", r)), pred.actual.all.log)
    
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_pred_actual_all_hex_log.png")), pred.actual.all.log,  
           width = 3, height = 3, units = "in", scale = 1, dpi = 600)
    
    
    
    ################################################
    # summary plots by functional class and region #
    ################################################
    
    # mean pen rate, probe count, and EMFR by functional class
    
    # factorize FRC label with N = ..
    frc.sum.r <- frc.sum[Region == r]
    frc.labs.l <- levels(ml[,FRC])[levels(ml[,FRC]) %in% frc.sum.r[, FRC]]
    frc.sum.r[, FRC.f := factor(FRC, levels = frc.labs.l)] # create factor to keep road hiarchery
    frc.sum.r[, FRC.lab := paste0(gsub(" ", "\n", gsub(" \\&", "/", FRC.f)), "\n(N = ", N.station, ")")] # create FRC label with N
    frc.sum.r[, FRC.lab.f := factor(FRC.lab, levels = frc.sum.r[order(FRC.f), FRC.lab])]  # order FRC with N by ordered levels
    
    # custom label sizing for n observations by frc, dependent on number of frc and length of characters in max observations 
    n.size <- frc.sum.r[, 5 / (length(unique(FRC)) / 4) / (max(nchar(N)) / 3)]
    #frc.sum[, 5 / (length(unique(FRC)) / 4) / (max(nchar(N)) / 3), by = Region]
    
    # probe penetration rate by functional class
    pen.rate.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_pen), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_pen - max(mean_pen)/30, x = FRC.lab.f, label = paste0(signif(frc.sum[Region == r, mean_pen]*100, 3), "%")), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_pen)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Probe Penetration Rate") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    
    # mean probe volume by functional class
    mean.probe.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_probe), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(expand = c(0,1)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_probe - max(mean_probe)/30, x = FRC.lab.f, label = round(frc.sum[Region == r, mean_probe], 0)), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_probe)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Volume (veh/hr)") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    # mean station volume by functional class
    mean.vol.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_vol), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(expand = c(0,1)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_vol - max(mean_vol)/30, x = FRC.lab.f, label = round(frc.sum[Region == r, mean_vol], 0)), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_vol)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Volume (veh/hr)") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    # median station volume by functional class
    med.vol.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = med_vol), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(expand = c(0,1)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = med_vol - max(med_vol)/30, x = FRC.lab.f, label = round(frc.sum[Region == r, med_vol], 0)), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(med_vol)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Median Volume (veh/hr)") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    # error to max flow ratio by functional class
    emfr.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_emfr), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(labels = percent_format(accuracy = 1), expand = c(0,0)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_emfr - max(mean_emfr)/30, x = FRC.lab.f, label = paste0(signif(frc.sum[Region == r, mean_emfr]*100, 3), "%")), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_emfr)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Error to Max Flow Ratio") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    # mean average error by functional class
    mae.fc <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_mae), fill = nrel.cols$blue, stat = "identity") + 
      scale_y_continuous(expand = c(0,0)) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_mae - max(mean_mae)/30, x = FRC.lab.f, label = round(frc.sum[Region == r, mean_mae], 0)), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_mae)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Absolute Error") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.title = element_text(hjust = 0.5),
            axis.text = element_text(color = nrel.cols$black),
            axis.title.x = element_text(vjust = 0))
    
    # save
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_cnt_fc.png")), mean.probe.fc, width = 5, height = 5, units = "in", scale = 0.9, dpi = 300)
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_pen_fc.png")), pen.rate.fc, width = 5, height = 5, units = "in", scale = 0.9, dpi = 300)
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_mean_volume_fc.png")), mean.vol.fc, width = 5, height = 5, units = "in", scale = 0.9, dpi = 300)
    #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_med_volume_fc.png")), med.vol.fc, width = 4, height = 5, units = "in", scale = 0.9, dpi = 300)
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_emfr_fc.png")), emfr.fc, width = 5, height = 5, units = "in", scale = 0.9, dpi = 300)
    ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_mae_fc.png")), mae.fc, width = 5, height = 5, units = "in", scale = 0.9, dpi = 300)
    
    
    # hex plus side by side vol and prob pen for paper
    
    # probe penetration rate by functional class
    pen.rate.fc.sbs <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_pen), fill = nrel.cols$blue, stat = "identity", alpha = 0.95) + 
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.05))) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_pen - max(mean_pen)/35, x = FRC.lab.f, label = paste0(signif(frc.sum[Region == r, mean_pen]*100, 3), "%")), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_pen)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = 2, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Probe Penetration Rate") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.margin = unit(c(5, 1, 1, 1), "mm"),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")),
            axis.text.x = element_text(size = 5),
            axis.text = element_text(color = nrel.cols$black))
    
    # mean station volume by functional class
    mean.vol.fc.sbs <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_vol), fill = nrel.cols$blue, stat = "identity", alpha = 0.95) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_vol - max(mean_vol)/35, x = FRC.lab.f, label = round(frc.sum[Region == r, mean_vol], 0)), 
                size = n.size, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_vol)/30, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = 2, stat = "identity", color = nrel.cols$white) + 
      labs(x = "Functional Class", y = "Mean Volume (veh/hr)") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black), 
            title = element_text(color = nrel.cols$black),
            plot.margin = unit(c(5, 6, 1, 1), "mm"),
            plot.title = element_text(hjust = 0.5),
            axis.title.x = element_text(margin = unit(c(1, 0, 0, 0), "mm")),
            axis.text.x = element_text(size = 5),
            axis.text = element_text(color = nrel.cols$black))
    
    # error to max flow ratio by functional class
    emfr.fc.sbs <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_emfr), fill = nrel.cols$blue, stat = "identity", alpha = 0.95) + 
      scale_y_continuous(labels = percent, expand = expansion(mult = c(0, 0.05))) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_emfr - max(mean_emfr)/20, x = FRC.lab.f, label = paste0(signif(frc.sum[Region == r, mean_emfr]*100, 2), "%")), 
                size = 2.65, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_emfr)/20, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = 1.75, stat = "identity", color = nrel.cols$white) + 
      labs(x = NULL, y = "EMFR") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black, size = 9.5),
            axis.text.x = element_text(size = 5),
            plot.margin = unit(c(3, 2, 0, 3.9), "mm"),
            title = element_text(color = nrel.cols$black),
            plot.title = element_blank(),
            axis.text = element_text(color = nrel.cols$black))
    
    # mean average error by functional class
    mae.fc.sbs <- ggplot(frc.sum.r) +
      geom_bar(aes(x = FRC.lab.f, y = mean_mae), fill = nrel.cols$blue, stat = "identity", alpha = 0.95) + 
      scale_y_continuous(expand = expansion(mult = c(0, 0.05)), labels = comma) + 
      scale_x_discrete(expand = c(0,0)) + 
      geom_text(aes(y = mean_mae - max(mean_mae)/20, x = FRC.lab.f, label = round(frc.sum[Region == r, mean_mae], 0)), 
                size = 2.65, stat = "identity", color = nrel.cols$white) + 
      geom_text(aes(y = max(mean_mae)/20, x = FRC.lab.f, label = paste0("n = ", prettyNum(frc.sum[Region == r, N], big.mark = ","))), 
                size = 1.75, stat = "identity", color = nrel.cols$white) + 
      labs(x = NULL, y = "MAE") + 
      theme_minimal() +
      theme(text = element_text(color = nrel.cols$black, size = 9.5),
            axis.text.x = element_text(size = 5),
            plot.margin = unit(c(3, 2, 0, 6), "mm"),
            title = element_text(color = nrel.cols$black),
            plot.title = element_blank(),
            axis.text = element_text(color = nrel.cols$black))
    
    
    # mean vol and pen side-by-side
    combo.plot.1 <- plot_grid(mean.vol.fc.sbs, pen.rate.fc.sbs, labels = c("(a)", "(b)"), label_size = 11, 
                              nrow = 1, ncol = 2)
    
    # mae and emfr side-by-side
    sbs.plot.2 <- plot_grid(mae.fc.sbs, emfr.fc.sbs, labels = c("(b)", "(c)"), rel_widths = c(1, 1), rel_heights = c(1, 1), ncol = 1, label_size = 10.5)
    combo.plot.2 <- plot_grid(pred.actual.all + ggtitle("") + theme(plot.margin = unit(c(1.4, 0.5, 8, 0), "mm")), sbs.plot.2, labels = c("(a)", ""), label_size = 11, rel_widths = c(1, 1), rel_heights = c(1, 1), ncol = 2, nrow = 1)
    
    # save side-by-side plots
    ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_combo_mean_vol_pen_fc.png")), combo.plot.1, 
            width = 5.5, height = 2.75, units = "in", scale = 1.2, dpi = 300)
    ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r), "_combo_pred_actual_and_bar.png")), combo.plot.2, 
            width = 5.6, height = 3, units = "in", scale = 1, dpi = 300)
}


#####################################
# traffic volume distribution plots #
#####################################

dist.max <- ml[, .(m = signif(quantile(Volume, 0.98, na.rm = T), 2)), by = Region]
dist.max[, b := signif(m / 33, 1)]

for(r in regions){

  probe.volume.dist <- ggplot(ml[Region == r,]) +  
    stat_bin(aes(x = Volume, y = ..count../sum(..count..)), boundary = 0, closed = "left", fill = nrel.cols$blue, binwidth = dist.max[Region == r, b]) + 
    scale_y_continuous(labels = percent, expand = expansion(mult = c(0, .15))) +
    scale_x_continuous(expand = expansion(mult = c(0.005, 0.005)), limits = c(0, dist.max[, m])) +
    labs(x = "Volume (veh/hr)", y = "Frequency") +
    theme(text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.title = element_text(color = nrel.cols$black),
          axis.text = element_text(color = nrel.cols$black),
          plot.margin = unit(c(3, 3, 0, 0), "mm"))
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_distrib.png")), probe.volume.dist, 
         width = 7, height = 4, units = "in", scale = 0.7, dpi = 300)
}


##########################################
# Volume distribution and EMFR bar plots #
##########################################

# all cities comparison plots of EMFR, MAE, MAPE by Volume, probe counts, and probe pentration rates
# calc summaries and rescale secondary axis CDFs on relevant cut variables
# create plots that show all next to each other and each plot individually
# cut relavant variables (probe count, probe pen rate, and station volumes) for plotting

# define percentile thresholds for cut off by region and metric
bar.thresholds <- data.table(expand.grid(Region = unique(ml[, Region]), metric = c("Volume", "ProbeCount", "PenRate")))
bar.thresholds[, t := my.bar.threshold] # cdf/percentile max 

for(r in regions){
  #tryCatch({ # catch erros and warnings to print while looping
  
  # thresholds
  vol.t <- bar.thresholds[Region == r & metric == "Volume", t]
  cnt.t <- bar.thresholds[Region == r & metric == "ProbeCount", t]
  pen.t <- bar.thresholds[Region == r & metric == "PenRate", t]
  
  # filter to region and cut data into factors 
  # breaks leave last bin on right open to infinity "+"
  bar.ml <- ml[Region == r,]
  bar.ml[, pen_cut_to_max := pretty.cut.pct(cut(PenRate, include.lowest = T,
                                                breaks = unique(c(round(seq(0, pretty.round(quantile(PenRate, pen.t, na.rm = T, names = F), 1.0, 1), 
                                                             pretty.round(quantile(PenRate, pen.t, na.rm = T, names = F), 1.0, 1)/10), 3), Inf))))]
  
  bar.ml[, count_cut_to_max := pretty.cut(cut(ProbeCount, include.lowest = T, dig.lab = 4, 
                                              breaks = unique(c(signif(seq(0, pretty.round(quantile(ProbeCount, cnt.t, na.rm = T, names = F), 1.0, 1), 
                                                           pretty.round(quantile(ProbeCount, cnt.t, na.rm = T, names = F), 1.0, 1)/10), 2), Inf))))]
  
  bar.ml[, volume_cut_to_max := pretty.cut(cut(Volume, include.lowest = T, dig.lab = 4, 
                                               breaks = unique(c(signif(seq(0, pretty.round(quantile(Volume, vol.t, na.rm = T, names = F), 1.0, 1), 
                                                              pretty.round(quantile(Volume, vol.t, na.rm = T, names = F), 1.0, 1)/10), 2), Inf))))]
  
  # volume
  ml.vol.cut <- bar.ml[, .(N = as.numeric(.N),
                           mean_emfr_cut = mean(emfr, na.rm = T),
                           mean_mae_cut = mean(ae, na.rm = T),
                           mean_mape_cut = mean(ape, na.rm = T),
                           mean_pen_cut = mean(PenRate, na.rm = T),
                           ecdf.volume = max(ecdf.volume, na.rm = T)), by = volume_cut_to_max]
  ml.vol.cut[, freq := N/sum(N)] # do this before removing NA values so we can get total frequency but still cut extremes
  ml.vol.cut <- ml.vol.cut[!is.na(volume_cut_to_max) & !is.na(mean_mae_cut)]
  ml.vol.cut[, ecdf.emfr.vol := ml.vol.cut[, max(mean_emfr_cut, na.rm = T)] * (ecdf.volume)]
  ml.vol.cut[, ecdf.mae.vol := ml.vol.cut[, max(mean_mae_cut, na.rm = T)] * (ecdf.volume)]
  ml.vol.cut[, ecdf.mape.vol := ml.vol.cut[, max(mean_mape_cut, na.rm = T)] * (ecdf.volume)]
  ml.vol.cut[, ecdf.pen.vol := ml.vol.cut[, max(mean_pen_cut, na.rm = T)] * (ecdf.volume)]
  ml.vol.cut[, ecdf.emfr.vol.r := max(mean_emfr_cut, na.rm = T) * (ecdf.volume)] # within region for indiv plot
  ml.vol.cut[, freq.emfr.vol.r := max(mean_emfr_cut, na.rm = T) * (freq)] # within region for indiv plot

  n.vol.size <- (10 / ml.vol.cut[, .N]) * (3 / max(nchar(ml.vol.cut[, N]))) * 4
  
  volume.EMFR <- ggplot(ml.vol.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = volume_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 1, show.legend = F) + 
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .1))) +
    labs(x = "Station Volume (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = volume_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.vol.cut[, N], big.mark = ","))), 
              size = n.vol.size, stat = "identity", color = nrel.cols$white) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.text.y.left = element_text(color = nrel.cols$black))
  
  volume.pen <- ggplot(ml.vol.cut) + 
    geom_bar(aes(y = mean_pen_cut, x = volume_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 1, show.legend = F) + 
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .1))) +
    labs(x = "Station Volume (veh/hr)", y = "Probe Penetration Rate") +
    geom_text(aes(y = max(mean_pen_cut)/30, x = volume_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.vol.cut[, N], big.mark = ","))), 
              size = n.vol.size, stat = "identity", color = nrel.cols$white) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.text.y.left = element_text(color = nrel.cols$black))
  
  volume.mae <- ggplot(ml.vol.cut) + 
    geom_bar(aes(y = mean_mae_cut, x = volume_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 1, show.legend = F) + 
    scale_y_continuous(labels = comma, expand = expansion(mult = c(0, .1))) +
    labs(x = "Station Volume (veh/hr)", y = "Mean Absolute Error") +
    geom_text(aes(y = max(mean_mae_cut)/30, x = volume_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.vol.cut[, N], big.mark = ","))), 
              size = n.vol.size, stat = "identity", color = nrel.cols$white) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.text.y.left = element_text(color = nrel.cols$black))
  
  volume.EMFR.cdf <- ggplot(ml.vol.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = volume_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 0.7, show.legend = F) + 
    geom_line(aes(y = ecdf.emfr.vol.r, x = volume_cut_to_max, group = 1), stat = "identity", color = nrel.cols$gray) + 
    geom_text(aes(y = ecdf.emfr.vol.r + max(mean_emfr_cut)/30, x = volume_cut_to_max, group = 1), color = nrel.cols$gray,
              label = paste0(signif(ml.vol.cut[, ecdf.volume]*100, 2), "%"), 
              size = n.vol.size, stat = "identity", hjust = -0.05) + 
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       sec.axis = sec_axis(~./max(ml.vol.cut[, ecdf.emfr.vol.r]), 
                                           labels = percent_format(accuracy = 1), name = "CDF")) +
    labs(x = "Station Volume (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = volume_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.vol.cut[, N], big.mark = ","))), 
              size = n.vol.size, stat = "identity", color = nrel.cols$white) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.title.y.right = element_text(color = nrel.cols$gray),
          axis.text.y.left = element_text(color = nrel.cols$black),
          axis.text.y.right = element_text(color = nrel.cols$gray))
  
  volume.EMFR.pdf <- ggplot(ml.vol.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = volume_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 0.7, show.legend = F) + 
    geom_line(aes(y = freq.emfr.vol.r, x = volume_cut_to_max, group = 1), stat = "identity", color = nrel.cols$gray) + 
    geom_text(aes(y = freq.emfr.vol.r + max(mean_emfr_cut)/30, x = volume_cut_to_max, group = 1), color = nrel.cols$gray,
              label = paste0(signif(ml.vol.cut[, freq]*100, 2), "%"), 
              size = 2, stat = "identity", hjust = -0.05) + 
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       sec.axis = sec_axis(~./max(ml.vol.cut[, mean_emfr_cut]), 
                                           labels = percent_format(accuracy = 1), name = "PDF")) +
    labs(x = "Station Volume (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = volume_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.vol.cut[, N], big.mark = ","))), 
              size = 2, stat = "identity", color = nrel.cols$white) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.title.y.right = element_text(color = nrel.cols$gray),
          axis.text.y.left = element_text(color = nrel.cols$black),
          axis.text.y.right = element_text(color = nrel.cols$gray))
  
  # probe count 
  ml.cnt.cut <- bar.ml[, .(N = as.numeric(.N),
                           mean_emfr_cut = mean(emfr, na.rm = T),
                           mean_mae_cut = mean(ae, na.rm = T),
                           mean_mape_cut = mean(ape, na.rm = T),
                           ecdf.count = max(ecdf.count, na.rm = T)), by = count_cut_to_max]
  ml.cnt.cut[, freq := N/sum(N)] # do this before removing NA values so we can get total frequency but still cut extremes
  ml.cnt.cut <- ml.cnt.cut[!is.na(count_cut_to_max) & !is.na(mean_mae_cut)]
  ml.cnt.cut[, ecdf.emfr.count := ml.cnt.cut[,max(mean_emfr_cut, na.rm = T)] * (ecdf.count)]
  ml.cnt.cut[, ecdf.mae.count := ml.cnt.cut[,max(mean_mae_cut, na.rm = T)] * (ecdf.count)]
  ml.cnt.cut[, ecdf.mape.count := ml.cnt.cut[,max(mean_mape_cut, na.rm = T)] * (ecdf.count)]
  ml.cnt.cut[, ecdf.emfr.cnt.r := max(mean_emfr_cut, na.rm = T) * (ecdf.count)] # within region for indiv plot
  ml.cnt.cut[, freq.emfr.cnt.r := max(mean_emfr_cut, na.rm = T) * (freq)] # within region for indiv plot
  
  n.cnt.size <- (10 / ml.cnt.cut[, .N]) * (3 / max(nchar(ml.cnt.cut[, N]))) * 4
  
  probe.count.EMFR <- ggplot(ml.cnt.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = count_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 1, show.legend = F) + 
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .1))) +
    labs(x = "Probe Count (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = count_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.cnt.cut[, N], big.mark = ","))), 
              size = n.cnt.size, stat = "identity", color = nrel.cols$white) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          plot.title = element_text(hjust = 0.5),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.text.y.left = element_text(color = nrel.cols$black))
  
  probe.count.EMFR.cdf <- ggplot(ml.cnt.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = count_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 0.7, show.legend = F) + 
    geom_line(aes(y = ecdf.emfr.cnt.r, x = count_cut_to_max, group = 1), stat = "identity", color = nrel.cols$gray) + 
    geom_text(aes(y = ecdf.emfr.cnt.r + max(mean_emfr_cut)/30, x = count_cut_to_max, group = 1), color = nrel.cols$gray,
              label = paste0(signif(ml.cnt.cut[, ecdf.count]*100, 2), "%"), 
              size = n.cnt.size, stat = "identity", hjust = -0.05) + 
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       sec.axis = sec_axis(~./max(ml.cnt.cut[, ecdf.emfr.cnt.r]), 
                                           labels = percent_format(accuracy = 1), name = "CDF")) +
    labs(x = "Probe Count (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = count_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.cnt.cut[, N], big.mark = ","))), 
              size = n.cnt.size, stat = "identity", color = nrel.cols$white) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.title.y.right = element_text(color = nrel.cols$gray),
          axis.text.y.left = element_text(color = nrel.cols$black),
          axis.text.y.right = element_text(color = nrel.cols$gray))
  
  probe.count.EMFR.pdf <- ggplot(ml.cnt.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = count_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 0.7, show.legend = F) + 
    geom_line(aes(y = freq.emfr.cnt.r, x = count_cut_to_max, group = 1), stat = "identity", color = nrel.cols$gray) + 
    geom_text(aes(y = freq.emfr.cnt.r + max(mean_emfr_cut)/30, x = count_cut_to_max, group = 1), color = nrel.cols$gray,
              label = paste0(signif(ml.cnt.cut[, freq]*100, 2), "%"), 
              size = n.cnt.size, stat = "identity", hjust = -0.05) + 
    scale_y_continuous(labels = percent_format(accuracy = 1),
                       sec.axis = sec_axis(~./max(ml.cnt.cut[, mean_emfr_cut]), 
                                           labels = percent_format(accuracy = 1), name = "PDF")) +
    labs(x = "Probe Count (veh/hr)", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = count_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.cnt.cut[, N], big.mark = ","))), 
              size = n.cnt.size, stat = "identity", color = nrel.cols$white) + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.title.y.right = element_text(color = nrel.cols$gray),
          axis.text.y.left = element_text(color = nrel.cols$black),
          axis.text.y.right = element_text(color = nrel.cols$gray))
  
  # PenRate
  ml.pen.cut <- bar.ml[, .(N = as.numeric(.N),
                           mean_emfr_cut = mean(emfr, na.rm = T), 
                           mean_mae_cut = mean(ae, na.rm = T),
                           mean_mape_cut = mean(ape, na.rm = T),
                           ecdf.PenRate = max(ecdf.PenRate, na.rm = T)), by = pen_cut_to_max]
  ml.pen.cut[, freq := N/sum(N)] # do this before removing NA values so we can get total frequency but still cut extremes
  ml.pen.cut <- ml.pen.cut[!is.na(pen_cut_to_max) & !is.na(mean_mae_cut)]
  ml.pen.cut[, ecdf.emfr.PenRate := ml.pen.cut[, max(mean_emfr_cut, na.rm = T)] * (ecdf.PenRate)]
  ml.pen.cut[, ecdf.mae.PenRate := ml.pen.cut[, max(mean_mae_cut, na.rm = T)] * (ecdf.PenRate)]
  ml.pen.cut[, ecdf.mape.PenRate := ml.pen.cut[, max(mean_mape_cut, na.rm = T)] * (ecdf.PenRate)]
  ml.pen.cut[, ecdf.emfr.PenRate.r := max(mean_emfr_cut, na.rm = T) * (ecdf.PenRate)] # within region for indiv plot
  
  n.pen.size <- (10 / ml.pen.cut[, .N]) * (3 / max(nchar(ml.pen.cut[, N]))) * 3
  
  probe.pen.EMFR <- ggplot(ml.pen.cut) + 
    geom_bar(aes(y = mean_emfr_cut, x = pen_cut_to_max), fill = nrel.cols$blue, stat = "identity", alpha = 1, show.legend = F) + 
    scale_y_continuous(labels = percent_format(accuracy = 1), expand = expansion(mult = c(0, .1))) +
    labs(x = "Probe Penetration Rate", y = "Error to Max Flow Ratio") +
    geom_text(aes(y = max(mean_emfr_cut)/30, x = pen_cut_to_max, 
                  label = paste0("n = ", prettyNum(ml.pen.cut[, N], big.mark = ","))), 
              size = n.pen.size, stat = "identity", color = nrel.cols$white) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, color = nrel.cols$black),
          text = element_text(color = nrel.cols$black), 
          title = element_text(color = nrel.cols$black),
          plot.title = element_text(hjust = 0.5),
          axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
          axis.title.y.left = element_text(color = nrel.cols$black),
          axis.title.y.right = element_text(color = nrel.cols$gray),
          axis.text.y.left = element_text(color = nrel.cols$black),
          axis.text.y.right = element_text(color = nrel.cols$gray))
  
  # save
  #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_EMFR_CDF.png")), 
  #       volume.EMFR.cdf, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_count_EMFR_CDF.png")), 
  #       probe.count.EMFR.cdf, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_EMFR_PDF.png")), 
  #       volume.EMFR.pdf, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  #ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_count_EMFR_PDF.png")), 
  #       probe.count.EMFR.pdf, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_EMFR.png")), 
         volume.EMFR, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_pen.png")), 
         volume.pen, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_station_volume_mae.png")), 
         volume.mae, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_count_EMFR.png")), 
         probe.count.EMFR, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
  
  ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_probe_pen_EMFR.png")), 
         probe.pen.EMFR, width = 7, height = 4, units = "in", scale = 1, dpi = 300)
}


##############################
# time-of-day pred vs actual #
##############################

ml[, Date := as.IDate(Date)] # convert to integer based date
ml[, week := week(Date)]
ml[, date.time := ymd_h(paste(Date, Hour))]

# create day and week plot sub folders

lapply(regions, function(x) dir.create(here("visualize", "output", gsub(" ", "_", x), "day_plots"), showWarnings = F))
lapply(regions, function(x) dir.create(here("visualize", "output", gsub(" ", "_", x), "week_plots"), showWarnings = F))

for(x in 1:day.week.plot.iterations){ # repeat a few times to get a few options

    # summarize data and filter based on desireable characteristics for plottings
    sample.days.raw <- ml[, .(N = .N, 
                              mae = mean(ae, na.rm = T),
                              emfr = mean(emfr, na.rm = T),
                              mape = mean(ape, na.rm = T),
                              m_pen = mean(PenRate, na.rm = T),
                              m_vol = mean(Volume, na.rm = T),
                              max_vol = max(Volume, na.rm = T)),
                          , by = c("Region","FRC","Date","StationId","Dir")] # by date
    
    # filtering
    sample.days.raw <- sample.days.raw[is.finite(get(c("mae","emfr","mape"))),] # remove non finitie
    
    #sample.days.raw <- sample.days.raw[N >= 24,] # full days only
    sample.days.raw[, q.N := quantile(N, 0.9, na.rm = T), by = Region] # 90th percentile observations in a day by region
    sample.days.raw <- sample.days.raw[N >= q.N, ] # only 90th percentile and up observations in a week
    
    sample.days.raw <- sample.days.raw[!is.na(Date),] # has date
    sample.days.raw[, wkday := 0][!(weekdays(Date) %in% c("Saturday", "Sunday")), wkday := 1] # weekday variable
    sample.days.raw[, q.emfr := ecdf(emfr)(emfr), by = c("Region","FRC","StationId","Dir")] # quantile of EMFR
    sample.days.raw[, q.mae := ecdf(mae)(mae), by = c("Region","FRC","StationId","Dir")] # quantile of MAE
    
    # filter to representative (near median) cases and the most extreme cases using
    # use EMFR & MAE percentile ranks within region, FRC, station, and direction to find best dates to plot
    typical.days <- unique(sample.days.raw[(q.emfr > 0.45 & q.emfr < 0.55 & q.mae > 0.45 & q.mae < 0.55), .SD[sample(1:.N, 1)], by = .(Region, FRC, wkday)][wkday == 1])
    extreme.days <- unique(sample.days.raw[(q.emfr < 0.05 | q.emfr > 0.95) & (q.mae < 0.05 | q.mae > 0.95), .SD[sample(1:.N, 1)], by = .(Region, FRC, wkday)])
    
    # combine sample days
    sample.days <- setkey(rbind(cbind(typical.days, extreme = 0), cbind(extreme.days, extreme = 1))[wkday == 1], Region, FRC, Date, max_vol, m_vol)
    #sample.days <- setkey(sample.days.raw[StationId == "701"][, extreme := 0][(q.emfr < 0.05 | q.emfr > 0.95) & (q.mae < 0.05 | q.mae > 0.95), extreme := 1], Region, FRC, Date, max_vol, m_vol)
    
    # iterate plotting all desired days
    for(i in 1:sample.days[,.N]){   # 
      
      # define filtering params for plotting 
      r <- sample.days[i, Region]
      f <- sample.days[i, FRC]
      s <- sample.days[i, StationId]
      g <- sample.days[i, Dir]
      d <- sample.days[i, Date]
      
      # if any of the filtering variables are NA, skip
      if(any(is.na(c(r,f,s,g,d))) == T){next}
      
      # add hour 24 as next day hour zero???
      #(Region == r & FRC == f & Date == as.Date(d) + days(1) & StationId == s & Dir == g & Hour == 0
      day.plot <- ggplot(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g]) +
        geom_line(aes(x = Hour, y = PredVolume, color = "XGBoost", linetype = "XGBoost"), size = 1) +
        geom_line(aes(x = Hour, y = Volume, color = "Observed", linetype = "Observed"), size = 1) +
        #annotate("text", x = 3, label = paste("MAE =", sample.days[i, signif(mae, 3)]), size = 3,
        #         y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume,PredVolume)]) * 0.7) + 
        #annotate("text", x = 3, label = paste0("EMFR = ", 100 * sample.days[i, signif(emfr, 2)],"%"), size = 3,
        #         y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume,PredVolume)]) * 0.6) + 
        scale_y_continuous(expand = expansion(mult = c(0, .10))) + 
        scale_x_continuous(expand = c(0,0), breaks = seq(0, 23, 3), limits = c(0, 23)) + 
        scale_color_manual(name = "", values = c(nrel.cols$blue, nrel.cols$red)) + 
        scale_linetype_manual(name = "", values = c("solid", "32")) + 
        labs(title = paste(f), subtitle = paste("Station", s, g,"on",d), y = "Volume (veh/hr)", x = "Hour") + 
        theme_bw() +
        theme(legend.position = c(0.115,0.9),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.key.width = unit(9, "mm"),
              text = element_text(color = nrel.cols$black), 
              title = element_text(color = nrel.cols$black),
              axis.text = element_text(color = nrel.cols$black),
              plot.subtitle = element_text(hjust = 0.5, vjust = 0, size = 7),
              plot.title = element_text(hjust = 0.5, vjust = 0, size = 12),
              plot.margin = unit(c(2, 2, 2, 2), "mm"))
      
      day.plot.name <- paste(gsub(" ", "_", r), 
                             gsub(" ", "_", f), 
                             gsub(" ", "_", s), 
                             gsub(" ", "_", g), 
                             gsub(" ", "_", d),
                             sep = "_")
      
      # save the plot in the envir for creating 4 panel plot
      if(sample.days[i, extreme] == 0){
        typical.day.plot <- day.plot + ggtitle("") + theme(legend.position = "none", plot.subtitle = element_text(size = 8.5))  + 
          annotate("text", x = 2.6, label = paste("MAE =", sample.days[i, signif(mae, 3)]), size = 3,
                   y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume, PredVolume)]) * 1.05) + 
          annotate("text", x = 20, label = paste0("EMFR = ", 100 * sample.days[i, signif(emfr, 2)],"%"), size = 3,
                   y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume, PredVolume)]) * 1.05)
        
        typical.obj.name <- gsub("-", "_", day.plot.name) # name and assign
        assign(typical.obj.name, typical.day.plot) 
        sample.days[r == Region & f == FRC & s == StationId & g == Dir & d == Date, typical.obj := typical.obj.name] 
        
      } else {
        
        extreme.day.plot <- day.plot + ggtitle("") + theme(legend.position = "none", plot.subtitle = element_text(size = 8.5))  + 
          annotate("text", x = 2.6, label = paste("MAE =", sample.days[i, signif(mae, 3)]), size = 3,
                   y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume, PredVolume)]) * 1.05) + 
          annotate("text", x = 20, label = paste0("EMFR = ", 100 * sample.days[i, signif(emfr, 2)],"%"), size = 3,
                   y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume, PredVolume)]) * 1.05)
        
        extreme.obj.name <- gsub("-", "_", day.plot.name) # name and assign
        assign(extreme.obj.name, extreme.day.plot) 
        sample.days[r == Region & f == FRC & s == StationId & g == Dir & d == Date, extreme.obj := extreme.obj.name] 
      }
      
      # add annotations for single day plottings
      day.plot <- (day.plot + 
                     annotate("text", x = 3, label = paste("MAE =", sample.days[i, signif(mae, 3)]), size = 3,
                              y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume,PredVolume)]) * 0.7) + 
                     annotate("text", x = 3, label = paste0("EMFR = ", 100 * sample.days[i, signif(emfr, 2)],"%"), size = 3,
                              y = max(ml[Region == r & FRC == f & Date == d & StationId == s & Dir == g, .(Volume,PredVolume)]) * 0.6)) 
      
      # save individual plots
      if(save.indv == T){
        ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/day_plots/", day.plot.name,".png")), 
               day.plot, width = 5, height = 3, units = "in", scale = 1, dpi = 300)
      }
      
    }
    
    # designate which FRC to drop in case there is more than 4
    keep.FRC <- rep(list(c("Local", "Major Collector", "Minor Arterial", "Principal Arterial")), length(ml[, unique(Region)]))
    names(keep.FRC) <- ml[, unique(Region)]
    
    ## 4 tiled plots
    for(r in names(keep.FRC)){
      
      # get dropped FRCs
      if(sample.days[Region == r & extreme == 0, .N,] > 4){
        drop.FRC.typical <- as.character(sample.days[Region == r & !(FRC %in% unname(unlist(keep.FRC[r]))), unique(FRC)])
      } else {drop.FRC.typical <- ""}
      if(sample.days[Region == r & extreme == 1, .N,] > 4){
        drop.FRC.extreme <- as.character(sample.days[Region == r & !(FRC %in% unname(unlist(keep.FRC[r]))), unique(FRC)])
      } else {drop.FRC.extreme <- ""}
      
      plot.list <- sample.days[Region == r & (!(FRC %in% drop.FRC.extreme) | !(FRC %in% drop.FRC.extreme)), .(typical.obj, extreme.obj, FRC)]
      
      if(plot.list[!is.na(typical.obj), .N] > 0){
        nc.4.pred.act.typical <- plot_grid(plotlist = mget(plot.list[!is.na(typical.obj), typical.obj]),
                                           labels = paste0("(", letters[1:4], ") ", plot.list[!is.na(typical.obj), FRC]),
                                           ncol = 2, nrow = 2, label_size = 11)
        nc.4.pred.act.typical <- plot_grid(nc.4.pred.act.typical, # add legend
                                           get_legend(day.plot + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))), 
                                           ncol = 1, nrow = 2, rel_heights = c(1, 0.05))
        
        file.name.typical <- paste0(gsub(" ", "", r), "_typical",
                                    #"_drop", gsub(" ", "", strtrim(drop.FRC.typical, 8)), # unique filename w/ station ids and dates
                                    "_dates", paste0(gsub("-", "", substr(sample.days[Region == r & extreme == 0 & !(FRC %in% drop.FRC.typical), Date], 5, 10)), collapse = "-")#,
                                    #"_ids", paste0(sample.days[Region == r & extreme == 0 & !(FRC %in% drop.FRC.typical), StationId], collapse = "")
        )
        
        ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/day_plots/", file.name.typical, ".png")), nc.4.pred.act.typical, 
                width = 5.5, height = 4, units = "in", scale = 1.4, dpi = 300)
        if(save.wide == T){
          ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/day_plots/", file.name.typical, "_wide.png")), nc.4.pred.act.typical, 
                  width = 8, height = 4, units = "in", scale = 1.4, dpi = 300)
        }
      }
      
      if(plot.list[!is.na(extreme.obj), .N] > 0){
        nc.4.pred.act.extreme <- plot_grid(plotlist = mget(plot.list[!is.na(extreme.obj), extreme.obj]), 
                                           labels = paste0("(", letters[1:4], ") ", plot.list[!is.na(extreme.obj), FRC]),
                                           ncol = 2, nrow = 2, label_size = 11) #
        nc.4.pred.act.extreme <- plot_grid(nc.4.pred.act.extreme, # add legend
                                           get_legend(day.plot + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom", legend.text = element_text(size = 12))), 
                                           ncol = 1, nrow = 2, rel_heights = c(1, 0.05))
        
        file.name.extreme <- paste0(gsub(" ", "", r), "_extreme",
                                    #"_drop", gsub(" ", "", strtrim(drop.FRC.extreme, 8)), # unique filename w/ station ids and dates
                                    "_dates", paste0(gsub("-", "", substr(sample.days[Region == r & extreme == 1 & !(FRC %in% drop.FRC.extreme), Date], 5, 10)), collapse = "")#,
                                    #"_ids", paste0(sample.days[Region == r & extreme == 1 & !(FRC %in% drop.FRC.extreme), StationId], collapse = "")
        )
        
        ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/day_plots/", file.name.extreme, ".png")), nc.4.pred.act.extreme, 
                width = 5.5, height = 4, units = "in", scale = 1.4, dpi = 300)
        if(save.wide == T){
          ggsave2(here(paste0("visualize/output/", gsub(" ", "_", r),"/day_plots/", file.name.extreme, "_wide.png")), nc.4.pred.act.extreme, 
                  width = 8, height = 4, units = "in", scale = 1.4, dpi = 300)
        }
      }
    }
    
    ##############
    # Week plots #
    ##############
    
    sample.weeks.raw <- ml[, .(N = .N,
                               mae = mean(ae, na.rm = T),
                               emfr = mean(emfr, na.rm = T),
                               mape = mean(ape, na.rm = T),
                               m_pen = mean(PenRate, na.rm = T),
                               m_vol = mean(Volume, na.rm = T),
                               max_vol = max(Volume, na.rm = T)),
                           , by = c("Region","FRC","week","StationId","Dir")] # by week
    
    # filtering
    sample.weeks.raw <- sample.weeks.raw[is.finite(get(c("mae","emfr","mape"))),] # remove non finite
    
    #sample.weeks.raw <- sample.weeks.raw[N >= 168,] # only full weeks 
    sample.weeks.raw[, q.N := quantile(N, 0.9, na.rm = T), by = Region] # 90th percentile observations in a week by region
    sample.weeks.raw <- sample.weeks.raw[N >= q.N, ] # only 90th percentile and up observations in a week
    
    sample.weeks.raw <- sample.weeks.raw[!is.na(week),] # has week
    sample.weeks.raw[, q.emfr := ecdf(emfr)(emfr), by = c("Region","FRC","StationId","Dir")] # quantile of EMFR
    sample.weeks.raw[, q.mae := ecdf(mae)(mae), by = c("Region","FRC","StationId","Dir")] # quantile of MAE
    
    # typical and extreme weeks
    typical.weeks <- unique(sample.weeks.raw[(q.emfr > 0.45 & q.emfr < 0.55 & q.mae > 0.45 & q.mae < 0.55), .SD[sample(1:.N, 1)], by = .(Region, FRC)])
    extreme.weeks <- unique(sample.weeks.raw[(q.emfr < 0.05 | q.emfr > 0.95) & (q.mae < 0.05 | q.mae > 0.95), .SD[sample(1:.N, 1)], by = .(Region, FRC)])
    
    # select 
    sample.weeks <- setkey(rbind(cbind(typical.weeks, extreme = 0), cbind(extreme.weeks, extreme = 1)), Region, FRC, week, max_vol, m_vol)
    
    for(i in 1:sample.weeks[,.N]){
      r <- sample.weeks[i, Region]
      f <- sample.weeks[i, FRC]
      s <- sample.weeks[i, StationId]
      g <- sample.weeks[i, Dir]
      w <- sample.weeks[i, week]
      
      # if any of the filtering variables are NA, skip
      if(any(is.na(c(r,f,s,g,w))) == T){next}
      
      # starting day
      min.d <- ml[Region == r & FRC == f & week == w & StationId == s & Dir == g, min(date.time)]
      
      week.plot <- ggplot(ml[Region == r & FRC == f & week == w & StationId == s & Dir == g]) +
        geom_line(aes(x = date.time, y = PredVolume, color = "XGBoost", linetype = "XGBoost"), size = 0.7) +
        geom_line(aes(x = date.time, y = Volume, color = "Observed", linetype = "Observed"), size = 0.7) +
        scale_y_continuous(expand = c(0,0), limits = c(0, max(ml[Region == r & FRC == f & week == w & StationId == s & Dir == g, .(Volume,PredVolume)])*1.05)) + #expansion(mult = c(0.01, .05))) +
        scale_x_datetime(date_breaks = "1 day", date_labels = "%a", expand = c(0,0)) +
        scale_color_manual(name = "", values = c(nrel.cols$blue, nrel.cols$red)) + 
        scale_linetype_manual(name = "", values = c("solid", "22")) + 
        labs(title = paste(f), subtitle = paste("Station", s, g, "for week of", as.Date(min.d)), y = "Volume (veh/hr)",
             tag = paste0("MAE = ", sample.weeks[i, signif(mae, 3)], "       EMFR = ", 100 * sample.weeks[i, signif(emfr, 3)],"%")) + 
        theme_bw() +
        theme(legend.position = c(0.225,-0.17),
              legend.background = element_blank(),
              legend.key = element_blank(),
              legend.key.width = unit(9, "mm"),
              legend.direction = "horizontal",
              text = element_text(color = nrel.cols$black), 
              title = element_text(color = nrel.cols$black),
              axis.text = element_text(color = nrel.cols$black),
              #axis.text.x = element_text(hjust = -0.25, size = 7),
              axis.title.x = element_blank(),
              plot.tag = element_text(size = 9),
              plot.tag.position = c(0.775,-0.055),
              panel.border = element_rect(size = 0.25),
              plot.subtitle = element_text(hjust = 0.5, vjust = 0, size = 7),
              plot.title = element_text(hjust = 0.5, vjust = 0, size = 12),
              plot.margin = unit(c(2, 2, 10, 2), "mm"))
      
      week.plot.name <- paste(gsub(" ", "_", r),
                              "week", w, 
                              ifelse(sample.weeks[i, extreme] == 0, "typical", "extreme"),
                              gsub(" ", "_", f), 
                              gsub(" ", "_", s), 
                              gsub(" ", "_", g), 
                              sep = "_")
      
      ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/week_plots/", week.plot.name,".png")), 
             week.plot, width = 5, height = 3, units = "in", scale = 1, dpi = 300)
      if(save.wide == T){
        ggsave(here(paste0("visualize/output/", gsub(" ", "_", r),"/week_plots/", week.plot.name,"_wide.png")), 
               week.plot, width = 10, height = 2.75, units = "in", scale = 1, dpi = 300)
      }
    }
}


###########################################
# correlation plots on relevant variables #
###########################################

# define desired columns to plot correlations
cor.cols <- c("Temp", "WindSp", "Precip", "Snow", "Long", "Lat", "NumberOfLanes", 
              "SpeedLimit", "Road Class", "Month", "Hour", "AvgSp", "ProbeCount", "Volume")

names(cor.cols) <-  c("Temperature", "Wind Speed", "Precipitation", "Snow", "Longitude", "Latitude", "Number of Lanes", 
                      "Speed Limit", "Road Class", "Month", "Hour", "Average Speed", "Probe Count", "Station Volume")

# custom color palette
my.cols.r <- colorRampPalette(rev(spec.cols)) # reverse: red to blue spectral
cor.colors <- c(my.cols.r(8)[1:4], "#ffffff", my.cols.r(8)[5:8])
cor.pal <- colorRampPalette(cor.colors)

for(r in regions){
  #tryCatch({
  
  data.r <- ml[Region == r,] # subset to region
  no.obs.r <- names(which(unlist(data.r[, lapply(.SD, function(x) all(x == 0 | is.na(x) | is.infinite(x)))]))) # no finite obs
  cor.cols.r <- colnames(data.r)[colnames(data.r) %in% cor.cols & !(colnames(data.r) %in% no.obs.r)] # keep what available desired columns
  data.r[, `Road Class` := FRC] # name FRC as "Road Class"
  new.cor.names <- names(cor.cols[cor.cols %in% cor.cols.r]) # renamed columns
  data.r[, (new.cor.names) := lapply(.SD, as.numeric), .SDcols = cor.cols.r] # force numeric for correlation
  
  # plot and save
  cor.r <- cor(data.r[, ..new.cor.names])
  diag(cor.r) <- NA # remove diagonal
  png(here(paste0("visualize/output/", gsub(" ", "_", r),"/", gsub(" ", "_", r),"_variable_corrplot.png")), width = 720, height = 720 * 1.2, res = 100)
  print(corrplot(cor.r, 
                 type = "lower", 
                 tl.col = nrel.cols$black,
                 outline = "transparent", 
                 na.label = "1", 
                 na.label.col = nrel.cols$blue,
                 col = cor.pal(10)
                 ))
  dev.off()
}

# once everything has generated, save processed data and delete input folder raw data
lapply(regions, function(x) fwrite(ml[Region == x], paste0(here("visualize", "output", x), "/", x, "_cleaned.csv"))) 
lapply(filenames[basename(filenames) != "test.csv"], file.remove) # don't remove test file if exists, used for testing
