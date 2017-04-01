# Name: Yanli Xu
# Date: Nov 28th, 2016
# Note: This script is used for plotting time vs zone_1_nectrig, time vs zone_2_nectrig, time vs zone_3_nectrig, time vs zone_nearPV_nectrig, for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("perZone_time_vs_nectrig.R"), the script will start working 
# input file: XXXX-XX-XX-XXXX-XXXX_perZone_nectrig.csv

# The following is an example of running this script
# > source("perZone_time_vs_nectrig.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
# Please choose one zone to plot, your choices are Z3, Z2, Z1 and nearPV: Z3
# Please input the cycleLimit of these experiments: 36000
# Please input bottom and upper limits of y-axis to represent the range of Z3 nectrig value: 0, 4500
# You already got the plot, please input x and y coordinates to put legends: 0, 4000

perZone_time_vs_nec <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_perZone_nectrig.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("nectrig_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  par(mfrow=c(1, 1))
  
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # ask user choose one zone from zone1, zone2, zone3, zone_nearPV
  zone_str <- readline("Please choose one zone to plot, your choices are Z3, Z2, Z1 and nearPV: ")
  zone <- unlist(strsplit(zone_str, "[, ]+"))
  
  # ask user to input the run time
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
#   # predct the max y value from the max concentration
#   exp <- eval(parse(text = exp_names[length(files)]))
#   y_max <- max(exp[, c(paste("Cumu_Nectrig_", zone, sep = ""))], na.rm = TRUE) *1.5
  
  y_lim_str <- readline(paste("Please input bottom and upper limits of y-axis to represent the range of ", zone, " nectrig value: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  title <- paste("Cumu_Nectrig_", zone, " for different concentrations", sep = "")
  plot(c(), c(), main = title, xlab = "Time", ylab = "nectrig", ylim = y_limits, xlim = c(0, run_time))
  for (i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    y_value <- exp[, c(paste("Cumu_Nectrig_", zone, sep = ""))]
    x_value <- exp[, c(paste("Time_", zone, sep = ""))]
    lines(y_value ~ x_value, col = color[i])
  }
  legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
  legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
  legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
  legend(legend_x, legend_y, as.character(conc),
         col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
}
perZone_time_vs_nec()
