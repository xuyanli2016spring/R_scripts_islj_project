# Name: Yanli Xu
# Date: Nov 21th, 2016
# Note: This script is used for plotting time vs perZone nectrig for multiple files, the output is m*n plots, every plot for one concentration
# Every plot includes Zone1, zone2, zone3 and zone_nearPV
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("time_vs_perZone_trig_for_for_all_zones.R"), the script will start working
# input file: XXXX-XX-XX-XXXX-XXXX_perZone_nectrig.csv

# The following is an example of running this script
# > source("time_vs_perZone_trig_for_all_zones.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 36000
# Based on your input, the bottom and upper limits of x-axis are 0 and 36000, Please input bottom and upper limits of y-axis to represent the max range of per_zone_nectrig: 0, 5500

time_vs_perZone_nec <- function()
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
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  y_lim_str <- readline(paste("Based on your input, the bottom and upper limits of x-axis are 0 and ", run_time, ", Please input bottom and upper limits of y-axis to represent the max range of per_zone_nectrig: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  for(i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    title = paste("PerZone_trig_conc", conc[i], sep = "_")
    plot(exp$Cumu_Nectrig_Z3 ~ exp$Time_Z3, type = "l", main = title, xlab = "Time", ylab = "nectrig", ylim = y_limits, xlim = c(0, run_time))
    lines(exp$Cumu_Nectrig_Z2 ~ exp$Time_Z2, col = "blue")
    lines(exp$Cumu_Nectrig_Z1 ~ exp$Time_Z1, col = "red")
    lines(exp$Cumu_Nectrig_nearPV ~ exp$Time_nearPV, col = "green")
    
    if(i == 1)
    {
      legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
      legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      legend(legend_x, legend_y, c("Z_3", "Z_2", "Z_1", "Z_PV"), col=c("Black", "blue", "red", "green"), cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
  }
}
time_vs_perZone_nec()

