# Name: Yanli Xu
# Date: Nov 30th, 2016
# Note: This script is used for plotting time vs mean_distance_from_cv, time vs mean_distance_from_pv, time vs median_distance_from_cv and time vs median_distance_from_pv for multiple files
# Every concentration has a plot
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("trig_time_vs_dist_from_CV_or_PV_mbyn.R"), the script will start working
# Input file: XXXX_nectrig.csv

# The following is an example of running this script
# > source("trig_time_vs_dist_from_CV_or_PV_mbyn.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# You have  9  plots, please input how many rows and columns do you want to display the plots:3, 3
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
# Please input the cycleLimit of these experiments: 36000
# Please input bottom and upper limits of y-axis to represent the range of mean distances from CV, for example 0, 40: 0, 30
# Please choose one distance to plot, your choices are CV and PV: CV
# Please choose one type of distance, your choices are Mean and Median: Mean

time_vs_nec_dist <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
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
  
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
#   # Ask the user to decide the x-axis and y-axis range
#   run_time_str <- readline("Please input the cycleLimit of these experiments: ")
#   run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
#   y_lim_str <- readline(paste("Please input bottom and upper limits of y-axis to represent the range of mean distances from CV, for example 0, 40: ", sep = ""))
#   y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  # Ask user from which distance, CV or PV?
  zone_str <- readline("Please choose one distance to plot, your choices are CV and PV: ")
  zone <- unlist(strsplit(zone_str, "[, ]+"))
  # Ask user choose median or mean dist
  type_str <- readline("Please choose one type of distance, your choices are Mean and Median: ")
  type_dist <- unlist(strsplit(type_str, "[, ]+"))
  
  # Ask the user to decide the x-axis and y-axis range
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  y_lim_str <- readline(paste("Please input bottom and upper limits of y-axis to represent the range of ",  type_dist," distances from ", zone,", for example 0, 40: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  ##########
  for(i in 1: length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    moving_dist <- c()
    moving_time <- c()
    for(index_1 in 1: (nrow(exp)-99))
    {
      sub_dist = c()
      sub_time = c()
      count = 1
      for(index_2 in index_1:(index_1 + 99))
      {
        sub_dist[count] = exp[index_2, c(paste(type_dist, ".Dist_from_", zone, sep=""))]
        sub_time[count] = exp$Time_of_NecTrig[index_2]
        count = count + 1
      }
      if(type_dist == "Mean")
      {
        moving_dist[index_1] = mean(sub_dist)
        moving_time[index_1] = mean(sub_time)
      }
      else if(type_dist == "Median")
      {
        moving_dist[index_1] = median(sub_dist)
        moving_time[index_1] = median(sub_time)
      }
    }
    title <- paste("Trig_Time_vs_", type_dist, "_dist_to_", zone, "_", conc[i], sep = " " )
    plot(moving_dist ~ moving_time, type = "l", main = title, col = color[i], xlab = "Time", ylab = paste("Dist_to_", zone, sep = ""), xlim = c(0, run_time), ylim = y_limits)
  }
}
time_vs_nec_dist()

