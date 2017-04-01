# Name: Yanli Xu
# Date: Jan 6th, 2017
# Note: This script is used for plotting time vs mean_distance_from_cv, time vs mean_distance_from_pv, time vs median_distance_from_cv and time vs median_distance_from_pv for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("nec_time_vs_dist_from_CV_or_PV_updated_1A.R"), the script will start working 
# Input file: XXXX_necrotic.csv
# Output is four plots, every plot includes all the concentrations

# The following is an example of running this script
# > source("nec_time_vs_dist_from_CV_or_PV_updated_1A.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161215_161219/exp_data_0p1_to_0p9
# Enter 9 concentration values separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
# Please input the cycleLimit of these experiments: 21600


mav <- function(x,n=51){filter(x,rep(1/n,n), sides=2)}
time_vs_nec_dist <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_necrotic.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("necrotic_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  #   # ask user to decide the rows and columns to display the plots
  #   mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  #   par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  par(mfrow=c(1, 1))

  
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))

  # Ask the user to decide the x-axis and y-axis range
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  zone <- c("CV", "PV")
  type_dist <- c("Mean", "Median")
  
  for(z in zone)
  {
    for(t in type_dist)
    {
      # calculate the min and max value of y axis
      max_of_every_exp <- c()
      min_of_every_exp <- c()
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        max_of_every_exp[i] <- max(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]), na.rm = TRUE)
        min_of_every_exp[i] <- min(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]), na.rm = TRUE)
      }
      y_max <- max(max_of_every_exp)
      y_min <- min(min_of_every_exp)

      ####### End of "calculate the min and max value of y axis"
      plot(c(), c(), xlim = c(0, run_time), ylim = c(y_min, y_max), main = paste("Necrotic time vs ", t, ".dist_from_", z), xlab = "Time", ylab = paste("Distance from ", z, sep = ""))
      
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]) ~ exp$Time_of_Necrosis, col = color[i])
      }
      
#       legend_str <- readline("please input x and y coordinates to put legends: ")
#       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
        
      if(z == "CV")
      {
        legend(0, y_max, as.character(conc), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      }
      else
      {
        legend((run_time * 0.8), y_max, as.character(conc), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      }
    }
  }

}
time_vs_nec_dist()

