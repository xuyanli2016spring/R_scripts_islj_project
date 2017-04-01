# Name: Yanli Xu
# Date: Jan 6th, 2017
# Note: This script is used for plotting time vs mean_distance_from_cv, time vs mean_distance_from_pv, time vs median_distance_from_cv and time vs median_distance_from_pv for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("trig_time_vs_dist_from_CV_or_PV_updated_1A.R"), the script will start working 
# Input file: XXXX_nectrig.csv
# Output is four plots, every plot includes all the concentrations

# this plot is updated on Mar 14th, 2017 for comparing the mouse and cultuer experiment pairs for the conference of 2017

library(Hmisc)
library(svglite)
svg("plots.svg")
mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_nec_dist <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " different names to represent these experiments: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("nectrig_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  #   # ask user to decide the rows and columns to display the plots
  #   mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  #   par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  par(mfrow=c(1, 1))
  
  
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask the user to range of the x-axis
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
      plot(c(), c(), xlim = c(0, run_time), ylim = c(y_min, y_max), main = paste("Nectrig time vs ", t, ".dist_from_", z), xlab = "Time", ylab = paste("Distance from ", z, sep = ""))
      minor.tick(nx=5, ny=5, tick.ratio=0.5)
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]) ~ exp$Time_of_NecTrig, col = color[i])
      }
      legend("center", as.character(conc), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      
      #       legend_str <- readline("please input x and y coordinates to put legends: ")
      #       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      #       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      
      # if(z == "CV")
      # {
      #   legend(0, y_max, as.character(conc), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      # }
      # else
      # {
      #   legend((run_time * 0.8), y_max, as.character(conc), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      # }
      Sys.sleep(10)
    }
  }
  
}
time_vs_nec_dist()
dev.off()

