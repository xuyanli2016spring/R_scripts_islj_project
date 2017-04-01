# Name: Yanli Xu
# Date: Nov 29th, 2016
# Note: This script is used for plotting time vs nectrig for multiple files, the plots can be displayed in 1 by 1, or m by n
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("time_vs_nectrig.R"), the script will start working 
# Input file: XXXX_nectrig.csv

# updated on Mar 12th, 2017 to compare the mouse and culture for the research conference of March of 2017

#install.packages("svglite")
library(Hmisc)
library(svglite)
svg("plots.svg")
time_vs_nec <- function()
{
  
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " different names to represent the experiments: ", sep = ""))
  # conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("nectrig_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  # ask user choose the number of rows and columns to display the plots
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots, you may choose (1, 1), or (m, n): "))
  mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
  par(mfrow=mf)
  # ask user to choose the colors 
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  y_lim_str <- readline(paste("Based on your input, the bottom and upper limits of x-axis are 0 and ", run_time, ", Please input bottom and upper limits of y-axis to represent the range of nectrig: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  if(identical(mf, c(1, 1)))
  {
    plot(c(), c(), type = "l", main = "Time vs Cumu_Nectrig", xlab = "Time", ylab = "Cumu_Nectrig", xlim = c(0, run_time), ylim = y_limits)
    minor.tick(nx=5, ny=5, tick.ratio=0.5)
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(exp$Cumu_Nectrig ~ exp$Time_of_NecTrig, col = color[i])
      
    }
    legend("topleft", conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    #legend("center", conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
#     legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
#     legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#     legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
#     
#     legend(legend_x, legend_y, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
  }
  else
  {
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      title = paste(exp_names[i], "Time vs Nectrig", sep = " ")
      plot(exp$Cumu_Nectrig ~ exp$Time_of_NecTrig, type = "l", main = title, xlim = c(0, run_time), ylim = y_limits, col = color[i], xlab = "Time", ylab = "Nectrig")
    }
  }
}
time_vs_nec()
dev.off()





# #############################
# svg("plots.svg")
# setwd("/Users/yanlixu/Desktop/experiment_data_2017/exp_2017_01_15_to_2017_01_21_and_exp_2017_02_10_to_2017_03_06/exp_mouse_12MC_2017_02_17_to_2017_03_06/selected_6_mouse_exp/mouse_and_culture_exp_of_0p340_nectrig_rate")
# files <- list.files(pattern = "[0-9]+_nectrig.csv", recursive = T)
# nec_1 <- read.csv(files[1], header = T, sep = ",")
# nec_2 <- read.csv(files[2], header = T, sep = ",")
# plot(c(), c(), type = "l", main = "Time vs Cumu_Nectrig", xlab = "Time", ylab = "Cumu_Nectrig", xlim = c(0, 10800), ylim = c(0, 30000))
# lines(nec_1$Cumu_Nectrig ~ nec_1$Time_of_NecTrig)
# lines(nec_2$Cumu_Nectrig ~ nec_2$Time_of_NecTrig, col = "red")
# legend(6000, 15000, c("mouse_0p340", "culture_0p340"), col=c("black", "red"), cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
# 
# dev.off()


#library(svglite)


