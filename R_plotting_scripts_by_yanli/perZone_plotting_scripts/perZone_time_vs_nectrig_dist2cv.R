# Name: Yanli Xu
# Date: Nov 29th, 2016
# Note: This script is used for plotting time vs trig_dist2cv for zone_1, zone2, zone3 and zone_nearPV.
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("perZone_time_vs_trig_dist2cv.R"), the script will start working 
# input file: XXXX-XX-XX-XXXX-XXXX_perZone_nectrig.csv

# The following is an example of running this script
# > source("perZone_time_vs_trig_dist2cv.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
# Please choose one zone to plot, your choices are Z3, Z2, Z1 and nearPV: Z3
# Please input the cycleLimit of these experiments: 36000
# You already got the plot, please input x and y coordinates to put legends: 25000, 6



perZone_time_vs_dist <- function()
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
  # Choose the y values based on the input of "zone"
  if(zone == "Z3")
  {
    y_limit <- c(0, 10)
  }
  else if(zone == "Z2")
  {
    y_limit <- c(10, 20)
  }
  else if(zone == "Z1")
  {
    y_limit <- c(20, 30)
  }
  else
  {
    y_limit <- c(30, 50)
  }
  
  title <- paste("NecTrig_DistCV_", zone, " for different concentrations", sep = "")
  plot(c(), c(), main = title, xlab = "Time", ylab = "DistCV", ylim = y_limit, xlim = c(0, run_time))
  for(i in 1: length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    
    MA_Dist_from_CV <- c()
    MA_Time_of_NecTrig <- c()
    
    if(nrow(exp) <= 99)
    {
      MA_Dist_from_CV <- rep(0, nrow(exp))
      MA_Time_of_NecTrig <- rep(0, nrow(exp))
    }
    else
    {
      for(index_1 in 1: (nrow(exp)-99))
      {
        sub_dist = c()
        sub_time = c()
        count = 1
        for(index_2 in index_1:(index_1 + 99))
        {
          sub_dist[count] = exp[index_2, c(paste("DistCV_", zone, sep = ""))]
          sub_time[count] = exp[index_2, c(paste("Time_", zone, sep = ""))]
          count = count + 1
        }
        MA_Dist_from_CV[index_1] = mean(sub_dist)
        MA_Time_of_NecTrig[index_1] = mean(sub_time)
      }
      lines(MA_Dist_from_CV ~ MA_Time_of_NecTrig, col = color[i])
    }
  }
  
  
  
  legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
  legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
  legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
  legend(legend_x, legend_y, as.character(conc),
         col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
}
perZone_time_vs_dist()


















# perZone_time_vs_dist <- function()
# {
#   # Ask user to input the working directory
#   work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
#   # Read the files
#   files <- list.files(path = work_dir, pattern = "[0-9]+_perZone_nectrig.csv", recursive = T)
#   # Ask user to input the concentrations
#   conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
#   conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
#   exp_names <- c()
#   for(i in 1:length(conc))
#   {
#     exp_names[i] <- paste("nectrig_", conc[i], sep="")
#     assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
#   }
#   par(mfrow=c(1, 1))
#   
#   col_str <- readline(paste("Please input ", length(files), " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
#   color <- unlist(strsplit(col_str, "[, ]+"))
#   
#   # ask user choose one zone from zone1, zone2, zone3, zone_nearPV
#   zone_str <- readline("Please choose one zone to plot, your choices are Z3, Z2, Z1 and nearPV: ")
#   zone <- unlist(strsplit(zone_str, "[, ]+"))
#   
#   # ask user to input the run time
#   run_time_str <- readline("Please input the cycleLimit of these experiments: ")
#   run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
#   # Choose the y values based on the input of "zone"
#   if(zone == "Z3")
#   {
#     y_limit <- c(0, 10)
#   }
#   else if(zone == "Z2")
#   {
#     y_limit <- c(10, 20)
#   }
#   else if(zone == "Z1")
#   {
#     y_limit <- c(20, 30)
#   }
#   else
#   {
#     y_limit <- c(30, 50)
#   }
# 
#   title <- paste("Nectrig_DistCV_", zone, " for different concentrations", sep = "")
#   plot(c(), c(), main = title, xlab = "Time", ylab = "DistCV", ylim = y_limit, xlim = c(0, run_time))
#   for(i in 1: length(conc))
#   {
#     exp <- eval(parse(text = exp_names[i]))
#     MA_Dist_from_CV <- c()
#     MA_Time_of_NecTrig <- c()
#     for(index_1 in 1: (nrow(exp)-99))
#     {
#       sub_dist = c()
#       sub_time = c()
#       count = 1
#       for(index_2 in index_1:(index_1 + 99))
#       {
#         sub_dist[count] = exp[index_2, c(paste("DistCV_", zone, sep = ""))]
#         sub_time[count] = exp[index_2, c(paste("Time_", zone, sep = ""))]
#         count = count + 1
#       }
#       MA_Dist_from_CV[index_1] = mean(sub_dist)
#       MA_Time_of_NecTrig[index_1] = mean(sub_time)
#     }
#     lines(MA_Dist_from_CV ~ MA_Time_of_NecTrig, col = color[i])
#   }
# 
#   legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
#   legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#   legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
#   legend(legend_x, legend_y, as.character(conc),
#          col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
# }
# perZone_time_vs_dist()
