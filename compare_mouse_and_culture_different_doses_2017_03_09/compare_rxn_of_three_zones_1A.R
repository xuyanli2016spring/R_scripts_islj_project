# Name: Yanli Xu
# Date: Jan 7th, 2017
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files, here x = 1, 2, and 3
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_rxn_of_three_zones_1A.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)
# every page is for one solute, every solute has different concentrations, and every ceoncentration has three zones.

# updated on Mar 10th, 2017 to compare the mouse and culture for the research conference of March of 2017

library(Hmisc)
mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  # Read the files
  files_zone_0 <- list.files(path = work_dir, pattern = "_rxnProduct_zone-0.csv", recursive = T)
  files_zone_1 <- list.files(path = work_dir, pattern = "_rxnProduct_zone-1.csv", recursive = T)
  files_zone_2 <- list.files(path = work_dir, pattern = "_rxnProduct_zone-2.csv", recursive = T)
  
  #print(files_zone_0)
  
#   # Ask user to input the concentrations
#   conc_str <- readline(paste("Enter ", length(files_zone_0),  " concentration values separated by comma: ", sep = ""))
#   conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  
  # Ask user to input the names of the experiments
  conc_str <- readline(paste("Enter ", length(files_zone_0),  " names for the experiments that you want to compare: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  
  
  # Read the files
  exp_names_zone_0 <- c()
  exp_names_zone_1 <- c()
  exp_names_zone_2 <- c()
  for(i in 1:length(conc))
  {
    exp_names_zone_0[i] <- paste("rxn_zone_0",conc[i], sep="_")
    assign(exp_names_zone_0[i], read.csv(paste(work_dir, files_zone_0[i], sep= "/"), header = TRUE, sep = ","))
    exp_names_zone_1[i] <- paste("rxn_zone_1",conc[i], sep="_")
    assign(exp_names_zone_1[i], read.csv(paste(work_dir, files_zone_1[i], sep= "/"), header = TRUE, sep = ","))
    exp_names_zone_2[i] <- paste("rxn_zone_2",conc[i], sep="_")
    assign(exp_names_zone_2[i], read.csv(paste(work_dir, files_zone_2[i], sep= "/"), header = TRUE, sep = ","))
    
  }
  mf_input <- readline(paste("You have ", length(files_zone_1), " plots, please input how many rows and columns do you want to display the plots: ", sep = ""))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  # ask user to input three colors for three zones
  col_str <- readline(paste("Please input 3 different colors to represent 3 different zones, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  rxn_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
  # Ask the user which reaction product to plot
#   rxn_str <- readline("Please choose a number for the rxn you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N, 9 for GSH_Depletion: ")
#   rxn_num <- as.numeric(unlist(strsplit(rxn_str, "[, ]+")))
  #   # ask the max value of rxn
  #   y_max_str <- readline("Please input the maximun value of rxn in three zones: ")
  #   y_max <- as.numeric(unlist(strsplit(y_max_str, "[, ]+")))
  
  
  #####################
  for(rxn_num in 1:length(rxn_names))
  {
    max_of_every_con <- c()
    for(i in 1:length(conc))
    {
      exp_0 <- eval(parse(text = exp_names_zone_0[i]))
      exp_1 <- eval(parse(text = exp_names_zone_1[i]))
      exp_2 <- eval(parse(text = exp_names_zone_2[i]))
      max_of_every_con[i] = max(max(mav(exp_0[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_1[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_2[, (rxn_num + 1)]), na.rm=TRUE))
    }
    y_max = max(max_of_every_con)
    y_min = 0
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names_zone_0[i]))
      title = paste("Time vs", rxn_names[rxn_num], "of", conc[i], sep = " ")
      plot(mav(exp[, (rxn_num + 1)]) ~ exp$Time, type = "l", main = title, col = color[1], xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
      minor.tick(nx=5, ny=5, tick.ratio=0.5)
      exp <- eval(parse(text = exp_names_zone_1[i]))
      lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[2])
      exp <- eval(parse(text = exp_names_zone_2[i]))
      lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[3])
      legend("center", c("Z_1", "Z_2", "Z_3"), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      
#       if(i == 1)
#       {
#         #       legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
#         #       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#         #       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
#         legend(0, y_max, c("Z_1", "Z_2", "Z_3"), col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
#       }
    }

  }

  #########################
#   max_of_every_con <- c()
#   for(i in 1:length(conc))
#   {
#     exp_0 <- eval(parse(text = exp_names_zone_0[i]))
#     exp_1 <- eval(parse(text = exp_names_zone_1[i]))
#     exp_2 <- eval(parse(text = exp_names_zone_2[i]))
#     max_of_every_con[i] = max(max(mav(exp_0[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_1[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_2[, (rxn_num + 1)]), na.rm=TRUE))
#   }
#   y_max = max(max_of_every_con)
#   
#   for(i in 1:length(conc))
#   {
#     exp <- eval(parse(text = exp_names_zone_0[i]))
#     title = paste("Time_vs", rxn_names[rxn_num], conc[i], sep = "_")
#     plot(mav(exp[, (rxn_num + 1)]) ~ exp$Time, type = "l", main = title, col = color[1], xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(0, y_max))
#     exp <- eval(parse(text = exp_names_zone_1[i]))
#     lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[2])
#     exp <- eval(parse(text = exp_names_zone_2[i]))
#     lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[3])
#     
#     if(i == 1)
#     {
#       #       legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
#       #       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#       #       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
#       legend(0, y_max, c("Z_1", "Z_2", "Z_3"),
#              col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
#     }
#   }
  
}
time_vs_rxn()
