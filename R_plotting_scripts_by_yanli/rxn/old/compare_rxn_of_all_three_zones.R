# Name: Yanli Xu
# Date: Dec 23th, 2016
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files, here x = 1, 2, and 3
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_rxn_of_all_three_zones.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)

# The following is an example of running this script
# > source("compare_rxn_of_all_three_zones.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/161003_161010
# Enter 9 concentration values separated by comma: 1, 5, 15, 25, 35, 50, 75, 100, 125
# You have 9 plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input 3 different colors to represent 3 different zones, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red, blue
# Please input the cycleLimit of these experiments: 21600
# Please choose a number for the rxn you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N, 9 for GSH_Depletion: 1

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  # Read the files
  files_zone_0 <- list.files(path = work_dir, pattern = "[0-9]+_rxnProduct_zone-0.csv", recursive = T)
  files_zone_1 <- list.files(path = work_dir, pattern = "[0-9]+_rxnProduct_zone-1.csv", recursive = T)
  files_zone_2 <- list.files(path = work_dir, pattern = "[0-9]+_rxnProduct_zone-2.csv", recursive = T)
  
  #print(files_zone_0)
  
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files_zone_0),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
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
  rxn_str <- readline("Please choose a number for the rxn you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N, 9 for GSH_Depletion: ")
  rxn_num <- as.numeric(unlist(strsplit(rxn_str, "[, ]+")))
#   # ask the max value of rxn
#   y_max_str <- readline("Please input the maximun value of rxn in three zones: ")
#   y_max <- as.numeric(unlist(strsplit(y_max_str, "[, ]+")))

  max_of_every_con <- c()
  for(i in 1:length(conc))
  {
    exp_0 <- eval(parse(text = exp_names_zone_0[i]))
    exp_1 <- eval(parse(text = exp_names_zone_1[i]))
    exp_2 <- eval(parse(text = exp_names_zone_2[i]))
    max_of_every_con[i] = max(max(mav(exp_0[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_1[, (rxn_num + 1)]), na.rm=TRUE), max(mav(exp_2[, (rxn_num + 1)]), na.rm=TRUE))
  }
  y_max = max(max_of_every_con)

  for(i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names_zone_0[i]))
    title = paste("Time_vs", rxn_names[rxn_num], conc[i], sep = "_")
    plot(mav(exp[, (rxn_num + 1)]) ~ exp$Time, type = "l", main = title, col = color[1], xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(0, y_max))
    exp <- eval(parse(text = exp_names_zone_1[i]))
    lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[2])
    exp <- eval(parse(text = exp_names_zone_2[i]))
    lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[3])
    
    if(i == 1)
    {
#       legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
#       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      legend(0, y_max, c("Z_1", "Z_2", "Z_3"),
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
  }

}
time_vs_rxn()
