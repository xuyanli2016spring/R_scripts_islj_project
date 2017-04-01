# Name: Yanli Xu
# Date: Dec 22th, 2016
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("Zone_X_time_vs_rxn_update.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)

# The following is an example of running this script
# > source("time_vs_solute_medium_update.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/exp_2016_11_24_to_2016_12_06/data_from_2016_11_24_to_2016_12_06
# Enter 9 concentration values separated by comma: 1, 5, 15, 25, 35, 50, 75, 100, 125
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 36000
# Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: 7

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  # Ask user the zone number
  zone_str <- readline("Please enter the zone number you would like to plot, you may choose from 0, 1 and 2: ")
  zone_num <- as.numeric(unlist(strsplit(zone_str, "[, ]+")))
  # Read the files
  files <- list.files(path = work_dir, pattern =  paste("[0-9]+_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("rxn_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  #  convert the last concentration file to an object
  exp <- eval(parse(text = exp_names[length(files)]))
  
  # Get the names of the variables 
  rxn_names <- names(exp)  
  # "Time", "Z.X.S", "Z.X.nMD", "Z.X.MitoDD", "Z.X.oAPAP", "Z.X.G", "Z.X.Marker", "Z.X.Repair", "Z.X.N", "Z.X.GSH_Depletion"
  # Ask the user which reaction product to plot
  rxn_str <- readline("Please choose a number for the rxn you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N, 9 for GSH_Depletion: ")
  rxn_num <- as.numeric(unlist(strsplit(rxn_str, "[, ]+")))
  
  # get the max value in every experiment 
  max_for_every_exp <- c()
  for(i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    max_for_every_exp[i] = max(exp[, (rxn_num + 1)])
  }
  y_max = max(max_for_every_exp) * 0.9
  
  for(i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    title = paste("Time_vs", rxn_names[rxn_num + 1], conc[i], sep = "_")
    plot(exp[, (rxn_num + 1)] ~ exp$Time, main = title, col = "grey85", xlab = "Time", ylab = rxn_names[rxn_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
    lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time)
  }
}
time_vs_rxn()
