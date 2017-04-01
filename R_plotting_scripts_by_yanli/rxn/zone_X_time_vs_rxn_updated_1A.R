# Name: Yanli Xu
# Date: Jan 7th, 2017
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("zone_X_time_vs_rxn_updated_1A.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)
# the output is 27 pages of plots, every page is for one zone of one solute

# The following is an example of running this script
# > source("zone_X_time_vs_rxn_updated_1A.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161215_161219/exp_data_0p1_to_0p9
# Please enter the number of concentrations: 9
# Enter 9 concentration values separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 21600

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  
  # Ask user how many concentrations
  num_conc_str <- readline(paste("Please enter the number of concentrations: ", sep = ""))
  num_conc <- as.numeric(unlist(strsplit(num_conc_str, "[, ]+")))

  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", num_conc,  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  # Ask user to input the layout of th eplots
  mf_input <- readline(paste("You have ", num_conc, " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")

  for(zone_num in 0:2)
  {
    files <- list.files(path = work_dir, pattern =  paste("_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
    exp_names <- c()
    for(i in 1:length(conc))
    {
      exp_names[i] <- paste("rxn_", conc[i], sep="")
      assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
    }
    #  convert the first concentration file to an object
    exp <- eval(parse(text = exp_names[1]))
    # Get the names of the variables 
    rxn_names <- names(exp)  
    # "Time", "Z.X.S", "Z.X.nMD", "Z.X.MitoDD", "Z.X.oAPAP", "Z.X.G", "Z.X.Marker", "Z.X.Repair", "Z.X.N", "Z.X.GSH_Depletion"
    
    for(rxn_num in 1:length(solute_names))
    {
      # get the max value in every experiment 
      max_for_every_exp <- c()
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        max_for_every_exp[i] = max(exp[, (rxn_num + 1)])
      }
      y_max = max(max_for_every_exp)
      
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        title = paste("Time_vs", rxn_names[rxn_num + 1], conc[i], sep = "_")
        plot(exp[, (rxn_num + 1)] ~ exp$Time, main = title, col = "grey85", xlab = "Time", ylab = rxn_names[rxn_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
        lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time)
      }
    }

  }
}
time_vs_rxn()
