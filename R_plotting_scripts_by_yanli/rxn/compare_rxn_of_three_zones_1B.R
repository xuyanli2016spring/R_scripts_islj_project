# Name: Yanli Xu
# Date: Jan 7th, 2017
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_rxn_of_three_zones_1B.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)
# the output is every solute is on onepage, every page has 3 plots for three different zones, every plot includens the lines of all the concentrations

# The following is an example of running this script
# > source("compare_rxn_of_three_zones_1B.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161215_161219/exp_data_0p1_to_0p9
# Please enter the number of concentrations: 9
# Enter 9 concentration values separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
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
  
  # ask user to input colors for different concentrations
  col_str <- readline(paste("Please input ", num_conc, " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
#   # Ask user to input the layout of th eplots
#   mf_input <- readline(paste("You have ", num_conc, " plots, please input how many rows and columns do you want to display the plots: "))
#   par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  
  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
  # every page has three plots, every plot is one zone
  par(mfrow = c(1, 3)) 
  
  ##################
  for(solute_number in 1: length(solute_names))
  {
    # Get the max y_axis value
    max_of_every_zone <- c()
    for(zone_num in 1:3)
    {
      files <- list.files(path = work_dir, pattern =  paste("_rxnProduct_zone-", (zone_num - 1), ".csv", sep = ""), recursive = T)
      exp_names <- c()
      for(i in 1:num_conc)
      {
        exp_names[i] <- paste("rxn_", conc[i], sep="")
        assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
      }
      max_for_every_exp <- c()
      for(i in 1:num_conc)
      {
        exp <- eval(parse(text = exp_names[i]))
        max_for_every_exp[i] = max(mav(exp[, (solute_number + 1)]), na.rm = T)
        
      }
      max_of_every_zone[zone_num] = max(max_for_every_exp)
    }
    y_max = max(max_of_every_zone)
    y_min = 0
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }

    # plot start
    for(zone_num in 0:2)
    {
      # read the files
      files <- list.files(path = work_dir, pattern =  paste("_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
      exp_names <- c()
      for(i in 1:num_conc)
      {
        exp_names[i] <- paste("rxn_", conc[i], sep="")
        assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
      }
      
#       # ###### 
#       # get the max value in every experiment 
#       max_for_every_exp <- c()
#       for(i in 1:num_conc)
#       {
#         exp <- eval(parse(text = exp_names[i]))
#         max_for_every_exp[i] = max(mav(exp[, (solute_number + 1)]), na.rm = T)
#       }
#       y_max = max(max_for_every_exp)

      plot(c(), c(), main = paste("Time_vs_", solute_names[solute_number], "_Z", zone_num, sep = ""), xlab = "Time", ylab = solute_names[solute_number], xlim = c(0, run_time), ylim = c(y_min, y_max))
      
      for(i in 1:num_conc)
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, (solute_number + 1)]) ~ exp$Time, col = color[i])
        
      }
      
      if(zone_num == 0)
      {
        legend((run_time*0.6), y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      }
      

    }
  }

}
time_vs_rxn()
