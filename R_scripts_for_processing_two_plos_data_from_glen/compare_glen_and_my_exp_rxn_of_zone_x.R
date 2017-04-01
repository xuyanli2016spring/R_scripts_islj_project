# Name: Yanli Xu
# Date: Feb 5th, 2017
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_glen_and_my_exp_rxn_of_zone_x.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, G, Repair, N, GSH_Depletion)
# the output is 7 plots in every zone, every plot includes all the lines of Glen's and my exp

# The following is an example of running this script


mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  
  # Ask user how many concentrations
  num_conc_str <- readline(paste("Please enter the number of experiments in this directory: ", sep = ""))
  num_conc <- as.numeric(unlist(strsplit(num_conc_str, "[, ]+")))
  
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", num_conc,  " different names to distinguish these experiments: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))

  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  solute_names <- c("S", "nMD", "MitoDD", "G", "Repair", "N", "GSH_Depletion")
  
  col_str <- readline(paste("Please input ", num_conc, " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  for(zone_num in 0:2)
  {
    files <- list.files(path = work_dir, pattern =  paste("_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
    exp_names <- c()
    for(i in 1:length(conc))
    {
      exp_names[i] <- paste("rxn", zone_num, i, sep="_")
      assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
    }
#     #  convert the first concentration file to an object
#     exp <- eval(parse(text = exp_names[1]))
#     # Get the names of the variables 
#     rxn_names <- names(exp)  
#     # "Time", "Z.X.S", "Z.X.nMD", "Z.X.MitoDD", "Z.X.oAPAP", "Z.X.G", "Z.X.Marker", "Z.X.Repair", "Z.X.N", "Z.X.GSH_Depletion"
    
    for(s in solute_names)
    {
      ##########################
      max_of_every_exp <- c()
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        max_of_every_exp[i] = max(mav(exp[, paste("Z", zone_num, s, sep = ".")]), na.rm = T)
      }
      y_max = max(max_of_every_exp)
      y_min = 0
      #####
      if(y_max == 0)
      {
        y_min = -1
        y_max = 1
      }
      ##########################
      plot(c(), c(), type = "l", main = paste("rxn_Time_vs", s, "zone", zone_num, sep = "_"), xlab = "time", ylab = s, xlim = c(0, run_time), ylim = c(y_min, y_max))
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, paste("Z", zone_num, s, sep = ".")]) ~ exp$Time, col=color[i])
        Sys.sleep(1) 
      }
      
      legend((run_time/2), y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      
    }
    
    
    
    ##########################     
#     for(rxn_num in 1:length(solute_names))
#     {
#       # get the max value in every experiment 
#       max_for_every_exp <- c()
#       for(i in 1:length(conc))
#       {
#         exp <- eval(parse(text = exp_names[i]))
#         max_for_every_exp[i] = max(exp[, (rxn_num + 1)])
#       }
#       y_max = max(max_for_every_exp)
#       
#       for(i in 1:length(conc))
#       {
#         exp <- eval(parse(text = exp_names[i]))
#         title = paste("Time_vs", rxn_names[rxn_num + 1], conc[i], sep = "_")
#         plot(exp[, (rxn_num + 1)] ~ exp$Time, main = title, col = "grey85", xlab = "Time", ylab = rxn_names[rxn_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
#         lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time)
#       }
#  ##########################     
#       
#     }
    
  }
}
time_vs_rxn()



# setwd("/Users/yanlixu/Desktop/experiment_data_2017/From_Glen_to_compare_results_from_PLOS_paper/compare_Glen_and_my_exp/Glen_exp_data_2015-03-21-2249")
# list.files()
# data_1 <- read.csv("2015-03-21-2249_rxn_Zone_0.csv", header = T, sep = ",")
# names(data_1)
# names(data_1) <- c("Time", "Z.0.NecInhib", "Z.0.G", "Z.0.S", "Z.0.N", "Z.0.nMD", "Z.0.MitoDD", "Z.0.Repair", "Z.0.GSH_Depletion")
# write.table(data_1, "2015-03-21-2249_rxn_Zone_0.csv", row.names=FALSE, sep = ",")
# 
# data_2 <- read.csv("2015-03-21-2249_rxn_Zone_1.csv", header = T, sep = ",")
# names(data_2)
# 
# data_3 <- read.csv("2015-03-21-2249_rxn_Zone_2.csv", header = T, sep = ",")
# names(data_3)
