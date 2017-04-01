# Name: Yanli Xu
# Date: Jan 7th, 2017
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("zone_X_time_vs_rxn_updated_1A.R"), the script will start working
# Input files: XXXX-XX-XX-XXXX-XXXX-rxnProduct_zone-x.csv (including zone 1, 2 and 3)
# plot all the time vs rxnProduct (S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)
# the output is 27 pages of plots, every page is for one solute in a specific zone

# updated on Mar 10th, 2017 for comparing the 6 pairs of mouse and culture experiments which were selected for conference of March of 2017 in MD

library(Hmisc)
mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[, ]+"))
  
  # Ask user how many experiments
  num_conc_str <- readline(paste("Please enter the number of experiments for comparison: ", sep = ""))
  num_conc <- as.numeric(unlist(strsplit(num_conc_str, "[, ]+")))

  # Ask user to input the names of the experiments
  conc_str <- readline(paste("Enter ", num_conc,  " names for the experiments that you want to compare: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  
  # ask user to choose the colors 
  col_str <- readline(paste("Please input ", num_conc, " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask user to input the layout of the plots
  mf_input <- readline(paste("You have ", num_conc, " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  # Ask user the cycleLimit, this information can be used for the max value of the x-axis
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  #solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")

  for(zone_num in 0:2)
  {
    files <- list.files(path = work_dir, pattern =  paste("_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)

    exp_names <- c()
    for(i in 1:length(files))
    {
      exp_names[i] <- paste("rxn_", i, sep="")
      assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
    }
    #  convert the first concentration file to an object
    exp <- eval(parse(text = exp_names[1]))
    # Get the names of the variables 
    rxn_names <- names(exp)  
    # "Time", "Z.X.S", "Z.X.nMD", "Z.X.MitoDD", "Z.X.oAPAP", "Z.X.G", "Z.X.Marker", "Z.X.Repair", "Z.X.N", "Z.X.GSH_Depletion"
    
#     # Plot all the rxn_products, lines is floated on the dots
#     for(rxn_num in 2:length(rxn_names))
#     {
#       # get the max value in all the experiments
#       max_for_every_exp <- c()
#       for(exp_num in 1:length(exp_names))
#       {
#         exp <- eval(parse(text = exp_names[exp_num]))
#         max_for_every_exp[exp_num] = max(exp[, rxn_num])
#       }
#       y_max = max(max_for_every_exp)
#       if(y_max == 0)
#       {
#         y_min = -1
#         y_max = 1
#       }
#       else
#       {
#         y_min = 0
#       }
#       for(exp_num in 1:length(exp_names))
#       {
#         exp <- eval(parse(text = exp_names[exp_num]))
#         title = paste("Time vs", rxn_names[rxn_num], "of", conc[exp_num], sep = " ")
#         plot(exp[, rxn_num] ~ exp$Time, pch= ".", main = title, col = "grey85", xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
#         lines(mav(exp[, rxn_num]) ~ exp$Time)
#       }
#     }
    
    # plot all the rxn_products with only lines but no dots under the lines
    for(rxn_num in 2:length(rxn_names))
    {
      max_for_every_exp <- c()
      for(exp_num in 1:length(exp_names))
      {
        exp <- eval(parse(text = exp_names[exp_num]))
        max_for_every_exp[exp_num] = max(mav(exp[, rxn_num]), na.rm = TRUE)
      }
      y_max = max(max_for_every_exp)
      y_min = 0
      if(y_max == 0)
      {
        y_min = -1
        y_max = 1
      }
      title = paste("Time vs", rxn_names[rxn_num], sep = " ")
      plot(c(), c(), type = "l", main = title, xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
      minor.tick(nx=5, ny=3, tick.ratio=0.5)
      
      for(exp_num in 1:length(exp_names))
      {
        exp <- eval(parse(text = exp_names[exp_num]))
        lines(mav(exp[, rxn_num]) ~ exp$Time, col = color[exp_num])
        #Sys.sleep(1)
      }
      legend("center", conc, col=color, cex=1, text.font=1, lty = c(1, 1), bg='lightblue')
      
#       title = paste("Time vs", rxn_names[rxn_num], sep = " ")
#       plot(c(), c(), main = title, xlab = "Time", ylab = rxn_names[rxn_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
#       minor.tick(nx=5, ny=3, tick.ratio=0.5)
#       # plot all the experiments
#       for(exp_num in 1:length(exp_names))
#       {
#         exp <- eval(parse(text = exp_names[exp_num]))
#         lines(mav(exp[, rxn_num]) ~ exp$Time, color[exp_num])
#         #mav(exp[, rxn_num]) ~ exp$Time
#         #Sys.sleep(1)
#       }
      #legend((run_time * 0.8), ((y_max - y_min) * 0.5), conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
  }
}
time_vs_rxn()
