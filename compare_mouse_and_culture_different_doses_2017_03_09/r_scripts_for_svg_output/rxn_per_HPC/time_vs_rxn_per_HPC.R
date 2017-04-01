# Name: Yanli Xu
# Date: Mar 13th, 2017
# Note: This script is used for plotting time vs rxn_perHPC for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("tiem_vs_rxn_per_HPC.R"), this script will start working.


library(Hmisc)
library(svglite)
svg("plots.svg")

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_solute <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "rxn_per_HPC.csv", recursive = T)
  
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " different experiment names to represent these experiments: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  # Ask user to input one color for every experiment
  col_str <- readline(paste("Please input ", length(conc), " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Read the files
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("rxn_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  exp <- eval(parse(text = exp_names[length(files)]))
  solute_names <- names(exp) # "Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion"
  
  #   solute_str <- readline("Please choose a number for every plot of the solute you would like to put legends, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  #   solute_num_legend <- as.numeric(unlist(strsplit(solute_str, "[, ]+"))) + 1
  
  mf_input <- readline(paste("You have ", (length(solute_names) - 1), " plots, please input how many rows and columns do you want to display the plots: ", sep = ""))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  for(solute_num in 2:length(solute_names))
  {
    # get the max value in every experiment 
    max_for_every_exp <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      max_for_every_exp[i] = max(mav(exp[, solute_num]), na.rm=TRUE)
    }
    y_max = max(max_for_every_exp)
    y_min = 0
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    
    plot(c(), c(), type = "l", main = paste("Time vs rxn", solute_names[solute_num], "per HPC", sep = " "), xlab = "time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
    for(i in 1:length(files))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(mav(exp[, solute_num]) ~ exp$Time, col = color[i])
    } 
    Sys.sleep(10)
    minor.tick(nx=5, ny=5, tick.ratio=0.5)
    legend(0.3*run_time, y_max*0.7, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    #     if(solute_num %in% solute_num_legend)
    #     {
    #       legend(0, y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    #     }
  }
  
}
time_vs_solute()
dev.off()

