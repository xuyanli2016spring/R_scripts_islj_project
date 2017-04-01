# Name: Yanli Xu
# Date: Dec 6th, 2016
# Note: This script is used for plotting time vs solute for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("profile_time_vs_solute_culture.R"), the script will start working 
# Input file: XXXX-XX-XX-XXXX-XXXX_profile_mean_per_node.csv
# Plot all the time vs profile_solute ("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N" )

# The following is an example of running this script
# > source("profile_time_vs_solute_culture.R")

time_vs_solute <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_profile_mean_per_node.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("profile_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  exp <- eval(parse(text = exp_names[1]))
  # Ask the user which solute to plot
  solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
  
  y_lim_str <- readline(paste("Please input the bottom and upper value of y-axis to represent the min and max solute value, for example 0, 10: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  for(i in 1:length(conc))
  {

      exp <- eval(parse(text = exp_names[i]))
      title = paste("Time_vs", solute_names[solute_num + 1], conc[i], sep = "_")
      plot(exp[, (solute_num + 1)] ~ exp$Time, type = "l", main = title, xlab = "Time", ylab = solute_names[solute_num + 1], xlim = c(0, run_time), ylim = y_limits)

    
  }
  
}
time_vs_solute()