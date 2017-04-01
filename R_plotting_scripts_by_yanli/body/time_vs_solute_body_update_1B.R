# Name: Yanli Xu
# Date: Jan 4th, 2017
# Note: This script is used for plotting time vs solute in body for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("time_vs_solute_body_update_1B.R"), the script will start working 
# Input file: XXXX-XX-XX-XXXX-XXXX_mean_body_.csv
# plot all the time vs medium_solute ("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N" )
# This R script plots every solute on one page. Every concentration of a solute has a plot. Every plot has the lines superimposed on the surface of the light color dots.

# The following is an example of running this script
# > source("time_vs_solute_body_update_1B.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data/bolus_in_2016_12/161215_161219/exp_data_0p1_to_0p9
# Enter 9 concentration values separated by comma: 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments and wait... 21600


mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
time_vs_solute <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_mean_body_.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("body_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  # Ask the user how to display the plot
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: ")) 
  
  run_time_str <- readline("Please input the cycleLimit of these experiments and wait: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  exp <- eval(parse(text = exp_names[length(files)]))
  solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  

  for(solute_num in 2:length(solute_names))
  {
    par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
    # get the max value in every experiment 
    max_for_every_exp <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      max_for_every_exp[i] = max(exp[, solute_num])
    }
    y_max = max(max_for_every_exp)
    y_min = 0
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      title = paste("Body_Time_vs", solute_names[solute_num], conc[i], sep = "_")
      plot(exp[, solute_num] ~ exp$Time, main = title, col = "grey85", xlab = "Time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
      lines(mav(exp[, solute_num]) ~ exp$Time)
    }
  }

}
time_vs_solute()


