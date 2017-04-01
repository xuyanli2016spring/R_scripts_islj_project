# Name: Yanli Xu
# Date: Nov 22th, 2016
# Note: This script is used for plotting time vs solute in medium for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("time_vs_solute_medium.R"), the script will start working 
# Input file: XXXX-XX-XX-XXXX-XXXX_mean_medium_.csv
# plot all the time vs medium_solute ("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N" )

# The following is an example of running this script

time_vs_solute <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_mean_medium_.csv", recursive = T)
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
  
  
  #  predict the y-axis range by the last concentration
  exp <- eval(parse(text = exp_names[length(files)]))
  
  # Ask the user which solute to plot
  solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
  
  # predict the y axis from the the max_concentration file  
  MA_solute <- c()
  for(index_1 in 1: (nrow(exp)-99))
  {
    sub_solute = c()
    count = 1
    for(index_2 in index_1:(index_1 + 99))
    {
      sub_solute[count] = exp[index_2, (solute_num + 1)]
      count = count + 1
    }
    MA_solute[index_1] = mean(sub_solute)
  }
  y_max <- max(MA_solute)*1.1
  # plot time vs solute of all the concentrations
  for(i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    MA_solute <- c()
    MA_Time <- c()
    for(index_1 in 1: (nrow(exp)-99))
    {
      sub_solute = c()
      sub_time = c()
      count = 1
      for(index_2 in index_1:(index_1 + 99))
      {
        sub_solute[count] = exp[index_2, (solute_num + 1)]
        sub_time[count] = exp$Time[index_2]
        count = count + 1
      }
      MA_solute[index_1] = mean(sub_solute)
      MA_Time[index_1] = mean(sub_time)
    }
    
    title = paste("Time_vs", solute_names[solute_num + 1], conc[i], sep = "_")
    plot(MA_solute ~ MA_Time, type = "l", main = title, xlab = "Time", ylab = solute_names[solute_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
  }
  
}
time_vs_solute()


