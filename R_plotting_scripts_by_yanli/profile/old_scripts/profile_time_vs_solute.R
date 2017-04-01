# Name: Yanli Xu
# Date: Nov 21th, 2016
# Note: This script is used for plotting time vs solute for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("profile_time_vs_solute.R"), the script will start working 
# Input file: XXXX-XX-XX-XXXX-XXXX_profile_mean_per_node.csv
# Plot all the time vs profile_solute ("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N" )

# The following is an example of running this script
# > source("profile_time_vs_solute.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/around_2016_11_01/nec_profile
# Enter 9 concentration values separated by comma: 5, 10, 15,  25, 35, 50, 75, 100, 125
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 36000
# Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: 1


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
  
  #  predict the y-axis range by the last concentration
  exp <- eval(parse(text = exp_names[1]))
  # Ask the user which solute to plot
  solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
  
  # When predict the max value of y-axis: if the solute is "oAPAP", then predict the max value based on the last experiment, since the last experiemt 
  # hss greater value than other experiments.
  if(solute_num == 4)
  {
    exp <- eval(parse(text = exp_names[length(files)]))
  }
  
  #######################
  # predict the max value of the y-axis  
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
  #########################
  # plot time vs solute of all the concentrations
  for(i in 1:length(conc))
  {
    # when the solume lumber is 2, 3, 6, 7 and 8, the moving average isn't required to calculte
    if(solute_num == 2 | solute_num == 3 | solute_num == 6 | solute_num == 7 | solute_num == 8)
    {
      exp <- eval(parse(text = exp_names[i]))
      title = paste("Time_vs", solute_names[solute_num + 1], conc[i], sep = "_")
      plot(exp[, (solute_num + 1)] ~ exp$Time, type = "l", main = title, xlab = "Time", ylab = solute_names[solute_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
    }
    else # when the solume number is 1, 4 and 5, calculate the moving average
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
 
}
time_vs_solute()
