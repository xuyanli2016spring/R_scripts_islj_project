# Name: Yanli Xu
# Date: Feb 5th, 2017
# Note: This script is used for comparing Glen and my profile files, I put all of my exp_profile files and Glen profile files in one directory. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_glen_and_my_exp_profile.R"), the script will start working 
# Input files: Glen's experiment profile files and my exp body files in one directory
# output created by this script: every page is for one sulte, and the plot on that page includes the lines from all the exps of mine and Glen's.

# An example to run this script


mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
compare_time_vs_solute <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_profile_mean_per_node.csv", recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " names to distinguish the experiments: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("profile_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  # Ask the user how to display the plot
  # mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: ")) 
  # Ask user to input solute names
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  col_str <- readline(paste("Please input ", length(files), " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  # 
  # solute_names <- c("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N")
  
  solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N")
  for (s in solute_names)
  {
    ######
    max_of_every_exp <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      max_of_every_exp[i] = max(mav(exp[, s]), na.rm = T)
    }
    y_max = max(max_of_every_exp)
    y_min = 0
    
    
    #####
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    ########
    
    plot(c(), c(), type = "l", main = paste("Profile_Time_vs", s, sep = "_"), xlab = "time", ylab = s, xlim = c(0, run_time), ylim = c(y_min, y_max))
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(mav(exp[, s]) ~ exp$Time, col=color[i])
      Sys.sleep(1) 
    }
    
    legend((run_time/2), y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    
  }
  
}
compare_time_vs_solute()
