# Name: Yanli Xu
# Date: Jan 5th, 2017
# Note: This script is used for compare the solute vs time in medium/body among different groups, the output is one plot for one solute of all the concentrations in every group. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_solute_in_medium_or_body_B.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "medium.csv" files in its directory: for example, if you want to 
# compare two groups of experiments, then you should put the two groups of experiments("medium.csv" or "body.csv" files) in two different directories. 
# One example created by this script: 8 pages of plots, every page is for one sulte and have 2 plots, every plot includes the lines from all the concentrations in one group

# An example to run this script


mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
compare_time_vs_solute <- function()
{
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))
  # Ask user to input the directories for all the groups of experiments
  exp_dirs <- c()
  exp_group_names <- c()
  for(d_num in 1:num_of_groups)
  {
    exp_dirs[d_num] <- unlist(strsplit(readline(paste("Please input the directory of experiments of group ", d_num, ": ", sep = "")), "[, ]+"))
    exp_group_names[d_num] <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
  }
  
  # Ask the user to input the number of experiments in every group
  num_of_exp_perGroup <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of experiments in every group, for example 9: "), "[, ]+")))
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", num_of_exp_perGroup,  " concentration values in every group and separated by comma: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  # Ask user to input one color for every group
  col_str <- readline(paste("Please input ", num_of_exp_perGroup, " different colors to represent different experiments, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask the user to decide the layout of output plots
  mf_input <- readline(paste("You have ", num_of_groups, " plots for every solute, please input how many rows and columns you would like to display the plots, for example (3, 3): "))


  
  #   # Ask the user which solute to plot
  #   solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
  #   solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
  #   solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  #   y_lim_str <- readline(paste("Please input the bottom and upper value of y-axis to represent the min and max solute value, for example 0, 10: ", sep = ""))
  #   y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  ##############
  solute_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N")
  
  for(solute_num in 1:length(solute_names)) # This for loop is for every solute
  {
    
    ##### Begining of "Decide the y_min and y_max"
    max_of_every_group <- c()
    for (grp_num in 1:num_of_groups) # for every group group 
    {
      # Start of "Read all the files in one group"
      exp_names <- c()
      for(i in 1:length(conc))
      {
        exp_names[i] <- paste("medium_", i, sep="")
        files <- list.files(path = exp_dirs[grp_num], pattern = "mean_medium|mean_body", recursive = T)
        assign(exp_names[i], read.csv(paste(exp_dirs[grp_num], files[i], sep= "/"), header = TRUE, sep = ","))
      }
      # End of "Read all the files in one group"
      max_for_every_exp <- c()
      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        max_for_every_exp[i] = max(mav(exp[, (solute_num + 1)]), na.rm = TRUE)
      }
      max_of_every_group[grp_num] = max(max_for_every_exp)
    }
    y_max = max(max_of_every_group)
    y_min = 0
    # This is for the cases that all the values are "0"
    if(y_max == 0)
    {
      y_min = -1
      y_max = 1
    }
    ##### End of "Decide the y_min and y_max"
    mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
    par(mfrow=mf)
    ####
    for (grp_num in 1:num_of_groups) # read all the files in every group 
    {
      
      exp_names <- c()
      for(i in 1:length(conc))
      {
        exp_names[i] <- paste("medium_", i, sep="")
        files <- list.files(path = exp_dirs[grp_num], pattern = "mean_medium|mean_body", recursive = T)
        assign(exp_names[i], read.csv(paste(exp_dirs[grp_num], files[i], sep= "/"), header = TRUE, sep = ","))
      }
      plot(c(), c(), type = "l", main = paste("Time_vs", solute_names[solute_num], exp_group_names[grp_num], sep = "_"), xlab = "time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))

      for(i in 1:length(conc))
      {
        exp <- eval(parse(text = exp_names[i]))
        lines(mav(exp[, (solute_num + 1)]) ~ exp$Time, col = color[i])
      } 
      if(grp_num == 1)
      {
#         legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
#         legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#         legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
        # legend(legend_x, legend_y, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      legend(0, y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
      }

    }
    

#     # Start plotting plot
#     for(exp_num in 1:num_of_exp_perGroup) # This for loop is for every concentration in every group
#     {
#       # read the files in every group
#       exp_names <- c()
#       for (grp_num in 1:num_of_groups)
#       {
#         
#         exp_names[grp_num] <- paste("medium_", grp_num, sep="")
#         files <- list.files(path = exp_dirs[grp_num], pattern = "mean_medium|mean_body", recursive = T)
#         assign(exp_names[grp_num], read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ","))
#       }
#       
#       plot(c(), c(), type = "l", main = paste("Time_vs", solute_names[solute_num], conc[exp_num], sep = "_"), xlab = "time", ylab = solute_names[solute_num], xlim = c(0, run_time), ylim = c(y_min, y_max))
#       
#       for(i in 1:length(exp_names))
#       {
#         exp <- eval(parse(text = exp_names[i]))
#         lines(mav(exp[, (solute_num + 1)]) ~ exp$Time, col = color[i])
#       }
#       if(exp_num == 1)
#       {
#         legend(0, y_max, exp_group_names, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
#       }
#     }# end of for loop for every concentration
    
    
  } # end of for loop for every solute
  
}
compare_time_vs_solute()
