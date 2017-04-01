# Name: Yanli Xu
# Date: Jan 17th, 2017
# Note: This script is used for compare the nectrig_vs_time between groups, the output is one plot for every group. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_trig_by_group_updated.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "netrig.csv" files in the its directory: for example, if you want to 
# compare two groups of experiemtns, then you should put the two groups of experiemtns("nectrig.csv" files) in two different directories. 

# The following is an example to run this script
# > source("compare_time_vs_trig_by_group_updated.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# You have  2  groups of experiments, please input how many rows and columns you would like to display the plots, for example (2, 2): 1, 2
# Please input the directory of experiments of group 1: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2016_12_31_to_2017_01_01
# Please input a name to distinguish this group, for example, infusion_liver: m1
# Please input the directory of experiments of group 2: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2017_01_05_to_2017_01_07
# Please input a name to distinguish this group, for example, infusion_liver: m2
# Please input the number of experiments in every group, for example 5: 2
# Enter 2 concentration values separated by comma: 0.1, 0.5
# Please input 2 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the cycleLimit of these experiments: 21600
# You already got the plot of the first group, please input x and y coordinates to put legends: 10000, 15000

compare_time_vs_nectrig <- function()
{
  #########
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))

  # Ask the user to decide the layout of output plots
  mf_input <- readline(paste("You have ", num_of_groups, " groups of experiments, please input how many rows and columns you would like to display the plots, for example (2, 2): "))
  mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
  par(mfrow=mf)
  # ask user to input the directory and name
  exp_dirs <- c()
  exp_group_names <- c()
  for(d_num in 1:num_of_groups)
  {
    exp_dirs[d_num] <- unlist(strsplit(readline(paste("Please input the directory of experiments of group ", d_num, ": ", sep = "")), "[, ]+"))
    exp_group_names[d_num] <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
  }
  
  # Ask user how many experiments in every group
  num_of_exp_in_one_group <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of experiments in every group, for example 5: "), "[, ]+")))
  # ask user the concentration values
  conc_str <- readline(paste("Enter ", num_of_exp_in_one_group,  " concentration values separated by comma: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  # ask user to choose the colors 
  col_str <- readline(paste("Please input ", num_of_exp_in_one_group, " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  # Calculate the max value of y_axis
  max_in_group <- c()
  for (grp_num in 1:num_of_groups)
  {
    exp_max <- c()
    
    files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_nectrig.csv", recursive = T)
    for(exp_num in 1:length(files))
    {
      exp <- read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ",")
      exp_max[exp_num] = max(exp$Cumu_Nectrig)  
      
    }
    max_in_group[grp_num] = max(exp_max)
  }
  y_max = max(max_in_group)
  #########
  for(group_num in 1:num_of_groups)
  {
    # Read the files
    files <- list.files(path = exp_dirs[group_num], pattern = "[0-9]+_nectrig.csv", recursive = T)
    plot(c(), c(), type = "l", main = paste("time vs nectrig of ", exp_group_names[group_num], sep = ""), xlab = "time", ylab = "nectrig", xlim = c(0, run_time), ylim = c(0, y_max))
    for(i in 1:length(conc))
    {
      exp <- read.csv(paste(exp_dirs[group_num], files[i], sep= "/"), header = TRUE, sep = ",")
      lines(exp$Cumu_Nectrig ~ exp$Time_of_NecTrig, col = color[i])
    }

    # Since all the plots use the same legends, therefore here only puts legends in the first plot to save space
    if(group_num == 1)
    {
      legend_str <- readline("You already got the plot of the first group, please input x and y coordinates to put legends: ")
      legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      
      legend(legend_x, legend_y, conc,
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
    
  }# end of for loop
}
compare_time_vs_nectrig()