# Name: Yanli Xu
# Date: Jan 17th, 2017
# Note: This script is used for comparing the necrotic_vs_time of the same concentrations among different groups, the output is one plot for every concentration.
# Every plot includes all the lines from different groups at a specific contration
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_nec_by_conc_updated.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "necrotic.csv" files in the its directory: for example, if you want to 
# compare two groups of experiemtns, then you should put the two groups of experiemtns("necrotic.csv" files) in two different directories. 

# The following is an example to run this script
# > source("compare_time_vs_nec_by_conc_updated.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# Please input the directory of experiments of group 1: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2016_12_31_to_2017_01_01
# Please input a name to distinguish this group, for example, infusion_liver: m1
# Please input the directory of experiments of group 2: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2017_01_05_to_2017_01_07
# Please input a name to distinguish this group, for example, infusion_liver: m2
# Please input 2 different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the number of experiments in every group, for example 9: 2
# You have  2  experiments in every group, please input how many rows and columns you would like to display the plots, for example (3, 3): 1, 2
# Enter 2 concentration values in every group and separated by comma: 0.1, 0.5
# Please input the cycleLimit of these experiments: 21600
# You already got the first plot, please input x and y coordinates to put legends: 0, 25000

compare_time_vs_nec <- function()
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
  # Ask user to input one color for every group
  col_str <- readline(paste("Please input ", num_of_groups, " different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # Ask the user to input the number of experiments in every group
  num_of_exp_perGroup <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of experiments in every group, for example 9: "), "[, ]+")))
  # Ask the user to decide the layout of output plots
  mf_input <- readline(paste("You have ", num_of_exp_perGroup, " experiments in every group, please input how many rows and columns you would like to display the plots, for example (3, 3): "))
  mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
  par(mfrow=mf)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", num_of_exp_perGroup,  " concentration values in every group and separated by comma: ", sep = ""))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  # Calculate the max value of y_axis
  max_in_group <- c()
  for (grp_num in 1:num_of_groups)
  {
    exp_max <- c()
    
    files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_necrotic.csv", recursive = T)
    for(exp_num in 1:length(files))
    {
      exp <- read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ",")
      exp_max[exp_num] = max(exp$Cumu_Necrotic)  
      
    }
    max_in_group[grp_num] = max(exp_max)
  }
  y_max = max(max_in_group)
  
  # end of calculation of the max y_value
  
  # plot every concentration
  for(exp_num in 1:num_of_exp_perGroup)
  {
    # read the files in every group
    exp_names <- c()
    for (grp_num in 1:num_of_groups)
    {
      exp_names[grp_num] <- paste("necrotic_", grp_num, sep="")
      files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_necrotic.csv", recursive = T)
      assign(exp_names[grp_num], read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ","))
    } # end of for
    # plot the files
    plot(c(), c(), type = "l", main = paste("time vs necrotic of ", conc[exp_num], sep = ""), xlab = "time", ylab = "necrotic", xlim = c(0, run_time), ylim = c(0, y_max))
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(exp$Cumu_Necrotic ~ exp$Time_of_Necrosis, col = color[i])

    } # end of for
    if(exp_num == 1)
    {
      legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
      legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      
      legend(legend_x, legend_y, exp_group_names,
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }# ed of if
  } # end of for
  
}
compare_time_vs_nec()
