# Name: Yanli Xu
# Date: Dec 15th, 2016
# Note: This script is used for compare the necrosis_vs_time between groups, the output is one plot for every group. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_nec_by_group.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "necrotic.csv" files in the its directory: for example, if you want to 
# compare two groups of experiemtns, then you should put the two groups of experiemtns("necrotic.csv" files) in two different directories. 

# The following is an example to run this script
# > source("compare_time_vs_nec_by_group.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# You have  2  groups of experiments, please input how many rows and columns you would like to display the plots, for example (2, 2): 1, 2
# Please input the cycleLimit of these experiments: 36000
# Please input bottom and upper limits of y-axis to represent the range of necrosis, for example 0, 20000: 0, 18000
# Please input the working directory of group 1: /Users/yanlixu/Desktop/experiment_data/data_from_2016_10_24_to_2016_11_05
# Please give a name to distinguish this group of exp, for example, liver infusion: liver infusion
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple
# You already got the plot of the first group, please input x and y coordinates to put legends: 0, 18000
# Please input the working directory of group 2: /Users/yanlixu/Desktop/experiment_data/exp_2016_11_24_to_2016_12_06/data_from_2016_11_24_to_2016_12_06
# Please give a name to distinguish this group of exp, for example, liver infusion: culture infusion
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please input 9 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple

compare_time_vs_necrosis <- function()
{
  #########
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))
  # Ask the user to decide the layout of output plots
  mf_input <- readline(paste("You have ", num_of_groups, " groups of experiments, please input how many rows and columns you would like to display the plots, for example (2, 2): "))
  mf <- as.numeric(unlist(strsplit(mf_input, "[, ]+")))
  par(mfrow=mf)
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  y_lim_str <- readline(paste("Please input bottom and upper limits of y-axis to represent the range of necrosis, for example 0, 20000: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))

  #########
  for(group_num in 1:num_of_groups)
  {
    # Ask user to input the working directory
    work_dir <- unlist(strsplit(readline(prompt=paste("Please input the working directory of group ", group_num, ": ",sep = "")), "[ ]+"))
    # Ask user to give a name to these experiments 
    group_name <- unlist(strsplit(readline(prompt="Please give a name to distinguish this group of exp, for example, liver infusion: "), "[,.]+"))
    
    # Read the files
    files <- list.files(path = work_dir, pattern = "[0-9]+_necrotic.csv", recursive = T)
    # Ask user to input the concentrations
    conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
    conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
    exp_names <- c()
    for(i in 1:length(conc))
    {
      exp_names[i] <- paste("necrotic_", conc[i], sep="")
      assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
    }
    # ask user to choose the colors 
    col_str <- readline(paste("Please input ", length(files), " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
    color <- unlist(strsplit(col_str, "[, ]+"))
    

    # plot 
    plot(c(), c(), type = "l", main = paste("time vs necrosis of ", group_name, sep = ""), xlab = "time", ylab = "necrosis", xlim = c(0, run_time), ylim = y_limits)
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(exp$Cumu_Necrotic ~ exp$Time_of_Necrosis, col = color[i])
    }
    # Since all the plots use the same legends, therefore here only puts legends in the first plot to save space
    if(group_num == 1)
    {
      legend_str <- readline("You already got the plot of the first group, please input x and y coordinates to put legends: ")
      legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      
      legend(legend_x, legend_y, as.character(conc),
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
    
  }# end of for loop
}
compare_time_vs_necrosis()