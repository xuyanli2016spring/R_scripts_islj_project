# Name: Yanli Xu
# Date: Dec 16th, 2016
# Note: This script is used for compare the necrotic_dist_from_CV_or_PV_vs_time of the same concentrations among different groups, the output is one plot for every concentration. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_nec_time_vs_dist_from_CV_or_PV_by_conc.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "necrotic.csv" files in the its directory: for example, if you want to 
# compare two groups of experimentns, then you should put the two groups of experiemtns("necrotic.csv" files) in two different directories. 

# The following is an example to run this script
# > source("compare_nec_time_vs_dist_from_CV_or_PV_by_conc.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# Please input the directory of experiments of group 1: /Users/yanlixu/Desktop/experiment_data/data_from_2016_10_24_to_2016_11_05
# Please input a name to distainguish this group, for example, infusion_liver: inf_liver
# Please input the directory of experiments of group 2: /Users/yanlixu/Desktop/experiment_data/exp_2016_11_24_to_2016_12_06/data_from_2016_11_24_to_2016_12_06
# Please input a name to distainguish this group, for example, infusion_liver: inf_culture
# Please input 2 different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the number of experiments in every group, for example 9: 9
# You have  9  experiments in every group, please input how many rows and columns you would like to display the plots, for example (3, 3): 3, 3
# Enter 9 concentration values in every group and separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please choose one distance to plot, your choices are CV and PV: CV
# Please choose one type of distance, your choices are Mean and Median: Mean
# Please input the cycleLimit of these experiments: 36000
# Please input bottom and upper limits of y-axis to represent the range of Mean distances from CV, for example 0, 40: 4, 37
# You already got the first plot, please input x and y coordinates to put legends: 0, 37

mav <- function(x,n=301){filter(x,rep(1/n,n), sides=2)}
compare_time_vs_nec_dist <- function()
{
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))
  # Ask user to input the directories for all the groups of experiments
  exp_dirs <- c()
  exp_group_names <- c()
  for(d_num in 1:num_of_groups)
  {
    exp_dirs[d_num] <- unlist(strsplit(readline(paste("Please input the directory of experiments of group ", d_num, ": ", sep = "")), "[, ]+"))
    exp_group_names[d_num] <- unlist(strsplit(readline(paste("Please input a name to distainguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
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
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  
  # Ask user from which distance, CV or PV?
  zone_str <- readline("Please choose one distance to plot, your choices are CV and PV: ")
  zone <- unlist(strsplit(zone_str, "[, ]+"))
  # Ask user choose median or mean dist
  type_str <- readline("Please choose one type of distance, your choices are Mean and Median: ")
  type_dist <- unlist(strsplit(type_str, "[, ]+"))
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  y_lim_str <- readline(paste("Please input bottom and upper limits of y-axis to represent the range of ",  type_dist," distances from ", zone,", for example 0, 40: ", sep = ""))
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
#   
  # Get the max and min values of the distance from CV/PV
  #############
#   max_of_every_group <- c()
#   min_of_every_group <- c()
#   for(group_num in 1:num_of_groups)
#   {
#     files <- list.files(path = exp_dirs[group_num], pattern = "[0-9]+_necrotic.csv", recursive = T)
#     max_values <- c()
#     min_values <- c() 
#     for(exp_num in 1:length(files))
#     {
#       data <- read.csv(paste(exp_dirs[group_num], files[exp_num], sep= "/"), header = TRUE, sep = ",")
#       max_values[exp_num] <- max(mav(data[, c(paste(type_dist, ".Dist_from_", zone, sep=""))]), na.rm=TRUE)
#       min_values[exp_num] <- min(mav(data[, c(paste(type_dist, ".Dist_from_", zone, sep=""))]), na.rm=TRUE)
#     }
#     max_of_every_group[group_num] <- max(max_values)
#     min_of_every_group[group_num] <- min(min_values)
#   }
#   y_max = max(max_of_every_group)
#   y_min = min(min_of_every_group)

#  ################ 
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
    plot(c(), c(), type = "l", main = paste("Nec_Time_vs_", type_dist, "_dist_to_", zone, "_", conc[exp_num], sep = " " ), xlab = "time", ylab = "necrotic_dist", xlim = c(0, run_time), ylim = y_limits)
    
#     for(i in 1:length(exp_names))
#     {
#       exp <- eval(parse(text = exp_names[i]))
#       lines(mav(exp[, c(paste(type_dist, ".Dist_from_", zone, sep=""))]) ~ exp$Time_of_Necrosis, col = color[i])
#     }
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      moving_dist <- c()
      moving_time <- c()
      for(index_1 in 1: (nrow(exp)-99))
      {
        sub_dist = c()
        sub_time = c()
        count = 1
        for(index_2 in index_1:(index_1 + 99))
        {
          sub_dist[count] = exp[index_2, c(paste(type_dist, ".Dist_from_", zone, sep=""))]
          sub_time[count] = exp$Time_of_Necrosis[index_2]
          count = count + 1
        }
        if(type_dist == "Mean")
        {
          moving_dist[index_1] = mean(sub_dist)
          moving_time[index_1] = mean(sub_time)
        }
        else if(type_dist == "Median")
        {
          moving_dist[index_1] = median(sub_dist)
          moving_time[index_1] = median(sub_time)
        }
      }
      lines(moving_dist ~ moving_time, col = color[i])
    } # end of for
    if(exp_num == 1)
    {
      legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
      legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
      legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
      
      legend(legend_x, legend_y, exp_group_names,
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }# end of if
  }
}
compare_time_vs_nec_dist()
