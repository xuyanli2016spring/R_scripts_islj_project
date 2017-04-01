# Name: Yanli Xu
# Date: Jan 17th, 2017
# Note: This script is used for comparing the necrotic_dist_from_CV_or_PV_vs_time of the different concentrations in one group and also the difference between different groups, the output is every plot includes all the concentrations in one group. 
# Four pages of plots, every page has n(n equals to the number of groups) plots, every plot includes all the concentrations
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_nec_time_vs_dist_from_CV_or_PV_by_updated_1B.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "necrotic.csv" files in the its directory: for example, if you want to 
# compare two groups of experiemtns, then you should put the two groups of experiments("necrotic.csv" files) in two different directories. 

# The following is an example to run this script
# > source("compare_nec_time_vs_dist_from_CV_or_PV_updated_1B.R")
# Please input the number of groups of experiments you would like to compare, for example 2: 2
# You have  2  groups of experiments, please input how many rows and columns you would like to display the plots, for example (2, 2): 1, 2
# Please input the directory of experiments of group 1: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2016_12_31_to_2017_01_01
# Please input a name to distinguish this group, for example, infusion_liver: mouse_bolus
# Please input the directory of experiments of group 2: /Users/yanlixu/Desktop/experiment_data_2017/exp_2016_12_31_to_2017_01_07/exp_2017_01_05_to_2017_01_07
# Please input a name to distinguish this group, for example, infusion_liver: culture_bolus
# Please input the number of experiments in every group, for example 5: 2
# Enter 2 concentration values separated by comma: 0.1, 0.5
# Please input 2 different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the cycleLimit of these experiments: 21600



mav <- function(x,n=51){filter(x,rep(1/n,n), sides=2)}

compare_time_vs_nec_dist <- function()
{
  #############################
  
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
  # conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  # ask user to choose the colors 
  col_str <- readline(paste("Please input ", num_of_exp_in_one_group, " different colors to represent different concentrations, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # x-axis and y-axis values
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  zone <- c("CV", "PV")
  type_dist <- c("Mean", "Median")
  
  for(z in zone)
  {
    for(t in type_dist)
    {
      ########## calculate the min and max values for the y_axis
      
      min_in_group <- c()
      max_in_group <- c()
      for (grp_num in 1:num_of_groups)
      {
        exp_min <- c()
        exp_max <- c()
        
        files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_necrotic.csv", recursive = T)
        for(exp_num in 1:length(files))
        {
          exp <- read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ",")
          exp_min[exp_num] = min(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]), na.rm = T)
          exp_max[exp_num] = max(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]), na.rm = T)  
          
        }
        min_in_group[grp_num] = min(exp_min)
        max_in_group[grp_num] = max(exp_max)
      }
      y_max = max(max_in_group)
      y_min = min(min_in_group)
      
      ############## end of calculating the min and max of y_axis
      for(grp_num in 1:num_of_groups)
      {
        # Read the files
        files <- list.files(path = exp_dirs[grp_num], pattern = "[0-9]+_necrotic.csv", recursive = T)
        plot(c(), c(), type = "l", main = paste("Nec_Time_vs_", t, "_dist_to_", z, "_", exp_group_names[grp_num], sep = ""), xlab = "time", ylab = "nec_dist", xlim = c(0, run_time), ylim = c(y_min, y_max))
        for(i in 1:length(conc))
        {
          exp <- read.csv(paste(exp_dirs[grp_num], files[i], sep= "/"), header = TRUE, sep = ",")
          lines(mav(exp[, c(paste(t, ".Dist_from_", z, sep=""))]) ~ exp$Time_of_Necrosis, col = color[i])
        }
        
        # Since all the plots use the same legends, therefore here only puts legends in the first plot to save space
        if(grp_num == 1)
        {
          
          if(z == "CV")
          {
            legend(0 , y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
          }
          else
          {
            legend((run_time*0.5), y_max, conc, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
          }
          
          
        }
        
      }# end of for loop
      
    }
    
  } # end of zone
  
}
compare_time_vs_nec_dist()

