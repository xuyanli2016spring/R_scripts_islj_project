# Name: Yanli Xu
# Date: Feb 14th, 2017
# To compare nectrig events of mouse group and culture group at a specific time
# create .csv files

# Example to use this script
# > source("compare_mouse_and_culture_dose_response_at_specific_time.R")
# Please input the number of groups that you are going to compare: 2
# Please input 2 different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, red
# Please input the time at which you want to calculte the trigger events: 20000
# Please input the number of monte carlo trials: 3
# Please input working directory 1: /Users/yanlixu/Desktop/experiment_data_2017/exp_2017_01_15_to_2017_01_21/mouse 
# Please input a name to distinguish this group, for example, infusion_liver: mouse
# Enter 6 doseRatios: 0.025, 0.050, 0.1, 0.25, 0.5, 0.75
# Please input min and max doses to represent bottom and upper x limits: 0, 0.8
# Please input the bottom and upper limits to represent the range of nectrig percentage: 0, 0.5
# Please input working directory 2: /Users/yanlixu/Desktop/experiment_data_2017/exp_2017_01_15_to_2017_01_21/culture 
# Please input a name to distinguish this group, for example, infusion_liver: culture
# Enter 6 doseRatios: 0.025, 0.050, 0.1, 0.25, 0.5, 0.75

compare_trig_events <- function()
{
  #########
  num_of_groups_str <- readline("Please input the number of groups that you are going to compare: ")
  num_of_groups <- as.numeric(unlist(strsplit(num_of_groups_str, "[, ]+")))
  
  col_str <- readline(paste("Please input ", num_of_groups, " different colors to represent different groups, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  # ask user at what time to calculte the trigger events
  run_time_str <- readline("Please input the time at which you want to calculte the trigger events: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  # ask user the number of monte carlo trials
  mc_str <- readline("Please input the number of monte carlo trials: ")
  num_mc <- as.numeric(unlist(strsplit(mc_str, "[, ]+")))
  
  exp_group_name <- c()
  
  for(group_num in 1:num_of_groups)
  {
    # Ask user to input the working directory
    work_dir <- unlist(strsplit(readline(prompt=paste("Please input working directory ", group_num, ": ", sep = "")), "[ ]+"))
    # ask user the group name
    exp_group_name[group_num] <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
    
    # Read the files
    files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
    
    # Ask user to input the doseRatios
    dose_str <- readline(paste("Enter ", length(files),  " doseRatios: ", sep = ""))
    doses <- unlist(strsplit(dose_str, "[, ]+"))
    
    trig_events <- c()
    trig_rate <- c()
    for(i in 1:length(files))
    {
      data <- read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ",")
      new_data <- subset(data, Time_of_NecTrig <= run_time)
      trig_events[i] = max(new_data$Cumu_Nectrig)
      trig_rate[i] = (max(new_data$Cumu_Nectrig))/(num_mc*14000)
    }
    if(group_num == 1)
    {
      x_lim_str <- readline("Please input min and max doses to represent bottom and upper x limits: ")
      x_limits <- as.numeric(unlist(strsplit(x_lim_str, "[, ]+")))
      y_lim_str <- readline("Please input the bottom and upper limits to represent the range of nectrig percentage: ")
      y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
      plot(trig_rate ~ doses, type = "l",  xlim = x_limits, ylim = y_limits, main = paste("Dose Response of ", num_of_groups, " groups at time ", run_time, sep = ""), xlab = "Doses", ylab = "Nectrig_Percentage", col = color[group_num])
    }
    else
    {
      lines(trig_rate ~ doses, col = color[group_num])
    }

  }
  
  legend(x_limits[1], y_limits[2], exp_group_name, col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
}
compare_trig_events()