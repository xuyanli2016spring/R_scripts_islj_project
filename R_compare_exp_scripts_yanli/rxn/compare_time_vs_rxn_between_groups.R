# Name: Yanli Xu
# Date: Dec 25th, 2016
# Note: This script is used for compare the rxn_vs_time of the same concentrations among different groups, the output is one plot for every concentration. 
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("compare_time_vs_rxn_between_groups.R"), the script will start working 
# Input files: To compare n groups of experiments, every group of experiments has m "rxnProduct_zone.csv" files in the its directory: for example, if you want to 
# compare two groups of experimentns, then you should put the two groups of experiemtns("rxnProduct_zone.csv" files) in two different directories. 

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
  conc <- unlist(strsplit(conc_str, "[, ]+"))
  
  # Ask user the zone number
  zone_str <- readline("Please enter the zone number you would like to plot, you may choose from 0, 1 and 2: ")
  zone_num <- as.numeric(unlist(strsplit(zone_str, "[, ]+")))
 
  # rxn_names <- c("S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
  # Ask the user which reaction product to plot
  rxn_str <- readline("Please choose a number for the rxn you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N, 9 for GSH_Depletion: ")
  rxn_num <- as.numeric(unlist(strsplit(rxn_str, "[, ]+")))
  
#   # Ask the user which solute to plot
#   solute_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
#   solute_str <- readline("Please choose a number for the solute you would like to plot, 1 for S, 2 for nMD, 3 for MitoDD, 4 for oAPAP, 5 for G, 6 for Marker, 7 for Repair, 8 for N: ")
#   solute_num <- as.numeric(unlist(strsplit(solute_str, "[, ]+")))
#   
  # x-axis value
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  # y-axis value
  
  max_of_every_group <- c()
  for(group_num in 1:num_of_groups)
  {
    files <- list.files(path = exp_dirs[group_num], pattern =  paste("[0-9]+_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
    max_values <- c()
    for(exp_num in 1:length(files))
    {
      data <- read.csv(paste(exp_dirs[group_num], files[exp_num], sep= "/"), header = TRUE, sep = ",")
      max_values[exp_num] <- max(mav(data[, (rxn_num + 1)]), na.rm=TRUE)
    }
    max_of_every_group[group_num] <- max(max_values)
  }
  y_max = max(max_of_every_group)
  
#   y_lim_str <- readline(paste("Please input the bottom and upper value of y-axis to represent the min and max value of the rxn product in zone ", zone_num, " for example 0, 10: ", sep = ""))
#   y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  for(exp_num in 1:num_of_exp_perGroup)
  {
    # read the files in every group
    exp_names <- c()
    for (grp_num in 1:num_of_groups)
    {
      exp_names[grp_num] <- paste("rxn_", grp_num, sep="")
      files <- list.files(path = exp_dirs[grp_num], pattern =  paste("[0-9]+_rxnProduct_zone-", zone_num, ".csv", sep = ""), recursive = T)
      assign(exp_names[grp_num], read.csv(paste(exp_dirs[grp_num], files[exp_num], sep= "/"), header = TRUE, sep = ","))
    }
    
    exp <- eval(parse(text = exp_names[1]))
    rxn_names <- names(exp)    # "Time"   "S"      "nMD"    "MitoDD" "oAPAP"  "G"      "Marker" "Repair" "N" 
    plot(c(), c(), type = "l", main = paste("Time_vs", rxn_names[rxn_num + 1], conc[exp_num], sep = "_"), xlab = "time", ylab = rxn_names[rxn_num + 1], xlim = c(0, run_time), ylim = c(0, y_max))
    
    for(i in 1:length(exp_names))
    {
      exp <- eval(parse(text = exp_names[i]))
      lines(mav(exp[, (rxn_num + 1)]) ~ exp$Time, col = color[i])
    }
    if(exp_num == 1)
    {
#       legend_str <- readline("You already got the first plot, please input x and y coordinates to put legends: ")
#       legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
#       legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
#       
      legend(0, y_max, exp_group_names,
             col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
  }
}
compare_time_vs_solute()
