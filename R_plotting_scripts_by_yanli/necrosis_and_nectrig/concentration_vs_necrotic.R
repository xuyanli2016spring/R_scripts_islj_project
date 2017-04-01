# Name: Yanli Xu
# Date: Nov 30th, 2016
# Note: This script is used for plotting concentration vs necrosis at different time for multiple files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("concentration_vs_necrotic.R"), the script will start working
# Input file: XXXX_necrotic.csv

# The following is an example of running this script
# > source("concentration_vs_necrotic.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# Please input min and max concentrations to be bottom and upper x limits: 5, 125
# Please input the bottom and upper limits to represent the range of necrosis: 0, 15000
# Please input the cycleLimit of these experiments: 36000
# Please input 10 different colors to represent different hours, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate
# You already got the plot, please input x and y coordinates to put legends: 0, 15000

necrosis_vs_conc <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  
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
#   mf_input <- readline("Please input how many rows and columns do you want to display the plots: ")
#   par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))
  par(mfrow = c(1, 1))
  
  x_lim_str <- readline("Please input min and max concentrations to be bottom and upper x limits: ")
  x_limits <- as.numeric(unlist(strsplit(x_lim_str, "[, ]+")))
  y_lim_str <- readline("Please input the bottom and upper limits to represent the range of necrosis: ")
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  plot(c(), c(), xlim = x_limits, ylim = y_limits, xlab = "Concentration", ylab = "Necrosis", main = "concentration vs necrosis at different time")

  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  col_str <- readline(paste("Please input ", (run_time/3600), " different colors to represent different hours, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  for(hrs in 1:(run_time/3600))
  {
    final_encrosis <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      exp_sub <- subset(exp[exp$Time_of_Necrosis <= (3600 * hrs), ])
      final_encrosis[i] <-  max(exp_sub$Cumu_Necrotic)  
    }
    lines(final_encrosis ~ conc, col = color[hrs])
  }
  legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
  legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
  legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
  
  legends <- c()
  for(i in 1:(run_time/3600))
  {
    legends[i] = paste(i, "hrs", sep = "")
  }
  
  legend(legend_x, legend_y, legends,
         col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
}
necrosis_vs_conc()