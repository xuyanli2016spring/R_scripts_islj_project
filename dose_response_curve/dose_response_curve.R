
# Name: Yanli Xu
# Date: Apr 4th, 2017
# Note: This script creates a dose response curve of mouse and culture experiments.
# Note: ggplot2 library is needed


culture_doses <- c(0.038, 0.045, 0.050, 0.170, 0.200, 0.220, 0.270, 0.300, 0.340, 0.350, 0.360, 0.370, 0.380, 0.390, 0.400, 0.440, 0.450, 0.460, 0.500, 0.600, 0.700, 0.800, 0.900, 1.000)
culture_response <- c(0.005119048, 0.006023810, 0.008464286, 0.051809524, 0.065309524, 0.071726190, 0.106851190, 0.140535714, 0.162523810, 0.177630952, 0.181666667, 0.182047619, 0.191488095, 0.207047619, 0.214642857, 0.240476190, 0.252059524, 0.268297619, 0.287845238, 0.372708333, 0.384726190, 0.408797619, 0.405619048, 0.435285714)
data2 <- data.frame(culture_doses, culture_response)

# for adding minor trick mark
insert_minor <- function(major_labs, n_minor) {labs <- 
  c( sapply( major_labs, function(x) c(x, rep("", 3) ) ) )
labs[1:(length(labs)-n_minor)]}

ggplot() + 
  geom_line(data = data1, aes(x = data1$mouse_doses, y = data1$mouse_response, color = "red"), linetype = 6) +
  geom_line(data = data2, aes(x = data2$culture_doses, y = data2$culture_response, color = "blue"), linetype = 6) +
  xlab("doses") + ylab("nectrig percentage") +
  ggtitle("mouse and culture dose response curve") + 
  theme(axis.text.x = element_text(face = "bold", color = "red", size = 12, angle = 45), axis.text.y = element_text(face = "bold", color = "red", size = 12, angle = 45) ) +
  theme(axis.line = element_line(color = "black", size = 1, linetype = "solid")) +
  theme(legend.position="top", panel.background = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=18, hjust=0)) +
  theme(axis.title = element_text(family = "Trebuchet MS", color="black", face="bold", size=14)) +
  scale_x_continuous(breaks = seq(0,1,by=0.025), labels = insert_minor(seq(0, 1, 0.1), 3),  limits = c(0, 1), expand = c(0.01,0.01)) +
  scale_y_continuous(breaks = seq(0,0.5,by=0.025), labels = insert_minor(seq(0, 0.5, 0.1), 3),  limits = c(0, 0.5), expand = c(0.01,0.01)) +
  theme(legend.title = element_text(colour="blue", size=10, face="bold"), legend.text = element_text(colour="blue", size=10, face="bold"),legend.background = element_rect(fill="lightblue", size=0.5, linetype="solid"), legend.title = element_blank()) +
  scale_color_manual(labels = c("mouse", "culture"), values = c("red", "blue")) +
  guides(color=guide_legend("Exp_Type"))
