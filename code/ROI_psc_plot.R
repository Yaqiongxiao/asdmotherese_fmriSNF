# Function to make boxplots showing group differences in percent signal changes 
# between TD and ASD toddlers as well as between TD toddlers and adults across 
# three language paradigms.

ROI_psc_plot <- function(dat, ROI, constrast) {
	
	# DESCRIPTION
	# This function plots boxplot graph by group, with invidual points showing percent 
	# signal changes or t statistical values across three language paradigms.  
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# ROI = name of the ROI to use
	# constrast = character denoting contrast to plot 
	#
	# OUTPUT
	# a boxplot graph with inidividual points by group across all language paradigm
	# 
	# load libraries
	require(ggplot2)
	
	# boxplots 	
	p <- ggplot(dat, aes_string(x = "task", y = ROI, color = "group")) +
		geom_boxplot(width = 0.8,outlier.shape = NA, 
			     position = position_dodge(width = 0.9),alpha = 0.4, size=1) +
		geom_point(aes(col = group), size = 3,  position = position_jitterdodge(jitter.width = 0.3)) + 
		labs(y = "% Signal Change [Speech vs. Rest]", x = " ") +
		#guides(color = 'none') +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
		      axis.text = element_text(hjust = 0.5, size = 12, face = "bold"),
		      axis.text.x = element_text(hjust = 0.5, size = 10, face = "bold"),
		      axis.title = element_text(hjust = 0.5, size = 12, face = "bold"))+
		theme(legend.title = element_blank()) + 
		theme(panel.border = element_blank(),
		      panel.background = element_blank(),
		      panel.grid = element_blank(),
		      axis.line = element_line(colour = "black"))
	
	# customize features based on contrast
	if (constrast == "TDvsASD") {
		p1 <- p + scale_color_manual(values = c("#5e3c99","#e66101")) +
			coord_cartesian(ylim=c(-0.1,0.3)) + 
			scale_y_continuous(breaks = round(seq(-0.1,0.3, 0.1),2))
	} else if (constrast == "TDvsAdults") {
		p1 <- p + scale_color_manual(values = c("magenta", "royalblue")) + 
			coord_cartesian(ylim=c(-0.1,0.6)) + 
			scale_y_continuous(breaks = seq(-0.1,0.6, 0.1))
	}
	# print graph
	print(p1)

} 
