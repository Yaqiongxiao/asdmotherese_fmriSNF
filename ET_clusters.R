# function to plot gaze preference for motherese across clusters 

ET_clusters <- function(dat, plotAll = FALSE) {

	# DESCRIPTION
	# This function plots violin graph by cluster/index, with invidual points 
	# showing % fixation towards motherese   
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# plotAll = if plot all clusters; plot 4 main clusters by default 
	#
	# OUTPUT
	# a violin graph with inidividual % Motherese by cluster
	# 
	# load libraries
	require(ggplot2)
	
	# if plot all clusters or not	
	if (plotAll == FALSE) {
		dat_tmp <- dat[dat$index !=5, ]
	} else {
		dat_tmp <- dat
	}
	
	p <- ggplot(dat_tmp, aes(x = index, y = `LK_.fixation.Motherese`)) + 
		geom_violin(aes(fill = index), position = "dodge",trim = T) + 
		geom_point(aes(fill = index), size = 3, alpha = 0.6, position = position_jitterdodge(jitter.width = 0.3)) + 
		stat_summary(fun = "mean", geom = "point", shape = 4, size = 7, color = "white", stroke = 2) + 
		labs(y = "% Motherese", x = "Cluster") +
		guides(color = F, fill = F) +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
		      axis.text = element_text(size = 16, face = "bold"),
		      axis.title = element_text(size = 16, face = "bold"))+
		theme(panel.border = element_blank(),
		      panel.background = element_blank(),
		      panel.grid = element_blank(),
		      axis.line = element_line(colour = "black")) +
		coord_cartesian(ylim=c(0, 100)) + 
		scale_y_continuous(breaks = seq(0, 100, 25))
	
	print(p)
}
