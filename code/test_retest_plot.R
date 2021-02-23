# function to make line graphs for test-retest ROI results and calculate intraclass
# correlation coefficients 


test_retest_plot <- function(dat, task, ROI) {
	
	# DESCRIPTION
	# This function plots line graphs by scan time, showing test-retest ROI values across time
	# for each language paradigm and each ROI. It also calculate the intraclass 
	# correlation coefficients between test and retest scans. 
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# task = name of the language paradigm to use
	# ROI = name of the ROI to use 
	#
	# OUTPUT
	# a line graph with intraclass correlation coefficient 
	#
	
	
	# load libraries
	require(ggplot2)
	require(irr)
	
# plot line graph for test-retest ROI results
	p <- ggplot(dat[dat$task == task, ], 
		    aes_string(x = "scan_group", y = ROI, group = "group")) + 
		geom_point(aes(color = group), size = 1.5) + 
		geom_line(aes(color = group, group = grp), size = 1) + 
		labs(y = "% Signal Change [Speech vs. Rest]", x = " ") + 
		scale_color_manual(values = c("#e66101","#5e3c99"),
				   name = "Dx", 
				   breaks = c("ASD","TD"),
				   labels = c("ASD","TD")) + 
		guides(color = F, fill = F) +
		theme(legend.title = element_text(colour="black", size=14, face="bold"), 
		      legend.text = element_text(colour="black", size=14, face="bold")) +
		theme(plot.title = element_text(size = 10, face = "bold", hjust = 0.5))+
		theme(axis.text.x = element_text(size = 10, face = "bold", angle = 30, hjust = 1),
		      axis.text.y = element_text(size = 10, face = "bold"), 
		      axis.title.y = element_text(size = 14, face = "bold"), 
		      axis.title.x = element_blank()) +
		theme(panel.background = element_blank(),
		      panel.grid = element_blank(),
		      panel.border = element_blank(),
		      axis.text = element_blank(),
		      axis.title = element_blank(),
		      axis.line = element_line(colour = "black")) +
		ylim(-0.1, 0.2)
	
# calculate intraclass correlation coefficients
	icc_tmp <- dat[dat$task == task, ROI]
	icc_data <- cbind(icc_tmp[seq(1,length(icc_tmp),2)], icc_tmp[seq(2,length(icc_tmp),2)])
	icc_output <- icc(as.matrix(icc_data))
	icc_value <- round(icc_output$value,2)
	
# add intraclass correlation coefficients (ICC) in the line graph
	p1 <- p + annotate(geom = "text", x = 0.7, y = -0.08, fontface="bold",
			   label = paste0("ICC = ",icc_value))

# print final graph	
	print(p1)
}

