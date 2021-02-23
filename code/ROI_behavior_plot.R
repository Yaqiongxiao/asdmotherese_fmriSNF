# function to plot ROI-behavior correlation regression

ROI_behavior_plot <- function(dat, var, ROI, xlab) {
	
	# DESCRIPTION
	# This function plots scatterplot showing correlation regression between
	# ROI percent signal changes and behavior (i.e.,Vineland communication 
	# and socialization scores) and the model fitted line. It also calculate 
	# Pearson’s correlation coefficients. 
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# var = name of the variable to use
	# ROI = name of the ROI to use 
	# xlab = label to put on x-axis
	#
	# OUTPUT
	# a scatterplot graph with model fitted line and correlation coefficient
	#
	# load libraries
	require(ggplot2)
	require(lme4)
	
	# obtain model fits parameters
	flm <- paste0(ROI, "~", var, "+ scan_age + gender + meanFD + (1|subjid) + (1|task)")
	model <- lmer(flm, dat)
	fixed.m1 <- data.frame(fixef(model))
	intercept <- fixed.m1[1,1]
	slope = fixed.m1[2,1]
	
	# plot
	p <- ggplot(dat, aes_string(x = var, y = ROI)) +
		geom_point(aes(color = group),position = "jitter", size = 3) +
		geom_abline(intercept =, slope = slope, size = 2, col = "black") + 
		#geom_smooth(method = "lm", alpha = 0.5, size = 2, col = "black") + 
		guides(color = F) + 
		labs(x = xlab, y = "% Signal Change [Speech vs. Rest]") +
		scale_color_manual(values = c("#e66101", "#5e3c99")) + 
		theme(legend.title = element_text(colour="black", size=16, face="bold"), 
		      legend.text = element_text(colour="black", size=16, face="bold")) +
		theme(plot.title = element_text(hjust = 0.5))+
		theme(axis.text = element_text(size = 16, face = "bold"),
		      axis.title = element_text(size = 18, face = "bold")) +
		theme(panel.background = element_blank(),
		      panel.border = element_blank(),
		      panel.grid = element_blank(),
		      axis.line = element_line(colour = "black")) + 
		# scale_color_manual(values = c("#ff0000","#0000ff")) + # c("#FFCC33","#e66101","#3399FF","#009900")
		coord_cartesian(ylim=c(-0.1,0.2), xlim=c(30, 130)) + 
		scale_y_continuous(breaks = seq(-0.1,0.2, 0.1)) +
		scale_x_continuous(breaks = seq(30,130, 20))
	
	# Compute Pearson’s correlation coefficients
	coreff <- cor.test(dat[, ROI], dat[, var])
	tmp <- round(coreff$estimate,2)
	
	# add correlation coefficients in the graph
	if (round(coreff$p.value,4) < 0.005 & round(coreff$p.value,4) > 0.001) {
		p1 <- p + annotate(geom = "text", x = 35, y = 0.2, size = 5,fontface="bold",
				   label = paste0("r = ",tmp,"**")) 
	} else if (round(coreff$p.value,4) < 0.001) {
		p1 <- p + annotate(geom = "text", x = 35, y = 0.2, size = 5,fontface="bold",
				   label = paste0("r = ",tmp,"***")) 	
	}

	
	# print final graph
	print(p1)

}

