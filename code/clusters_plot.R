# function to plot ROI and clinical test data across clusters 

# function to plot ROI and clinical test data across clusters 

clusters_plot <- function(dat, clusters, ROI_var, clinic_var) {

	# DESCRIPTION
	# This function plots boxplot graph by cluster/index, with invidual points 
	# showing percentage signal change values (ROI variables) or clinical scores 
	# (clinical variables)   
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# ROI_var = a string denoting ROI variables
	# clinic_var = a string denoting clinical variables
	#
	# OUTPUT
	# a boxplot graph with inidividual values/scores
	# 
	# load libraries
	require(ggplot2)
	
	# organize data file 
	ROI_clinic_var <- cbind.data.frame(dat[, c("subj","group",ROI_var, clinic_var)])
	dim(ROI_clinic_var)
	
	# add clustering results
	ROI_clinic_clusters <- merge(ROI_clinic_var, clusters, by = "subj")
	
	# distribution of clusters
	nn <- table(ROI_clinic_clusters$group.x, ROI_clinic_clusters$Clustering)
	nn
	
	# add clustering index
	aa <- aggregate(ROI_clinic_clusters, by=list(ROI_clinic_clusters$Clustering), FUN = mean)
	
	ab <- order(-aa$`Motherese_RHtemporal_psc`)[c(1,3,2,4)]

	for (i in 1:max(ROI_clinic_clusters$Clustering)) {
		ROI_clinic_clusters$index[ROI_clinic_clusters$Clustering == ab[i]] <- i
	}
	
	# reorganize data file for plotting
	ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, Story_LHtemporal_psc:Motherese_RHtemporal_psc)
	head(ROI_clinic_clusters_long)
	
	ROI_clinic_clusters_long$index <- as.factor(ROI_clinic_clusters_long$index)
	ROI_clinic_clusters_long$test <- factor(ROI_clinic_clusters_long$test, 
					 levels = c("Story_LHtemporal_psc","Karen_LHtemporal_psc","Motherese_LHtemporal_psc",
					 	   "Story_RHtemporal_psc","Karen_RHtemporal_psc","Motherese_RHtemporal_psc"))
	
	## barplots of %signal changes
	ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long
	tmp <- ROI_clinic_clusters_long_run
	tmp$test <- factor(tmp$test, levels = levels(tmp$test)[c(1,4,2,5,3,6)])
	tmp$gr[grep("Story*", tmp$test)] <- "Story"
	tmp$gr[grep("Karen*", tmp$test)] <- "Karen"
	tmp$gr[grep("Motherese*", tmp$test)] <- "Motherese"
	tmp$gr <- as.factor(tmp$gr)
	
	# barplot of average %signal changes for each cluster
	library(scales)
	
	tmp1 <- aggregate(values ~ subj + index, FUN = mean, tmp[grep("LHtemporal",tmp$test),])
	tmp2 <- aggregate(values ~ subj + index, FUN = mean, tmp[grep("RHtemporal",tmp$test),])
	
	tmp3 <- cbind(tmp1,tmp2[3])
	
	colnames(tmp3)[3:4] <- c("LHtemporal", "RHtemporal")
	
	tmp4 <- gather(tmp3, test, values, 3:4)
	
	tmp5 <- tmp4[order(tmp4$subj),]
	tmp5$index <- as.factor(tmp5$index)
	
p_ROIs_barplot <- ggplot(tmp5, aes(x = index, y = values,group = test, fill = index)) + 
		geom_bar(width = 0.8, stat = "summary",fun = "mean", color = "black", position = position_dodge(width = 0.8)) + 
		geom_errorbar(width = 0.6, stat = "summary", fun.data = "mean_se", position = position_dodge(width = 0.8)) +
		labs(y = "% Signal Change [Speech vs. Rest]", x = "", title = "ROI activation") +
		#guides(fill = F) +
		theme(legend.title = element_text(colour="black", size=14, face="bold"),
		      legend.text = element_text(colour="black", size=14, face="bold")) +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
		theme(axis.text.x = element_blank(),
		      axis.ticks.x = element_blank(),
		      axis.text.y = element_text(size = 10, face = "bold"),
		      axis.title.y = element_text(size = 10, face = "bold")) +
		scale_fill_hue(name = "Cluster") + 
		theme(panel.background = element_blank(),
		      panel.grid = element_blank(),
		      panel.border = element_blank(),
		      axis.line = element_line(colour = "black")) + # remove background
		coord_cartesian(ylim=c(0.00,0.12)) + 
		scale_y_continuous(breaks = seq(0.00,0.12, 0.02))	
	
	print(p_ROIs_barplot)	


	## organize data for plotting clinical data
	ab <- colnames(dplyr::select(ROI_clinic_clusters, contains("final")))
	ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, ab[1]:ab[length(ab)])
	head(ROI_clinic_clusters_long)
	
	ROI_clinic_clusters_long$index <- as.factor(ROI_clinic_clusters_long$index)
	
	ROI_clinic_clusters_long$test <- as.factor(ROI_clinic_clusters_long$test)
	
	# plot clinical data

	titles <- c("ADOS SA", "ADOS Total", "ADOS RRB", "Mullen ELC", "Mullen ELT",
		    "Mullen FMT", "Mullen RLT", "Mullen VRT", "Vineland ABC", 
		    "Vineland Communication", "Vineland Daily Living Skills", 
		    "Vineland Domain Total", "Vineland Motor", "Vineland Socialization")

	
	for (i in 1:length(titles)) {
		test <- levels(ROI_clinic_clusters_long$test)[i]
		tmp <- ROI_clinic_clusters_long[ROI_clinic_clusters_long$test == test, ]
		tmp <- tmp[tmp$values != 0, ]
		
		p_clinic <- ggplot(tmp, aes(x = index, y = values, fill = index)) +
			geom_boxplot(width = 0.9,outlier.shape = NA) +
			geom_point(aes(col = index), color = "black", size = 3, alpha = 0.6, position = position_jitterdodge(dodge.width = 1)) +
			labs(y = "Scores", x = "Cluster", title = titles[i]) +
			guides(fill = F) +
			theme(legend.title = element_text(colour="black", size=14, face="bold"),
			      legend.text = element_text(colour="black", size=14, face="bold")) +
			theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
			theme(axis.text.y = element_text(size = 16, face = "bold"),
			      axis.title.y = element_text(size = 16, face = "bold")) +
			theme( axis.text.x = element_blank(),  axis.title.x = element_blank(),
			       axis.ticks.x = element_blank()) + 
			theme(panel.background = element_blank(),
			      panel.grid = element_blank(),
			      panel.border = element_blank(),
			      axis.line = element_line(colour = "black"))
		
	print(p_clinic)	

	}
	return(list(nn,ROI_clinic_clusters,ROI_clinic_clusters_long_run))
}
