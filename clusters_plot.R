# function to plot ROI and clinical test data across clusters 

clusters_plot <- function(dat, clusters, ROI_var, clinic_var, plotAll = FALSE) {

	# DESCRIPTION
	# This function plots boxplot graph by cluster/index, with invidual points 
	# showing percentage signal change values (ROI variables) or clinical scores 
	# (clinical variables)   
	# 
	# INPUT ARGUMENTS
	# dat = data frame to process
	# ROI_var = a string denoting ROI variables
	# clinic_var = a string denoting clinical variables
	# plotAll = if plot all clusters or not; plot 4 main clusters by default 
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
	
	ab <- order(-aa$`Motherese_RHtemporal_psc`)

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
	
	if (plotAll == FALSE) {
		ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long[ROI_clinic_clusters_long$index !=5, ]
	} else {
		ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long
	}
	
	# plot fMRI/ROI data 
	p_ROIs <- ggplot(ROI_clinic_clusters_long_run, aes(x = test, y = values, fill = index)) + 
		geom_boxplot(outlier.shape = NA)+ 
		geom_point(aes(col = index), color = "black", size = 1, alpha = 0.8, 
			   position = position_jitterdodge()) + 
		labs(y = "% Signal Change [Speech vs. Rest]", x = "ROI results") +
		theme(legend.title = element_text(colour="black", size=14, face="bold"),
		      legend.text = element_text(colour="black", size=14, face="bold")) +
		theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
		theme(axis.text.x = element_text(size = 10, face = "bold", angle = 40, hjust = 1),
		      axis.text.y = element_text(size = 10, face = "bold"),
		      axis.title.y = element_text(size = 12, face = "bold"),
		      axis.title.x = element_blank()) +
		theme(panel.background = element_blank(),
		      panel.grid = element_blank(),
		      panel.border = element_blank(),
		      axis.line = element_line(colour = "black")) + # remove background
		scale_x_discrete(labels=c("Story_LHtemporal_psc" = "Story language left temporal",
					  "Story_RHtemporal_psc" = "Story language right temporal",
					  "Karen_LHtemporal_psc" = "Karen language left temporal",
					  "Karen_RHtemporal_psc" = "Karen language right temporal",
					  "Motherese_LHtemporal_psc" = "Motherese left temporal",
					  "Motherese_RHtemporal_psc" = "Motherese right temporal")) +
		coord_cartesian(ylim=c(-0.1,0.25)) + 
		scale_y_continuous(breaks = seq(-0.1,0.25, 0.1))
		
	print(p_ROIs)
	
	# organize data for plotting clinical data
	ROI_clinic_clusters_long <- gather(ROI_clinic_clusters, test, values, final_ados_CoSoTot:final_mullen_ELC_Std)
	head(ROI_clinic_clusters_long)
	
	ROI_clinic_clusters_long$index <- as.factor(ROI_clinic_clusters_long$index)
	
	ROI_clinic_clusters_long$test <- as.factor(ROI_clinic_clusters_long$test)
	
	# if plotting all clusters
	if (plotAll == FALSE) {
		ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long[ROI_clinic_clusters_long$index !=5, ]
	} else {
		ROI_clinic_clusters_long_run <- ROI_clinic_clusters_long
	}
	
	# plot

	titles <- c("ADOS SA", "ADOS Total", "ADOS RRB", "Mullen ELC", "Mullen ELT",
		    "Mullen FMT", "Mullen RLT", "Mullen VRT", "Vineland ABC", 
		    "Vineland Communication", "Vineland Daily Living Skills", 
		    "Vineland Domain Total", "Vineland Motor", "Vineland Socialization")
	
	for (i in 1:14) {
		test <- levels(ROI_clinic_clusters_long_run$test)[i]
		tmp <- ROI_clinic_clusters_long_run[ROI_clinic_clusters_long_run$test == test, ]
		tmp <- tmp[tmp$values != 0, ]
		
		p_clinic <- ggplot(tmp, aes(x = index, y = values, fill = index)) +
			geom_boxplot(width = 1,outlier.shape = NA) +
			geom_point(aes(col = index), color = "black", size = 3, alpha = 0.6, position = position_jitterdodge(dodge.width = 1)) +
			labs(y = "Scores", x = "Cluster", title = titles[i]) +
			guides(fill = F) +
			theme(legend.title = element_text(colour="black", size=14, face="bold"),
			      legend.text = element_text(colour="black", size=14, face="bold")) +
			theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))+
			theme(axis.text = element_text(size = 14, face = "bold"),
			      axis.title = element_text(size = 16, face = "bold")) +
			theme(panel.background = element_blank(),
			      panel.grid = element_blank(),
			      panel.border = element_blank(),
			      axis.line = element_line(colour = "black"))
	print(p_clinic)	
	}
	return(list(nn,ROI_clinic_clusters))
}
