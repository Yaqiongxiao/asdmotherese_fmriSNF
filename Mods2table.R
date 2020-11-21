# Mods2table.R
#
# Run mixed effects models in left and temporal ROIs across all three language paradigms,
# with Vineland communication and socialization scores as predictors of interest, 
# scan age, gender, and meanFD as variables of no interests, subjects and tasks as 
# random effects. 
#
# INPUT
# 	dat = a data frame file with ROI values across three language paradigms in one column
#	ROIs = specify ROIs, e.g., LHtemporal, RHtemporal
# 	clins = specify clinical tests, e.g., vine_ComTotal_DomStd", "vine_SocTotal_DomStd
# Yaqiongxiao 11/19/2020

Mods2table <- function(dat, ROIs, clins, cnames) {
	k <- 0
	modList <- list()
	for (i in 1:length(ROIs)) {
		
		for (j in 1:length(clins)) {
			k <- k + 1
			flm <- paste0(ROIs[i],"~", clins[j], "+ scan_age + gender + meanFD + (1|subjid) + (1|task)")	

			modList[[k]] <- lmer(flm, dat) # control = lmerControl(optimizer ="Nelder_Mead"),
			
			# calculate marginal R squared
			mod0 <- lmer(paste0(ROIs[i],"~ 1 + (1|subjid)"), dat)
				
			temp <- modList[[k]]
				
			Fmat <- getME(temp, "X")  # get fixed-effects model matrix
			VarF <- var(as.vector(fixef(temp) %*% t(Fmat)))
			VarComp <- as.data.frame(VarCorr(temp), comp=c("Variance","Std.Dev."))
			VarRand <- as.numeric(VarComp[VarComp$grp !="Residual",
							      "vcov"])
			VarResid <- as.numeric(VarComp[VarComp$grp =="Residual",
							       "vcov"])
			Rm <- VarF/(VarF + sum(VarRand) + VarResid)	
			modList[[k]] <- list(modList[[k]], Rm)
				
			}
		}
		

	# save results as a table
	FILTER <- matrix(NA, nrow = length(modList) , ncol = 6)
	colnames(FILTER)[1:6] <- c("Variables",cnames)
	for(i in 1:length(modList)) {
		
		FILTER[i, 2:3] <- round(summary(modList[[i]][[1]])$coefficients[2, 1:2],5)
		FILTER[i, 4:5] <- round(summary(modList[[i]][[1]])$coefficients[2, 4:5],3)
		FILTER[i, 6] <- round(modList[[i]][[2]][1],3)
		
	}
	
	FILTER[1:4, 1] <- rep(c("Communication scores", 
				"Social scores"),2)
	return(FILTER)

}
	
	
