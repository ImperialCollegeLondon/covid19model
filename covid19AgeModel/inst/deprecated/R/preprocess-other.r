update_ifr_by_age_to_include_best_Beta_parameters <- function()
{
	#require(prevalence)	
	path_to_file <- "~/git/R0t/data/ifr_age_200408.csv"
	ifr.by.age <- as.data.table(read.csv(path_to_file))
	tmp <- ifr.by.age[, {
				tmp <- betaExpert(ifr_mean, ifr_cl, ifr_cu, p = 0.95, method = "mean")
				list(ifr_beta_alpha= tmp$alpha, ifr_beta_beta=tmp$beta)								
			}, by='age']
	ifr.by.age <- merge(ifr.by.age,tmp, by='age')
	write.csv(ifr.by.age, file="~/git/R0t/data/ifr_age_200429.csv", row.names=FALSE)	
}