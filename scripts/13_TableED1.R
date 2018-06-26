source("scripts/helper_functions.R")
data <- read_rds("data/inputs/analysis_data.rds") 

#Specify model components
regression_fixed_effects <- "fe_loc + fe_season + fe_time"
regression_var_cluster <- "fe_loc"
all_regression_controls <- "tmp_pre + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights"
	
	
	
########## Panel A: Full Sample ##########	
	
	#####	Linear Model		#####
	
	
		#column 1: No controls, but with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared  ", 
	                                "|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients["pm25_post",]
			
	
		#column 2: Temp and rainfall controls, with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                                "tmp_pre + tmp_post + rain_pre + rain_post","|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients["pm25_post",]


		#column 3: All controls (except wealth), with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients["pm25_post",]
			


	#####	Quadratic Model		#####
	
	
		#column 4: No controls, but with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ poly(pm25_post,2,raw=T) + pm25_pre + pm25_pre_squared  ", 
	                                "|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients[c("poly(pm25_post, 2, raw = T)1","poly(pm25_post, 2, raw = T)2"),]
			
	
		#column 5: Temp and rainfall controls, with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ poly(pm25_post,2,raw=T) + pm25_pre + pm25_pre_squared + ", 
	                                "tmp_pre + tmp_post + rain_pre + rain_post","|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients[c("poly(pm25_post, 2, raw = T)1","poly(pm25_post, 2, raw = T)2"),]


		#column 6: Temp and rainfall controls, with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ poly(pm25_post,2,raw=T) + pm25_pre + pm25_pre_squared + ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = data, weights = data[,"reg_wt"]) 
			summary(results)$coefficients[c("poly(pm25_post, 2, raw = T)1","poly(pm25_post, 2, raw = T)2"),]
			



########## Panel B: Sample with wealth information ##########	
	
	#####	Linear Model		#####
	
		wealth_sample <- filter(data, !is.na(hh_awi))
		nrow(wealth_sample) #n obs
	
		#column 1: All controls (except wealth), with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared +  ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = wealth_sample, weights = wealth_sample[,"reg_wt"]) 
			summary(results)$coefficients["pm25_post",]


		#column 2: All controls (including wealth), with fe and weights. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared +hh_awi +  ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = wealth_sample, weights = wealth_sample[,"reg_wt"]) 
			summary(results)$coefficients["pm25_post",]
			




	#####	Quadratic Model		#####
	
	
		#column 3: All controls (except wealth) on sample with wealth info. Standard errors clustered at DHS cluster level.

	
		   	equation <- as.formula(paste("child_die_age1 ~ poly(pm25_post,2,raw=T) + pm25_pre + pm25_pre_squared  + ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = wealth_sample, weights = wealth_sample[,"reg_wt"]) 
			summary(results)$coefficients[c("poly(pm25_post, 2, raw = T)1","poly(pm25_post, 2, raw = T)2"),]


		#column 4: All controls (including wealth) on sample with wealth info. Standard errors clustered at DHS cluster level.

		   	equation <- as.formula(paste("child_die_age1 ~ poly(pm25_post,2,raw=T) + pm25_pre + pm25_pre_squared + hh_awi + ", 
	                                all_regression_controls,"|", 
	                                regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	                                
			results <- felm(equation, data = wealth_sample, weights = wealth_sample[,"reg_wt"]) 
			summary(results)$coefficients[c("poly(pm25_post, 2, raw = T)1","poly(pm25_post, 2, raw = T)2"),]

			
			