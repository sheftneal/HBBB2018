source("scripts/helper_functions.R")

################################################################
#Load data
################################################################
   
    cluster_birth_aves <- readr::read_rds("data/inputs/cluster_birth_averages.rds") %>% dplyr::mutate(cluster_id =fe_loc) #WorldPop number of births in location
    data <- readr::read_rds("data/inputs/analysis_data.rds") %>% 
            dplyr::mutate(  mother_age_at_birth_squared = mother_age_at_birth^2,
                            child_sex = as.numeric(as.factor(child_sex)),
                            cluster_id = fe_loc) #for replication we removed variable cluster_id to comply with DHS data use terms
                                         #but fe_loc is a re-coded version of this grouping from 1:n_group so use this here.
    pm_range <- tibble::tibble(pm = 0:70)
    



################################################################
# Code Equation (1) with f() specified as in Equation (2)
################################################################

	#define main components of regression model (names refer to columns in "data" object)
      	regression_controls <- "tmp_pre + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights"
      	regression_fixed_effects <- "fe_loc + fe_season"
      	regression_var_cluster <- "fe_loc"
      	#need to manually code the time fixed effects so we can use getfe() which is setup to work with 2 fixed effects but not 3.
      		#data$time_fe has values from 2:16 so code manually by setting up separate dummy variable for each factor level
      		data[,paste("time_fe_", 2:16, sep = "")] <- 0
      		for(i in 2:16){
      			data[,paste("time_fe_", i, sep = "")] <- as.numeric(data$fe_time == i)
      						}
      	
      		  #need to omit one dummy to avoid collinearity so we omit time_fe_1 
      		  #(choice arbitrary and doesn't make a difference for other coefficients)
      		  equation_1 <- as.formula(	paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared +", 
      		  							paste0("time_fe_", 2:15, collapse = "+"),"+",
      		  							regression_controls, "|", regression_fixed_effects  ,"| 0" , sep = ""))
      		
      		
      


#############################################
# Bootstrap RR CI
#############################################

B <- 1000
cluster_frame <- data %>% dplyr::group_by(cluster_id) %>% dplyr::summarise(wt = n())
n_s <- nrow(cluster_frame)
results_bstrap <- tibble::as_tibble(matrix(nrow = nrow(pm_range), ncol = B))
results_bstrap_raw <- list()

for(b in 1:B){
	
	#bootsrap block sampling clusters
	boot_sample <- data %>% dplyr::group_by(cluster_id) %>% sample_n_groups(n_s, replace = T)

	
	
	#follow the steps outlined in methods section "Calculating relative risk and excess deaths attributable to pollution"

		#(1) estimate y(.)
	
				boot_model <- lfe::felm(equation_1, data = boot_sample, weights = boot_sample$reg_wt)

		#(2) estimate y(z) and y(2)		
				
				#pull out fixed effect values
				model_fe <- lfe::getfe(boot_model) 

				#separate fe into loc vs sesaonal fe and merge back in with data
					boot_sample <-  boot_sample %>%
        									dplyr::mutate(
        										idx_loc = as.character(as.factor(fe_loc)),
        										idx_season = as.character(as.factor(fe_season))				
        												      )
																	
					fe_loc <- 	dplyr::filter(model_fe, fe=="fe_loc") %>% 
								      dplyr::mutate(idx_loc = as.character(idx), fe_loc_pred = effect) %>% 
								      dplyr::select(- idx)
					fe <- dplyr::left_join( boot_sample[,c("child_id","idx_loc","idx_season")], 
									                fe_loc[,c("idx_loc","fe_loc_pred")], by = "idx_loc")
					
					fe_season <- 	dplyr::filter(model_fe, fe=="fe_season") %>% 
					              dplyr::mutate(idx_season = as.character(idx), fe_season_pred = effect) %>% 
									      dplyr::select(- idx)
					fe <- dplyr::left_join(	fe[,c("child_id","idx_season", "idx_loc","fe_loc_pred")], 
										fe_season[,c("idx_season","fe_season_pred")], by = "idx_season")
			
			
				#calculate total fe (sum of location and seasonal fe)
					fe <- as.numeric(fe$fe_loc_pred + fe$fe_season_pred)
		
				#calculate y(z) and y(2) including fe and use to calculate the relative risk curve according to Equation (4)	
		
					#create newvalues to predict over. 
					#new_data_pmobs is z in y(z) from Equation (4). new_data_pm2 is 2 in y(2) from Equation (4).
					new_data_pmobs <- boot_sample
					new_data_pm2 <- new_data_pmobs %>% dplyr::mutate(pm25_post  = replace(pm25_post, pm25_post > 2, 2))
				
			# predict imr at observed pm levels (y_z) and at counterfactual pm level of 2 (y_2)
			boot_sample$y_z <- predict.felm(boot_model, newdata = new_data_pmobs)$fit + fe
			boot_sample$y_2 <- predict.felm(boot_model, newdata = new_data_pm2)$fit + fe
		
		
		
		#(3) average to the cluster level and divide data into 5 ug/m3 bins

			RR_estimates_boot <-  boot_sample %>% #cluster averages
							              dplyr::group_by(cluster_id) %>% #take DHS cluster level averages of each outcome
			                      dplyr::summarise(	y_z = mean(y_z, na.rm = T), 
										      	    y_2 = mean(y_2, na.rm = T),
											          pm25_post = mean(pm25_post, na.rm = T),											
											          n_obs = n()) %>%  #count number of observed births in cluster
			                      dplyr::left_join(	cluster_birth_aves, by = "cluster_id") %>%	#add in number of births in loc
			                      dplyr::mutate(  pm = round(pm25_post/5,0)*5	) %>% #divide pm into 5-unit bins
			                      dplyr::filter( 	pm %in% pm_range$pm) %>% #limit to area of observed pm in sample
			                      dplyr::group_by(pm) %>%
			                      dplyr::summarise(
            											#mean of cluster-level y(2) by pm level weighted by the # births in each cluster		
            											y_2 = weighted.mean(y_2, n_births, na.rm = T), 
            											#mean of cluster-level y(z) by pm level weighted by the # births in each cluster
            											y_z = stats::weighted.mean(y_z, n_births, na.rm = T),
            											#number of obs in each pm bin 
            											n_obs = n()	
            												) %>%
			                       dplyr::mutate(	RR_z = y_z/y_2 ) 	#Equation (4)		 
			
	
            #now save smoothed RR curve for this bootstrap iteration where RR curve estimated across pm distribution
			      #weighted by the number of obs in each pm bin
						results_bstrap[,b] <- predict(loess(RR_z ~ pm, 	data = RR_estimates_boot, span = 1.5, weights = n_obs), 
																		newdata = data.frame(pm = pm_range$pm) ) %>% as.numeric



				print(b)
					}#end bootstrap iteration


		RRx <- 0:70
		RRy <- apply(results_bstrap,1,function(x){quantile(x, 0.5, na.rm = T)})

		RRlb90 <- apply(results_bstrap,1,function(x){quantile(x, 0.05, na.rm = T)})
		RRub90 <- apply(results_bstrap,1,function(x){quantile(x, 0.95, na.rm = T)})

		RRlb95 <- apply(results_bstrap,1,function(x){quantile(x, 0.025, na.rm = T)})
		RRub95 <- apply(results_bstrap,1,function(x){quantile(x, 0.975, na.rm = T)})

		RR_curve <- tibble::tibble(RRx, RRy, RRlb90, RRub90, RRlb95, RRub95) %>%
				        dplyr::mutate(RRy = replace(RRy, RRy < 1, 1),
								              RRlb90 = replace(RRlb90, RRlb90 < 1, 1),
								              RRub90 = replace(RRub90, RRub90 < 1, 1),
								              RRlb95 = replace(RRlb95, RRlb95<1, 1),
							  	            RRub95 = replace(RRub95, RRub95<1, 1)
								              )

    #write functions 
		get_RR = function(RRx){ RR_curve$RRy[round(RRx)+1]} #starts at 0 so + 1
		get_RR_lb90 = function(RRx){ RR_curve$RRlb90[round(RRx)+1]}
		get_RR_ub90 = function(RRx){ RR_curve$RRub90[round(RRx)+1]}
		get_RR_lb95 = function(RRx){ RR_curve$RRlb95[round(RRx)+1]}
		get_RR_ub95 = function(RRx){ RR_curve$RRub95[round(RRx)+1]}

	  	

		
		save(RR_curve, get_RR, get_RR_lb90, get_RR_ub90, get_RR_lb95, get_RR_ub95, file = "data/RR_curve_HBBB2018_test.RData")

		
		
