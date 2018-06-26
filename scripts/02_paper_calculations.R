source("scripts/helper_functions.R")

  data <- readr::read_rds("data/inputs/analysis_data.rds") %>% 
          dplyr::mutate(cluster_id = fe_loc) #cluster_id has been removed so hh remain anonymous but fe_loc is same grouping
  load("data/RR_curve_HBBB2018.RData") #RR curve from our study
      #RR_curve: our estimated RR curve and CI
      #get_RR: function that inputs pm level and outputs relative risk at that pm level
  load("data/inputs/Gridded_data_for_calculations.RData")
      #grid: grid paramters
      #pm: gridded 2015 pm2.5 levels
      #births: gridded number of births
      #imr: gridded infant mortality rates
  load("data/inputs/map_boundaries.Rdata")
  

#########################################################################################################
#Calculations
#########################################################################################################

##### List of Calculations in the paper #####

## Abstract ##
	#(1) Reduction in imr associated with 10 ug/m3 reduction in PM2.5 concentration
	#(2) Share of infant deaths attributable to PM2.5 above 2 ug/m3 [Equation 5]
	#(3) Number of excess infant deaths attributable to PM2.5 above 2 ug/m3 [Equation 6]
	
## Main text ##
	#(1) Reduction in imr associated with uniform 10 ug/m3 reduction in PM2.5 concentration
	#(2) Our RR estimate at median exposure levels
	#(3) Share of excess infant deaths estimated to occur in Nigeria
	#(4) Reduction in imr associated with uniform 5 ug/m3 reduction in PM2.5 concentration in 2015
	#(5) RR comparison with GBD at median PM2.5 exposure in our sample

## Methods ##
	#(1) "Using the Burnett et al RR function, we estimate that 13% of infant deaths in our sample are attributable to PM2.5"
	#(2) "using 5.8ug/m3 that number becomes:"
	#(3) Using GBD RR curve to estimate excess infant deaths
	#(4) Share of attributable deaths occuring above/below median pm2.5 exposure in our data
	#(5) Comparing share estimates with different counterfactuals
	#(6) Increase in neo-natal mortality associated with 10 ug/m3 reduction in PM




#########################################################################################################
###################### Abstract ############################### 
#########################################################################################################		
## Abstract ##
	#(1) Reduction in imr associated with 10 ug/m3 reduction in PM2.5 concentration
	#(2) Share of infant deaths attributable to PM2.5 above 2 ug/m3 [Equation 5]
	#(3) Number of excess infant deaths attributable to PM2.5 above 2 ug/m3 [Equation 6]

		cluster_birth_aves <- readr::read_rds("data/inputs/cluster_birth_averages.rds") %>% dplyr::mutate(cluster_id = fe_loc) #WorldPop Birth data processed to cluster level

		pm_ave <- data %>%  dplyr::group_by(cluster_id) %>% 
		                    dplyr::summarise(pm = mean(pm25_post, na.rm = T))
		cluster_birth_aves <- cluster_birth_aves %>%
							            dplyr::left_join(pm_ave,c("cluster_id"))

		cluster_birth_aves$share_attr <- 1- 1/get_RR(cluster_birth_aves$pm)
			cluster_birth_aves$share_attr[cluster_birth_aves$pm>70]<-1- 1/get_RR(70) #limit estimated to observed pm support

		cluster_birth_aves$share_attr_lb <- 1- 1/get_RR_lb95(cluster_birth_aves$pm)
			cluster_birth_aves$share_attr_lb[cluster_birth_aves$pm>70]<-1- 1/get_RR_lb95(70)#limit estimated to observed pm support

		cluster_birth_aves$share_attr_ub <- 1- 1/get_RR_ub95(cluster_birth_aves$pm)
			cluster_birth_aves$share_attr_ub[cluster_birth_aves$pm>70]<-1- 1/get_RR_ub95(70)#limit estimated to observed pm support


#(1)	reduction in mortality from unfirom 10 ug/m3 reduction in pm2.5
		#	"We find that a 10ug/m3 increase in PM2.5 concentration is associated with a 9% (95% CI: 4-14%) rise in imr across the sample"

			
	     # coefficients from Table ED1
			 # 	(10 unit pm reduction)*(beta_pm)/(mean_imr) =
				round(10*(6.492e-04/0.07076136)*100,1) #(p = 0.0013)

		
#(2)	shares attributable for average in our sample
#		"Our estimates suggest that PM concentrations above min exposure were reponsible for 22% (95%CI: 11-34%) of infant deaths in 30 study countries

  		cluster_birth_aves %>% dplyr::summarise(
  								            share_lb = round(weighted.mean(share_attr_lb, n_births,na.rm =T),2)*100,
  								            share_attr = round(weighted.mean(share_attr, n_births,na.rm =T),2)*100,
  								            share_ub = round(weighted.mean(share_attr_ub, n_births,na.rm =T),2)*100
  								                            )
  

#(3)	excess deaths

      	#get values for relevant covariates at each grid cell	
      	gridValues <- data.frame(coordinates(imr), imr = getValues(imr), births = getValues(births), 
      								pm = getValues(pm)) %>% subset( !is.na(births) & !is.na(imr) & !is.na(pm)) %>% 
      								filter(births > 0)
      	
      	
      	### we know imr and n_births at each cell and pm. 
      	#   Use pm to get share from 1-1/RR then do calc at cell level and aggregate:
      	gridValues$country <- as.character(sp::over(SpatialPoints(gridValues[,c("x","y")]), africa)$NAME)
      	gridValues <- gridValues %>% dplyr::filter(country %in% unique(data$country))
      
      	gridValues$share_attr <- 1 - 1/get_RR(gridValues$pm)
      	gridValues$share_attr[gridValues$pm>70]<- 1 - 1/get_RR(70) #set obs outside our observed range to max observed RR
      
      	gridValues$share_lb <- 1 - 1/get_RR_lb95(gridValues$pm)
      	gridValues$share_lb[gridValues$pm>70]<- 1 - 1/get_RR_lb95(70) #set obs outside our observed range to max observed RR
      
      	gridValues$share_ub <- 1 - 1/get_RR_ub95(gridValues$pm)
      	gridValues$share_ub[gridValues$pm>70]<- 1 - 1/get_RR_ub95(70) #set obs outside our observed range to max observed RR
      	
      	
      	  gridValues <- gridValues %>% dplyr::select(country, x, y, pm, births, imr, starts_with("share"))
      
      	  cell_estimates <- gridValues %>%
      					            dplyr::mutate(
                							deaths = share_attr*(imr/1000)*births,
                							deaths_lb = share_lb*(imr/1000)*births,
                							deaths_ub = share_ub*(imr/1000)*births
      							          )
      			
      		cell_estimates %>% summarise(
              								deaths_lb = round(sum(deaths_lb)/1000)*1000,
              								deaths = round(sum(deaths)/1000)*1000,
              								deaths_ub = round(sum(deaths_ub)/1000)*1000	
      								                )
      		
      		cell_estimates_this_study <- cell_estimates
      
      
      
      		#calculate country level estimates for Fig. 4
      		country_estimates <- 	cell_estimates %>% 
      								group_by(country) %>%
      											summarise(
      													deaths = round(sum(deaths)),
      													share_attr = round(weighted.mean(share_attr, births),2)
      													)
      			

		

      		
      		
#########################################################################################################
###################### Main Text ############################### 
#########################################################################################################	

## Main text ##
	#(1) Reduction in imr associated with uniform 10 ug/m3 reduction in PM2.5 concentration
	#(2) Difference between our estimates and GBD at median exposure levels
	#(3) Share of excess infant deaths estimated to occur in Nigeria
	#(4) Reduction in imr associated with uniform 5 ug/m3 reduction in PM2.5 concentration
	#(5) Our estimates of relative risk are double gbd's estimates at median PM2.5 in our sample	
		
	#(1) see calculation (1) from Abstract 
	
	#(2) Relative Risk at average pm2.5 exposure	
	#	 "we estimate a 31% increase in mortality risk at sample median exposure levels (25 ug/m3)"		
			round(filter(RR_curve, RRx == round(median(data$pm25_post)))$RRy-1, 2)*100 #at median exposure
	
	
	#(3) share of excess infant deaths in Nigeria
	round(filter(country_estimates, country=="Nigeria")$deaths/sum(country_estimates$deaths)*100)	
						
	
	#(4) 	reduction in mortality from uniform 5 ug/m3 reduction in pm2.5
	
		diff_5 <-
		c(round((5*(6.492e-04 - 2.016e-04 * 1.959964))/0.07076136, 3)*100,
		round(5*(6.492e-04/0.07076136),3)*100, #(p = 0.0013)
		round((5*(6.492e-04 + 2.016e-04 * 1.959964))/0.07076136, 3)*100)
	
		#(used in Fig. 4)
		
		  
	#(5) #"At median African exposure levels, our estimate relative risk of mortality is double the risk estimated by the GBD"
			
			load("data/figure_data/Fig3_data.RData")#load gbd RR curve (ours is already loaded above)
			
			median_pm <- median(data$pm25_post)
			
			risk_med <- (filter(RR_curve, RRx == round(median_pm))$RRy - 1)	 #our estimate of risk at median exposure			
			gbd_risk_med <- (predict(loess(y_gbd ~ x_gbd), newdata = data.frame(x_gbd = median_pm)) -1) #gbd estimate	
			
			paste(round(risk_med/gbd_risk_med,0),"X as large", sep="") #ratio of estimates




	
				
				
#########################################################################################################
###################### Methods ############################### 
#########################################################################################################		
	#(1) "Using the Burnett et al RR function, we estimate that 13% of infant deaths in our sample are attributable to PM2.5"
	#(2) "using 5.8ug/m3 that number becomes:"
	#(3) Using GBD RR curve to estimate excess infant deaths
	#(4) Share of attributable deaths occuring above/below median pm2.5 exposure in our data
	#(5) comparing share estimates with different counterfactuals
	#(6) Increase in neo-natal mortality associated with 10 ug/m3 reduction in PM
				
				
#(1)	"Using the Burnett et al RR function, we estimate that 13% of infant deaths in our sample are attributable to PM2.5"			

		cluster_birth_aves <- read_rds("data/inputs/cluster_birth_averages.rds") #WorldPop Birth data processed to cluster level

		pm_ave <- data %>% dplyr::group_by(fe_loc) %>% dplyr::summarise(pm = mean(pm25_post, na.rm = T))
		cluster_birth_aves <- cluster_birth_aves %>%
							            dplyr::left_join(pm_ave,"fe_loc")

		
		get_RR_gbd <- function(pm){predict(loess(y_gbd ~ x_gbd, span = 0.5), newdata = data.frame(x_gbd = pm))}

		cluster_birth_aves$share_attr_gbd <- 1- 1/get_RR_gbd(cluster_birth_aves$pm)


		cluster_birth_aves %>% 
		            dplyr::summarise(
								                  share_attr = round(weighted.mean(share_attr_gbd, n_births,na.rm =T),2)*100
								                )

		cluster_birth_aves_gbd <-  cluster_birth_aves




#(2) 	Share calculations with different counterfactual. Using our RR curve with GBD cf: 
#		"using 5.8ug/m3 that number becomes 18%"

		#see figED7_gen_plot_data.R for details
		share_cfs <- read_rds("data/figure_data/figED7_data.rds")
		round(100*share_cfs["share_5_8"])


#(3) 	Using GBD RR curve to estimate excess infant deaths
#		"We can apply the approach described in Equation 6 to the integrated response curve developed By burnett et al. 
#		 This approach produces an estimate of 335 attributable deaths in 2015"

			gridValues <- data.frame(coordinates(imr), imr = getValues(imr), births = getValues(births), 
											pm = getValues(pm)) %>% subset( !is.na(births) & !is.na(imr) & !is.na(pm)) %>% 
											filter(births > 0)
				
				
						y_gbd <- y_gbd[x_gbd < max(gridValues$pm)]; x_gbd <- x_gbd[x_gbd < max(gridValues$pm)] #limit gbd curve to range observed pm
			
			
				gridValues$country <- as.character(sp::over(SpatialPoints(gridValues[,c("x","y")]), africa)$NAME)
				gridValues <- gridValues %>% filter(country %in% unique(data$country))
			
				gridValues$share_attr_gbd <- 1 - 1/get_RR_gbd(gridValues$pm)
				gridValues$share_attr_gbd[gridValues$pm < min(x_gbd)] <- 0
				gridValues$share_attr_gbd[gridValues$pm > max(x_gbd)] <- 1 - 1/get_RR_gbd(max(x_gbd))
							
				gridValues <- gridValues %>% dplyr::select(country, x, y, pm, births, imr, starts_with("share"))
			
				cell_estimates <- gridValues %>%
								mutate(
										deaths = share_attr_gbd*(imr/1000)*births
										)
						
				cell_estimates %>% summarise(
											deaths = round(sum(deaths)/1000)*1000
											)

				cell_estimates_gbd <- cell_estimates

	
#(4) 	Share of attributable deaths occuring above/below median pm2.5 exposure in our data
			#"For example, only 11 percent of attributable deaths (38,000) estimated using previously 
				# published response curve4 occurred in locations with lower than median (27 μg m−3) PM2.5 exposures 
				# whereas 18 percent (80,000) of attributable infant deaths estimated using our methods occurred 
				# in these relatively lower PM2.5 areas".
				

		round(100*sum(filter(cell_estimates_gbd, pm <= median(pm))$deaths)/sum(cell_estimates_gbd$deaths))
		round(sum(filter(cell_estimates_gbd, pm <= median(pm))$deaths)/1000)*1000
		
		round(100*sum(filter(cell_estimates_this_study, pm <= median(pm))$deaths)/sum(cell_estimates_this_study$deaths))
		round(sum(filter(cell_estimates_this_study, pm <= median(pm))$deaths)/1000)*1000		




#(5)	Share calculations with different cfs
		
#		"we note that if the GBD counterfactual PM25 concentration of 5.8ug/m3 is used to define
#		 our "clear air" basline then our estimate of attributable deaths would be reduced to 18%"		
		round(100*share_cfs["share_5_8"])
		
#		"with the GBD estimate of 12.9% within our ci"
		cluster_birth_aves_gbd %>% summarise(share_attr = round(weighted.mean(share_attr_gbd, n_births,na.rm =T),3)*100)

	
		
		
				
				
				
				
				
				