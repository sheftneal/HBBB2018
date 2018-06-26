#*******************************************************************************************************************    	
#**************       NOTE: This code is slow because of bootstrapping. It takes >1 hr to run.               *******
#*************        Can remove Nboot and bootexpr arguments from lfe::felm() for quick results w/out CI.        *******    	
#*******************************************************************************************************************    

 
source("scripts/helper_functions.R")
data <- read_rds("data/inputs/analysis_data.rds")

  #Specify model components
    	regression_controls <- "tmp_pre + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights"
    	regression_fixed_effects <- "fe_loc + fe_season + fe_time"
    	regression_var_cluster <- "fe_loc"
    	pm25_support <- 0:75 
    
    	
################################
#### Panel 1: Main Effect
################################

		  #Equation (1) with PM2.5-postbirth exposure modeled linearly and PM2.5-prebirth modeled as quadratic:

	    #Equation 1/Equation 2 in the paper
	    equation_1 <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                               regression_controls, "|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	
	    #to see results without bootstrapping remove Nboot and bootexpr arguments and run.
	    #Regression also corresponds to ED Table 1 Column (3)
	    main_model_results_a <- lfe::felm(equation_1, data = data, weights = data[,"reg_wt"], Nboot = 1000, bootexpr = quote(pm25_post*(pm25_support))) 
		  bstrap_a <- main_model_results_a$boot #pull out bootstrap runs

			#use bootstrapped model runs to generate 95% confidence intervals for imr | pm  across inner 99 percentile of observed pm range
			bstrap_a <- apply(bstrap_a, 2, function(x){center(x,pm25_support, round(mean(data$pm25_post)), mean(data$child_die_age1))}) #re-center to (mean pm, mean imr)
			response_a <- tibble::tibble(x =pm25_support,  y = apply(bstrap_a, 1, median), #bring in x and y
			                     ci_low = apply(bstrap_a, 1, function(x){quantile(x, 0.025)}), ci_high = apply(bstrap_a, 1, function(x){quantile(x, 0.975)}) ) %>%  #generate ci
			  filter(x > quantile(data$pm25_post, 0.01) & x < quantile(data$pm25_post, 0.99))  #keep inner 99 percentile observed pm concentrations
			
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_a$x[1], response_a$x, response_a$x[nrow(response_a):1])
			py <- c(response_a$ci_low[1], response_a$ci_high, response_a$ci_low[nrow(response_a):1])	
			ci_polygon_a <- tibble::tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_a <- get_hst_obj(data$pm25_post, 0,80,1,0.99)
			
	
			
################################
#### Panel 2: Pre vs Post period 
################################



			#Run main model with 1000 bootstraps to get confidence intervals. Getting CIs is slow but can run quickly to see results without bootstrapping by leaving out Nboot and bootexpr arguments. 
			main_model_results_b <- lfe::felm(equation_1, data = data, weights = data[,"reg_wt"], Nboot = 1000, bootexpr = quote(pm25_pre*(pm25_support) + pm25_pre_squared*(pm25_support^2))) #
			bstrap_b <- main_model_results_b$boot #pull out bootstrap runs
			
			#use bootstrapped model runs to generate 95% confidence intervals for imr | pm  across inner 99 percentile of observed pm range
			bstrap_b <- apply(bstrap_b, 2, function(x){center(x,pm25_support, round(mean(data$pm25_post)), mean(data$child_die_age1))}) #re-center to (mean pm, mean imr) [use pm25_post for consistency]
			response_b <- tibble::tibble(x =pm25_support,  y = apply(bstrap_b, 1, median), #bring in x and y
			                     ci_low = apply(bstrap_b, 1, function(x){quantile(x, 0.025)}), ci_high = apply(bstrap_b, 1, function(x){quantile(x, 0.975)}) ) %>%  #generate ci
			  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile observed pm concentrations
			
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_b$x[1], response_b$x, response_b$x[nrow(response_b):1])
			py <- c(response_b$ci_low[1], response_b$ci_high, response_b$ci_low[nrow(response_b):1])	
			ci_polygon_b <- tibble::tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_b <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			
	



################################
#### Panel 3: West vs. Rest of Africa
################################

		#could run as separate regressions if you think important to let fixed effects vary across regions or could run as single regression interacted.
		#we get similar response curves either way but here we allow fe to vary across regions by estimating separate regressions.
		data_west <- filter(data, region_west == 1) 
		data_other <- filter(data, region_west == 0) 
		
		#Equation 1 estimated separately by region.
		
		#Run main model with 1000 bootstraps to get confidence intervals. Getting CIs is slow but can run quickly to see results without bootstrapping by leaving out Nboot and bootexpr arguments. 
		main_model_results_c_west <- lfe::felm(equation_1, data = data_west, weights = data_west[,"reg_wt"], Nboot = 1000, bootexpr = quote(pm25_post*(pm25_support))) #
		bstrap_c_west <- main_model_results_c_west$boot #pull out bootstrap runs

		main_model_results_c_other <- lfe::felm(equation_1, data = data_other, weights = data_other[,"reg_wt"], Nboot = 1000, bootexpr = quote(pm25_post*(pm25_support))) #
		bstrap_c_other <- main_model_results_c_other$boot #pull out bootstrap runs
		
		
		#use bootstrapped model runs to generate 95% confidence intervals for imr | pm  across inner 99 percentile of observed pm range
		bstrap_c_west <- apply(bstrap_c_west, 2, function(x){center(x,pm25_support, round(mean(data_west$pm25_post)), 0)}) #re-center to (mean pm, mean imr) [use pm25_post for consistency]
		response_c_west <- tibble::tibble(x =pm25_support,  y = apply(bstrap_c_west, 1, median), #bring in x and y
		                     ci_low = apply(bstrap_c_west, 1, function(x){quantile(x, 0.025)}), ci_high = apply(bstrap_c_west, 1, function(x){quantile(x, 0.975)}) ) %>%  #generate ci
		                    filter(x > quantile(data_west$pm25_post, 0.01) & x < quantile(data_west$pm25_post, 0.99))  #keep inner 99 percentile observed pm concentrations
		
		#get response curve to pass through overall data sample mean pm, mean imr
		response_c_west[,c("y","ci_low","ci_high")] <- response_c_west[,c("y","ci_low","ci_high")]-
														filter(response_c_west, x==round(mean(data$pm25_post)))$y + mean(data$child_die_age1)
		
	
		bstrap_c_other <- apply(bstrap_c_other, 2, function(x){center(x,pm25_support, round(mean(data_other$pm25_post)), 0)}) #re-center to (mean pm, mean imr) [use pm25_post for consistency]
		response_c_other <- tibble::tibble(x =pm25_support,  y = apply(bstrap_c_other, 1, median), #bring in x and y
		                           ci_low = apply(bstrap_c_other, 1, function(x){quantile(x, 0.025)}), ci_high = apply(bstrap_c_other, 1, function(x){quantile(x, 0.975)}) ) %>%  #generate ci
		                    filter(x > quantile(data_other$pm25_post, 0.01) & x < quantile(data_other$pm25_post, 0.99))  #keep inner 99 percentile observed pm concentrations

		#get response curve to pass through overall data sample mean pm, mean imr
		response_c_other[,c("y","ci_low","ci_high")] <- response_c_other[,c("y","ci_low","ci_high")]-
														filter(response_c_other, x==round(mean(data$pm25_post)))$y + mean(data$child_die_age1)
		

		
					
		#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_c_west$x[1], response_c_west$x, response_c_west$x[nrow(response_c_west):1])
			py <- c(response_c_west$ci_low[1], response_c_west$ci_high, response_c_west$ci_low[nrow(response_c_west):1])	
			ci_polygon_c_west <- tibble::tibble(px, py)
	
			px <- c(response_c_other$x[1], response_c_other$x, response_c_other$x[nrow(response_c_other):1])
			py <- c(response_c_other$ci_low[1], response_c_other$ci_high, response_c_other$ci_low[nrow(response_c_other):1])	
			ci_polygon_c_other <- tibble::tibble(px, py)
					
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_c_west <- get_hst_obj(data_west$pm25_post, 0,80,1,0.99) 
			hist_ht_c_other <- get_hst_obj(data_other$pm25_post, 0,80,1,0.99)
			
			


################################
#### Panel 4: Rich vs Poor
################################

	#define ewi terciles
	data$awiBin <- statar::xtile(data$hh_awi, n= 3) #xtile stata function implemented in R
	data$awi_tec1 <- data$awiBin==1	
	data$awi_tec2 <- data$awiBin==2
	data$awi_tec3 <- data$awiBin==3
		
			
	 #Run wealth interaction regression [Equation (3) in the paper]	
	  equation_3_wealth <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_post*awi_tec2 +  pm25_post*awi_tec3 + poly(pm25_pre,2,raw=T) + ", 
	                               regression_controls, "|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
	
	
		model_results_d <- lfe::felm(equation_3_wealth, data = data, weights = data[,"reg_wt"] ) 

	
	#pull out coefficients. 1st row is just base estimate. Subsequent rows are base estimate + interacted coef.
		coef <- data.frame(est = rep(NA, 3), se = rep(NA, 3))
		coef[1,"est"] <- summary(model_results_d)$coefficients["pm25_post","Estimate"]

		for(i in 2:3){
		coef[i,"est"]<-summary(model_results_d)$coefficients[paste("pm25_post:awi_tec",i,"TRUE",sep=""),"Estimate"] +
		 				coef[1,"est"]
					}	
	
	#pull out standard errors. 1st row just pull it from regression. Other calculate s.e. on linear combination of coefficients.
		coef[1,"se"] <- summary(model_results_d)$coefficients["pm25_post","Cluster s.e."]				
	
			for(i in 2:3){
			coef[i,"se"]<-as.numeric((summary(
					glht(model_results_d, linfct = c(paste("pm25_post+pm25_post: awi_tec",i,"TRUE=0", sep="")))
										))$test$sigma)					
						}
							
	#calculate base rates by wealth index tercile
		base_rates  <- 	data %>%
						group_by(awiBin) %>%
						summarise(	
						      base_imr = mean(child_die_age1),
									base_pm = mean(pm25_post),
									base_awi = mean(hh_awi, na.rm = T)) %>%
						filter(!is.na(awiBin))
							
							
							
		#calulate CIs and divide through by wealth tercile base imr to get relative effects
			coef_d  <- 	coef %>% as.tibble %>%
						mutate(awiBin = 1:3) %>%
						left_join(base_rates, "awiBin") %>%
						mutate(	y = est/base_imr,
								ci_low = (est + qnorm(0.025)*se)/base_imr,
								ci_high = (est + qnorm(0.975)*se)/base_imr ) %>%
						dplyr::select(y, ci_low, ci_high)		
					
								
						





#########################################
#### Panel 5: Over Time
#########################################

	#We want to restrict data to balance the sample across years so we subset
	# to birth year <=2012 and survey year >=2011 with variable timeSample==1
  
	#create dummies for each time period
	data$dummy_time1 <- data$child_birth_year %in% 2001:2003
	data$dummy_time2 <- data$child_birth_year %in% 2004:2006
	data$dummy_time3 <- data$child_birth_year %in% 2007:2009
	data$dummy_time4 <- data$child_birth_year %in% 2010:2013
	data$time_period <- data$dummy_time1*1 + data$dummy_time2*2 + data$dummy_time3*3 + data$dummy_time4*4

	#we have year fe so create interaction terms manually 
	#(lfe::felm() doesn't like the pm25_post*dummy_time syntax here becuase dummy_time gets included as a separate regressor but it's already there as in fe_time)
	data$pm25_postxdummy_time2 <- data$pm25_post*data$dummy_time2
	data$pm25_postxdummy_time3 <- data$pm25_post*data$dummy_time3
	data$pm25_postxdummy_time4 <- data$pm25_post*data$dummy_time4	
	
	regression_controls <- "tmp_pre + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights"

	equation_3_time <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_postxdummy_time2 + pm25_postxdummy_time3 + pm25_postxdummy_time4 + poly(pm25_pre,2,raw=T) +",
										regression_controls,"|",regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))

	
	model_results_e <- lfe::felm(equation_3_time, data = filter(data, timeSample==1), weights = filter(data, timeSample==1)[,"reg_wt"] )
	
			#pull out coefficients. 1st row is just base estimate. Subsequent rows are base estimate + interacted coef.
				vars <- names(coef(model_results_e))
				c1 <- summary(model_results_e)$coefficients["pm25_post","Estimate"]
				cc <- summary(model_results_e)$coefficients[vars[grep(x=vars,pattern = "pm25_post" )],"Estimate"]
				coef <- data.frame(est = c(c1, c1+cc[2:length(cc)]), se = NA); rownames(coef)<-paste("period",1:4, sep="")
		
			#pull out standard errors. 1st row just pull it from regression. Other calculate s.e. on linear combination of coefficients.
				coef[1,"se"]<-summary(model_results_e)$coefficients["pm25_post","Cluster s.e."]
				for(y in 2:4){
				coef[y,"se"] <- as.numeric((summary(glht(model_results_e, 	
							linfct = c(paste("pm25_post+pm25_postxdummy_time",y, "=0",	sep="")))))$test$sigma)
										}


		#calculate base rates by time period	
			base_rates  <- 	data %>%
			        dplyr::filter(timeSample==1) %>%
			        dplyr::group_by(time_period) %>%
			        dplyr::summarise(	base_imr = mean(child_die_age1),
										base_pm = mean(pm25_post)) %>%
			        dplyr::mutate(period = 1:4) %>%
							dplyr::select(- time_period)			 


		#calulate CIs and divide through by wealth tercile base imr to get relative effects
		coef_e  <- 	coef %>% as.tibble %>%
		        dplyr::mutate(period = 1:4) %>%
		        dplyr::left_join(base_rates, "period") %>%
		        dplyr::mutate(	y = est/base_imr,
								ci_low = (est + qnorm(0.025)*se)/base_imr,
								ci_high = (est + qnorm(0.975)*se)/base_imr ) %>%
						dplyr::select(y, ci_low, ci_high)	

	




######## define plot colors #########

		#Derived from Paul Tol's palettes designed to be distinct in colour-blind vision
		#https://personal.sron.nl/~pault/
			col_line_a <- "#114477" 
			col_shade_a <- "#77AADD"
			col_line_b1 <- "#114477"
			col_shade_b1 <- "#77AADD"
			col_line_b2 <- "#117744"
			col_shade_b2 <- "#44AA77"
			col_line_c1 <-"#DDAA77"
			col_shade_c1 <-"#AA7744"
			col_line_c2 <- "#AA4488"
			col_shade_c2 <- "#CC99BB" 
			col_d <- "black"
			col_e <- "black"
			
	
		
######## ######## plot ######### ########

pdf(file = "figures/raw/Fig2_raw.pdf",width = 17, height= 8, useDingbats = F)	

	#layout for plot (1 , 2x2)
	mat <- cbind(matrix(rep(1,4),nrow=2),matrix(2:5,nrow=2,byrow=T))
	layout(mat)
	x <- 0:75



###############################
#### Panel a: Main Effect
################################

			par(mar = c(2,6,0,2))
			
			#plot
			plot(response_a$x, response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
				axes = F,xlab = "",ylab = "")
			polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha(col_shade_a,0.25),border =NA)
			lines(response_a$x, response_a$y, lwd = 3, col = col_line_a)		
			plotHist(hist_ht_a,col_shade_a, 0.25, 0, 0.025, add.alpha(col_line_a, 0.15) )
			
			#axis
			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-3,cex.axis = 2)
			axis(2, tick = T, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=2)

			#labels
			mtext(side = 1, text = "PM2.5 Concentration (ug/m3)",line = 0.75,cex = 1.5)	
			mtext(side = 2, text = "Infant Mortality Rate (deaths per 1,000 births)",line = 3.75,cex = 1.5)



##################################
##### Panel b: Pre vs Post period 
#################################

			par(mar = c(4,2,2,3))
			
			#plot
			plot(response_b$x, response_b$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
				axes = F,xlab = "",ylab = "")
			
			polygon(ci_polygon_b$px, ci_polygon_b$py, col = add.alpha(col_shade_b2, 0.25),border =NA)
			  lines(response_b$x, response_b$y, lwd = 3, lty=1, col = col_line_b2)
  	  polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha(col_shade_b1 , 0.25),border =NA)
			  lines(response_a$x, response_a$y, lwd = 3, col = col_line_b1)			
			
	
      #histograms
			plotHist(hist_ht_a, col_shade_b1, 0.25, 0, 0.0125, add.alpha(col_line_b1, 0.15) )
			plotHist(hist_ht_b, col_shade_b2, 0.25, 0.0175, 0.0125, add.alpha(col_line_b2,0.15) )

      #labels
			text(	x = response_a$x[which(response_a$x==60)]-2, 
					y = response_a$y[which(response_a$x ==60)]-0.0095, labels = "In-Utero",pos = 3, col = 'black',lwd =2)

			text(	x = response_b$x[which(response_b$x==60)]-2, 
					y = response_b$y[which(response_b$x ==60)], labels = "Post-birth",pos = 3, col = 'black',lwd = 2)
			
			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.25)
			axis(2, tick = T, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = 1)
				
			mtext(side = 1, text = expression("PM2.5 Concentration"),line = 2,cex = 1)	
			mtext(side = 2, text = "Infant mortality rate (deaths per 1,000 births)",line = 3.75,cex = 1)
					





#################################
##### Panel c: West vs. Rest of Africa
#################################
	

		par(mar = c(4,2,2,3))
		
		plot(response_c_west$x, response_c_west$y,type = 'l',col = NA, lwd = 3,ylim =  c(-0.0025,0.125),xlim = c(-1,71),
			axes = F,xlab = "",ylab = "")
	
		
		polygon(ci_polygon_c_west$px, ci_polygon_c_west$py, col = add.alpha(col_shade_c1, 0.3),border =NA)
		polygon(ci_polygon_c_other$px, ci_polygon_c_other$py, col = add.alpha(col_shade_c2, 0.4),border =NA)
	
		lines(response_c_west$x, response_c_west$y, lwd = 3, col = col_line_c1)		
		lines(response_c_other$x, response_c_other$y, lwd = 3, col = col_line_c2)		

		text(x = 7.5, y = response_c_west$y[5]+0.013, labels = "Rest of \n Africa",pos = 1, col=  'black')
		text(x = 66, y = response_c_other$y[35]+0.0075, labels = "West \n Africa",pos = 1, col = 'black')
		
		plotHist(hist_ht_c_west, col_shade_c1, 0.4, 0.0175, 0.0125, add.alpha(col_line_c1,0.4))
		plotHist(hist_ht_c_other, col_shade_c2, 0.3, 0, 0.0125, add.alpha(col_line_c2,0.2) )


		axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.25)
		axis(2, tick = T, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = 1)
			
		mtext(side = 1, text = expression("PM2.5 Concentration"),line = 2,cex = 1)	
		mtext(side = 2, text = "Infant mortality rate (deaths per 1,000 births)",line = 3.75,cex = 1)





# ################################
# #### Panel d: Rich vs Poor
# ################################

		par(mar = c(4,2,2,3))
		
		plot(1:3, coef_d$y,pch = 15,ylim = c(-0.01, 0.026),xlim = c(0.5,3.5), ylab = "",xlab = "",axes=F, col=  NA)	
			segments(y0 = 0, y1 = 0, x0 = 0.5, x1 = 10.5, col = 'red',lty=1,lwd=0.25)
			
			
		#plot coefficient estimates with error bars for each decile	
		segments(x0 = 1:3 ,y0 = coef_d$ci_low,y1 = coef_d$ci_high,lwd=1.5, col = col_d)
		points(1:3, coef_d$y, pch=16, cex = 2.5, col = col_d)
		
		#labels	
		axis(1, tick =F, at = 1:3,labels = NA,line = -2)
		axis(2, tick = T, las =2,line = 1, at = seq(-.01, .025, 0.005), labels = seq(-.01, .025, 0.005)*1000)						
		
		mtext(side = 1, text = "Wealth Level",cex = 1, line=2)
		mtext(side = 2, text = "Change in infant mortality rate (% per 10 ug/m3)",line= 3.75)
	
		text(x = 1, y = -0.0075, labels = "Poor")
		text(x = 2, y = -0.0075, labels = "Middle")
		text(x = 3, y = -0.0075, labels = "Wealthy")







# #########################################
# #### Panel e: Over Time
# #########################################

		par(mar = c(4,2,2,3))
		
		plot(1:4, coef_e$y,col = NA,ylim = c(-0.01, 0.026),ylab ="", xlab = "",bty = "n",axes=F,xlim =c(0.5, 4.5))
			segments(y0 = 0, y1 = 0, x0 = 0.5, x1 = 4.5, col = 'red',lty=1,lwd=0.25)
		
		#plot coefficient estimates with error bars for each decile	
		segments(x0 = 1:4 ,y0 = coef_e$ci_low,y1 = coef_e$ci_high,lwd=1.5, col = col_e)
		points(1:4, coef_e$y, pch=16, cex = 2.5, col = col_e)
		
		#labels	
		axis(1, at = 1:4, labels = c('2001-\n2003', '2004-\n2006','2007-\n2009','2010-\n2012'), las=1,line=-2.5,tick=F,cex.axis = 1.2)
		axis(2, tick = T, las =2,line = 1, at = seq(-.01, .025, 0.005), labels = seq(-.01, .025, 0.005)*1000)						

		mtext(side = 1, text = "Time Period",cex = 1, line=2)
		mtext(side = 2, text = "Change in infant mortality rate (% per 10 ug/m3)",line= 3.75)

		





dev.off()
