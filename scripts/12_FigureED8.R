#*******************************************************************************************************************    	
#**************       NOTE: This code is slow because of bootstrapping. It takes >1 hr to run.               *******
#*************        Can remove Nboot and bootexpr arguments from felm() for quick results w/out CI.        *******    	
#*******************************************************************************************************************

source("scripts/helper_functions.R")
data <- read_rds("data/inputs/analysis_data.rds")

pm25_support <- 0:70 #below range of estimated imr|pm2.5 is adjusted empirically to cover inner 99 percentile of observed pm2.5 concentrations
alpha = 0.05

	#define main components of regression model (names refer to columns in "data" object)
			#mom fe so can only include child specific controls 	
			regression_controls_pre <- "tmp_pre  + rain_pre  + child_sex + child_birth_order + child_multi_birth"			
			regression_controls_post <- "tmp_pre  + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth"			
			regression_fixed_effects <- "fe_mom + fe_season + fe_time"
			regression_var_cluster <- "fe_loc"
			   




### Panel a: Birth Weight

						   
			equation_a <- as.formula(paste("child_birth_wt ~ pm25_pre +", 
			                               regression_controls_pre, "|", regression_fixed_effects ,"| 0 |", regression_var_cluster , sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_a <- felm(equation_a, data = data, Nboot = 500, bootexpr = quote(pm25_pre*(pm25_support))) 
		
		
			bstrap_a <- model_a$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_a <- apply(bstrap_a, 2, function(x){center(x,pm25_support, round(mean(data$pm25_pre)), mean(data$child_birth_wt, na.rm = T))}) 			
			#define response function
			response_a <- tibble(x = pm25_support,  y = apply(bstrap_a, 1, median), #bring in x and y
					             ci_low = apply(bstrap_a, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_a, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_a$x[1], response_a$x, response_a$x[nrow(response_a):1])
			py <- c(response_a$ci_low[1], response_a$ci_high, response_a$ci_low[nrow(response_a):1])	
			ci_polygon_a <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_a <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			
		


### Panel b: Birth Size
		   
			equation_b <- as.formula(paste("child_birth_size ~ pm25_pre +", 
			                               regression_controls_pre, "|", regression_fixed_effects  ,"| 0 |", regression_var_cluster , sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_b <- felm(equation_b, data = data, Nboot = 500, bootexpr = quote(pm25_pre*(pm25_support))) 
		
			bstrap_b <- model_b$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_b <- apply(bstrap_b, 2, function(x){center(x,pm25_support, round(mean(data$pm25_pre)), mean(data$child_birth_size, na.rm = T))}) 			
			#define response function
			response_b <- tibble(x = pm25_support,  y = apply(bstrap_b, 1, median), #bring in x and y
					             ci_low = apply(bstrap_b, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_b, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_b$x[1], response_b$x, response_b$x[nrow(response_b):1])
			py <- c(response_b$ci_low[1], response_b$ci_high, response_b$ci_low[nrow(response_b):1])	
			ci_polygon_b <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_b <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
	



### Panel c: Neo-Natal Mortality

			   
			equation_c <- as.formula(paste("child_die_month1 ~ pm25_pre + pm25_pre_squared + ", 
			                               regression_controls_pre, "|", regression_fixed_effects  ,"| 0 |", regression_var_cluster , sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_c <- felm(equation_c, data = data, Nboot = 500, bootexpr = quote(pm25_pre*(pm25_support) + pm25_pre_squared*(pm25_support^2))) 
		
			bstrap_c <- model_c$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_c <- apply(bstrap_c, 2, function(x){center(x,pm25_support, round(mean(data$pm25_pre)), mean(data$child_die_month1, na.rm = T))}) 			
			#define response function
			response_c <- tibble(x = pm25_support,  y = apply(bstrap_c, 1, median), #bring in x and y
					             ci_low = apply(bstrap_c, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_c, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_c$x[1], response_c$x, response_c$x[nrow(response_c):1])
			py <- c(response_c$ci_low[1], response_c$ci_high, response_c$ci_low[nrow(response_c):1])	
			ci_polygon_c <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_c <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			



### Panel d: Stunting

			equation_d <- as.formula(paste("child_stunting ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
			                               regression_controls_post, "|", regression_fixed_effects ,"| 0 |", regression_var_cluster, sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_d <- felm(equation_d, data = data, Nboot = 500, bootexpr = quote(pm25_post*(pm25_support))) 
		
			bstrap_d <- model_d$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_d <- apply(bstrap_d, 2, function(x){center(x,pm25_support, round(mean(data$pm25_post)), mean(data$child_stunting, na.rm = T))}) 			
			#define response function
			response_d <- tibble(x = pm25_support,  y = apply(bstrap_d, 1, median), #bring in x and y
					             ci_low = apply(bstrap_d, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_d, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_post, 0.01) & x < quantile(data$pm25_post, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_d$x[1], response_d$x, response_d$x[nrow(response_d):1])
			py <- c(response_d$ci_low[1], response_d$ci_high, response_d$ci_low[nrow(response_d):1])	
			ci_polygon_d <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_d <- get_hst_obj(data$pm25_post, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			

### Panel e: Diarrhea

			equation_e <- as.formula(paste("child_diarrhea ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
			                               regression_controls_post, "|", regression_fixed_effects ,"| 0 |", regression_var_cluster, sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_e <- felm(equation_e, data = data, Nboot = 500, bootexpr = quote(pm25_post*(pm25_support))) 
		
			bstrap_e <- model_e$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_e <- apply(bstrap_e, 2, function(x){center(x,pm25_support, round(mean(data$pm25_post)), mean(data$child_diarrhea, na.rm = T))}) 			
			#define response function
			response_e <- tibble(x = pm25_support,  y = apply(bstrap_e, 1, median), #bring in x and y
					             ci_low = apply(bstrap_e, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_e, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_post, 0.01) & x < quantile(data$pm25_post, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_e$x[1], response_e$x, response_e$x[nrow(response_e):1])
			py <- c(response_e$ci_low[1], response_e$ci_high, response_e$ci_low[nrow(response_e):1])	
			ci_polygon_e <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_e <- get_hst_obj(data$pm25_post, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			


### Panel f: Child Sex

			data$child_sex <- as.numeric(data$child_sex)
			equation_f <- as.formula(paste("child_sex ~ pm25_pre + ", 
			                               "tmp_pre  + rain_pre + child_birth_order + child_multi_birth", #remove child_sex from covariates to avoid collinearity
			                               "|", regression_fixed_effects ,"| 0 |", regression_var_cluster, sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_f <- felm(equation_f, data = data, Nboot = 500, bootexpr = quote(pm25_pre*(pm25_support))) 
		
			bstrap_f <- model_f$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_f <- apply(bstrap_f, 2, function(x){center(x,pm25_support, round(mean(data$pm25_pre)), mean(data$child_sex, na.rm = T))}) 			
			#define response function
			response_f <- tibble(x = pm25_support,  y = apply(bstrap_f, 1, median), #bring in x and y
					             ci_low = apply(bstrap_f, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_f, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_f$x[1], response_f$x, response_f$x[nrow(response_f):1])
			py <- c(response_f$ci_low[1], response_f$ci_high, response_f$ci_low[nrow(response_f):1])	
			ci_polygon_f <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_f <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
			

### Panel g: Multi-birth
	
	
			equation_g <- as.formula(paste("child_multi_birth ~ pm25_pre + pm25_post  + ", 
			                              "tmp_pre  + tmp_post + rain_pre + rain_post + child_sex","|", regression_fixed_effects ,"| 0 |", regression_var_cluster, sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_g <- felm(equation_g, data = data, Nboot = 500, bootexpr = quote(pm25_pre*(pm25_support))) 
		
			bstrap_g <- model_g$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_g <- apply(bstrap_g, 2, function(x){center(x,pm25_support, round(mean(data$pm25_pre)), mean(data$child_multi_birth, na.rm = T))}) 			
			#define response function
			response_g <- tibble(x = pm25_support,  y = apply(bstrap_g, 1, median), #bring in x and y
					             ci_low = apply(bstrap_g, 1, function(x){quantile(x, alpha/2)}), 
					             ci_gigh = apply(bstrap_g, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_g$x[1], response_g$x, response_g$x[nrow(response_g):1])
			py <- c(response_g$ci_low[1], response_g$ci_gigh, response_g$ci_low[nrow(response_g):1])	
			ci_polygon_g <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_g <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_gst_obj() defined in "plotting_gunctions.R" file.
	
			
			
		

### Panel h: Bednet Use

			equation_h <- as.formula(paste("child_bed_net ~ pm25_post  +", 
			                               "tmp_pre  + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + mother_education + poly(mother_age_at_birth, 2, raw=T) ","|", 
			                               "fe_loc + fe_season + fe_time" ,"| 0 |", regression_var_cluster, sep = ""))
											#looks like too many siblings have same bed net binary value so must use cluster fe
			
			#bootstrap pm25_pre dose-response function 500 times
			model_h <- felm(equation_h, data = data, Nboot = 500, bootexpr = quote(pm25_post*(pm25_support))) 
		
			bstrap_h <- model_h$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_h <- apply(bstrap_h, 2, function(x){center(x,pm25_support, round(mean(data$pm25_post)), mean(data$child_bed_net, na.rm = T))}) 			
			#define response function
			response_h <- tibble(x = pm25_support,  y = apply(bstrap_h, 1, median), #bring in x and y
					             ci_low = apply(bstrap_h, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_h, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_pre, 0.01) & x < quantile(data$pm25_pre, 0.99))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_h$x[1], response_h$x, response_h$x[nrow(response_h):1])
			py <- c(response_h$ci_low[1], response_h$ci_high, response_h$ci_low[nrow(response_h):1])	
			ci_polygon_h <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_h <- get_hst_obj(data$pm25_pre, 0,80,1,0.99) #get_hst_obj() defined in "plotting_hunctions.R" file.
			
	
	
	


### Panel i: Second year pm2.5 exposure

			equation_i <- as.formula(paste("child_die_age1 ~ pm25_after + ", 
			                               regression_controls_post, "|", regression_fixed_effects ,"| 0 |", regression_var_cluster, sep = ""))
			
			#bootstrap pm25_pre dose-response function 500 times
			model_i <- felm(equation_i, data = data, Nboot = 500, bootexpr = quote(pm25_after*(pm25_support))) 
		
			bstrap_i <- model_i$boot #pull out bootstrap runs
		
			#re-center to (mean pm, mean imr)
			bstrap_i <- apply(bstrap_i, 2, function(x){center(x,pm25_support, round(mean(data$pm25_after, na.rm = T)), mean(data$child_die_age1, na.rm = T))}) 			
			#define response function
			response_i <- tibble(x = pm25_support,  y = apply(bstrap_i, 1, median), #bring in x and y
					             ci_low = apply(bstrap_i, 1, function(x){quantile(x, alpha/2)}), 
					             ci_high = apply(bstrap_i, 1, function(x){quantile(x, 1-alpha/2)}) ) %>%  #generate ci
					  filter(x > quantile(data$pm25_after, 0.01,na.rm =T) & x < quantile(data$pm25_after, 0.99,na.rm =T))  #keep inner 99 percentile oberved pm conc.
					
			#re-organize x and y value into structure for plotting with polygon() 	
			px <- c(response_i$x[1], response_i$x, response_i$x[nrow(response_i):1])
			py <- c(response_i$ci_low[1], response_i$ci_high, response_i$ci_low[nrow(response_i):1])	
			ci_polygon_i <- tibble(px, py)
			
			#define object that stores info on breaks and heights of histogram at bottom of panel (a)
			hist_ht_i <- get_hst_obj(data$pm25_post, 0,80,1,0.99) #get_hst_obj() defined in "plotting_functions.R" file.
		
	









######## define plot colors #########

		#Derived from Paul Tol's palettes designed to be distinct in color-blind vision
		#https://personal.sron.nl/~pault/
			col_line_1 <- "#114477" 
			col_shade_1 <- "#77AADD"
			col_line_2 <- "#117744"
			col_shade_2 <- "#44AA77"
			col_line_3 <-"#DDAA77"
			col_shade_3 <-"#AA7744"
			alpha = 0.05
			
	
		
######## ######## plot ######### ########

pdf(file = "figures/raw/FigED8_raw.pdf",width = 15, height= 10, useDingbats = F)	
    
    mat <- rbind(c(1,1,2,2,3,3),c(1,1,2,2,3,3),c(4,4,5,5,6,7),c(4,4,5,5,8,9) )
    layout(mat)
    
    par(mar = c(2,6,0,2))
    par(oma = c(2,0,0,2))
        
    
    ###############################
    #### Panel a: Birth Weight
    ################################
    
     			plot(response_a$x, response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(2975,3350),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    			polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha(col_shade_1,0.25),border =NA)
    			lines(response_a$x, response_a$y, lwd = 3, col = col_line_1)		
    			plotHist(hist_ht_a,col = col_shade_1, 0.2, 2975, 90, border.col = add.alpha(col_line_1,.05) )
    			
    			#axis
    			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    			axis(2, tick = F, at = seq(3000, 3400, 100),labels =seq(3000, 3400, 100), las =2 ,line = -1, cex.axis=1.5)
    
    			#labels
    			mtext(side = 1, text = "In-Utero PM2.5 Concentration (ug/m3)",line = 3,cex = 1.25)	
    			mtext(side = 2, text = "Birth Weight (grams)",line = 3.5,cex = 1.25)
    
    
 
    ##################################
    ##### Panel b: Birth Size
    #################################
    
    			plot(response_b$x, response_b$y,type = 'l',col = NA, lwd = 3,ylim = c(3.1,3.35),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    
    			polygon(ci_polygon_b$px, ci_polygon_b$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_b$x, response_b$y, lwd = 3, lty=1, col = col_line_1)

    			plotHist(hist_ht_b, col_shade_1, 0.2, 3.1, .06, add.alpha(col_line_1,0.05) )

    			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    			axis(2, tick = F, at = seq(3.0, 3.4, .05),labels =seq(3, 3.4, .05), las =2 ,line = -1, cex.axis=1.5)
    
    			mtext(side = 1, text = expression("In-utero PM"[2.5]~"Concentration"),line = 3,cex = 1.25)	
    			mtext(side = 2, text = "Birth Size (smallest-1 to largest-5)",line = 3.5,cex = 1.25)
    

    #################################
    ##### Panel c: NMR
    #################################
  
    			plot(response_c$x, response_c$y,type = 'l',col = NA, lwd = 3,ylim = c(0,0.051),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
     
    			polygon(ci_polygon_c$px, ci_polygon_c$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_c$x, response_c$y, lwd = 3, lty=1, col = col_line_1)
     
    			plotHist(hist_ht_c, col_shade_1, 0.2, 0, .01175, add.alpha(col_line_1,0.05) )
  
    			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.5)
    			axis(2, tick = F, at = seq(0, 0.08, 0.01),labels =seq(0, 0.08, 0.01)*1000, las =2 ,line = -1, cex.axis=1.5)
    
    			mtext(side = 1, text = expression("In-utero PM"[2.5]~"Concentration"),line = 3,cex = 1.25)	
    			mtext(side = 2, text = "NMR (deaths per 1,000 births)",line = 3.5,cex = 1.25)
    
    
     
    # ################################
    # #### Panel d: Stunting
    # ################################
    
    			plot(response_d$x, response_d$y,type = 'l',col = NA, lwd = 3,ylim = c(-300,-50),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_d$px, ci_polygon_d$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_d$x, response_d$y, lwd = 3, lty=1, col = col_line_1)
 
    			plotHist(hist_ht_d, col_shade_1, 0.2, -300, 50, add.alpha(col_line_1,0.05) )
    
     			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    			axis(2, tick = F, at = seq(-300, 0, 50),labels =seq(-300, 0, 50)/100, las =2 ,line = -1, cex.axis=1.5)
    
    			mtext(side = 1, text = expression("Post-birth PM"[2.5]~"Concentration"),line = 3,cex = 1.25)	
    			mtext(side = 2, text = "height/age standard deviation",line = 3.5,cex = 1.25)
    
    
    
    # #########################################
    # #### Panel e: Diarrhea
    # #########################################
    
    			plot(response_e$x, response_e$y,type = 'l',col = NA, lwd = 3,ylim = c(0,.4),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_e$px, ci_polygon_e$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_e$x, response_e$y, lwd = 3, lty=1, col = col_line_1)
    
    			plotHist(hist_ht_e, col_shade_1, 0.2, 0, 0.075, add.alpha(col_line_1,0.05) )
    
    			axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    			axis(2, tick = F, at = seq(0,1,.1),las =2 ,line = -1, cex.axis=1.5)
    
    			mtext(side = 1, text = expression("Post-birth PM"[2.5]~"Concentration"),line = 3,cex = 1.25)	
    			mtext(side = 2, text = "Share diarrhea in past 2 weeks",line = 3.5,cex = 1.25)
    
    
    
    ########################################################################################################################
    ### Placebos:
    ########################################################################################################################
    		par(mar = c(3,7,2,0))			
    			
    			
    # #########################################
    # #### Panel f: Child Sex
    # #########################################
    
    			plot(response_f$x, response_f$y,type = 'l',col = NA, lwd = 3,ylim = .3+c(0,.4),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_f$px, ci_polygon_f$py-1, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_f$x, response_f$y-1, lwd = 3, lty=1, col = col_line_1)
    
    			plotHist(hist_ht_f, col_shade_1,0.2, 0.3, .1, add.alpha(col_line_1,0.05) )
    
    				axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    				axis(2, tick = F, at = seq(0.3,.7,.1),las =2 ,line = -1, cex.axis=1.5)
    	
    				mtext(side = 1, text = expression("In-utero PM"[2.5]~"Con."),line = 3,cex = 1)	
    				mtext(side = 2, text = "Sex (female = 1)",line = 2.5,cex = 1.25)
    
    
    # #########################################
    # #### Panel g: Twins
    # #########################################
    
    			plot(response_g$x, response_g$y,type = 'l',col = NA, lwd = 3,ylim =c(-0.02,.1),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_g$px, ci_polygon_g$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_g$x, response_g$y, lwd = 3, lty=1, col = col_line_1)
    
    			plotHist(hist_ht_g, col_shade_1,0.2, -0.02, 0.03, add.alpha(col_line_1,0.05) )
    
    				axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    				axis(2, tick = F, at = seq(0,.1,.025),las =2 ,line = -1, cex.axis=1.5)
    	
    				mtext(side = 1, text = expression("In-utero PM"[2.5]~"Con."),line = 3,cex = 1)	
    				mtext(side = 2, text = "Multiple Birth",line = 3.5,cex = 1.25)
    
    
    
    
    # #########################################
    # #### Panel h: Bed-Net Use
    # #########################################
    
    			plot(response_h$x, response_h$y,type = 'l',col = NA, lwd = 3,ylim =c(0.2,0.6),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_h$px, ci_polygon_h$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_h$x, response_h$y, lwd = 3, lty=1, col = col_line_1)
    
    			plotHist(hist_ht_h, col_shade_1,0.2, 0.2, .12, add.alpha(col_line_1,0.05) )
    
    				axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    				axis(2, tick = F, at = seq(0.2, 0.6, .1),labels = seq(0.2, 0.6, .1), las =2 ,line = -1, cex.axis=1.5)
    	
    				mtext(side = 1, text ="Postbirth PM2.5 Concentration",line = 3,cex = 1)	
    				mtext(side = 2, text = "Bednet",line = 2.5,cex = 1.25)
    
    
    
    
    
    # #########################################
    # #### Panel i: PM in year 2
    # #########################################
    
    	#plot
    			plot(response_i$x, response_i$y,type = 'l',col = NA, lwd = 3,ylim =c(0,.125),xlim = c(-1,71),
    				axes = F,xlab = "",ylab = "")
    		
    			polygon(ci_polygon_i$px, ci_polygon_i$py, col = add.alpha(col_shade_1, 0.25),border =NA)
    			lines(response_i$x, response_i$y, lwd = 3, lty=1, col = col_line_1)
    
    			plotHist(hist_ht_i, col_shade_1,0.25, 0, 0.035, add.alpha(col_line_1,0.05) )
    
    				axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-0.5,cex.axis = 1.5)
    				axis(2, tick = F, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=1.5)
    	
    				mtext(side = 1, text = expression("second-year PM25 conc."),line = 3,cex = 1)	
    				mtext(side = 2, text = "Infant Mortality Rate",line = 2.5,cex = 1.25)
    
    
    




dev.off()