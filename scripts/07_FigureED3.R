source("scripts/helper_functions.R")
data <- read_rds("data/inputs/analysis_data.rds")
  data$cid <- as.numeric(as.factor(data$country))
  theta <- mean(data$child_die_age1)
  load("data/figure_data/Fig2_data.RData") #use main response curve plotted in Fig. 2a as comparison for all othe specs
  
   
  
  
#Specify regression
	regression_controls <- "tmp_pre + tmp_post + rain_pre + rain_post + child_sex + child_birth_order + child_multi_birth + poly(mother_age_at_birth, 2, raw=T) + mother_education + hh_clean_cook_fuel + hh_nightlights"
	regression_fixed_effects <- "fe_loc + fe_season + fe_time"
	regression_var_cluster <- "fe_loc"

	equation_1 <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                               regression_controls, "|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))


	 
	 
	 #time trends
	 cty <- sort(unique(data$cid))
	 data$time <- data$child_birth_year - 2000
	 
	 for (y in 1:length(cty)){				
	   data[,paste("trend",cty[y],sep="")] <-(as.numeric(data$cid == cty[y]))* data$time			
	 }
	 
	 for (y in 1:length(cty)){				
	   data[,paste("trendQuad",cty[y],sep="")] <-(as.numeric(data$cid == cty[y]))* data$time^2			
	 }
	 
	 
	 
	 
###### panel (a) ######

	 #polynomials
	 model2 <- summary(felm(as.formula(paste("child_die_age1 ~ poly(pm25_post, 2, raw = T) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))

	 model3 <- summary(felm(as.formula(paste("child_die_age1 ~ poly(pm25_post, 3, raw = T) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))
	 
	 model4 <- summary(felm(as.formula(paste("child_die_age1 ~ poly(pm25_post, 4, raw = T) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))

	 
	 
	###### panel (c) ######

	 #splines
	 model5 <- summary(felm(as.formula(paste("child_die_age1 ~ ns(pm25_post, knots = 10) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))
	 
	 model6 <- summary(felm(as.formula(paste("child_die_age1 ~ ns(pm25_post, knots = c(10,15,25,35)) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))
	 
	 model7 <- summary(felm(as.formula(paste("child_die_age1 ~ ns(pm25_post,3) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))
	 
	 model8 <- summary(felm(as.formula(paste("child_die_age1 ~ ns(pm25_post, 4) + pm25_pre + pm25_pre_squared + ", 
	                                         regression_controls, "|", regression_fixed_effects, sep = "")), data = data, weights = data[,"reg_wt"]))
	 
	 
	 
	 
	 x<- response_a$x
	 ind <- which(x==round(mean(data$pm25_post,na.rm = T)))
	 y2 = as.numeric(t(as.matrix((model2)$coefficients[1:2,"Estimate"]))%*%t(matrix(nrow = length(x), ncol = 2, 	
	                                                                                data = c(x, x^2))))
	 y3 = as.numeric(t(as.matrix((model3)$coefficients[1:3,"Estimate"]))%*%t(matrix(nrow = length(x), ncol = 3, 	
	                                                                                data = c(x, x^2, x^3))))
	 y4 = as.numeric(t(as.matrix((model4)$coefficients[1:4,"Estimate"]))%*%t(matrix(nrow = length(x), ncol = 4, 
	                                                                                data = c(x, x^2, x^3,x^4))))
	 y5 = as.numeric(t(as.matrix((model5)$coefficients[1:2,"Estimate"]))%*%t(ns(x, knots = 10)))
	 y6 = as.numeric(t(as.matrix((model6)$coefficients[1:5,"Estimate"]))%*%t(ns(x, knots = c(10,15,25,35))))				
	 y7 = as.numeric(t(as.matrix((model7)$coefficients[1:3,"Estimate"]))%*%t(ns(x,3)))
	 y8 = as.numeric(t(as.matrix((model8)$coefficients[1:4,"Estimate"]))%*%t(ns(x,4)))
	 
	 vertical <- mean(data$child_die_age1)
	 
	 y2 = y2 - y2[ind] + vertical; y3 = y3 - y3[ind] + vertical; y4 = y4 - y4[ind] + vertical; 
	 y5 = y5 - y5[ind] + vertical; y6 = y6 - y6[ind] + vertical; y7 = y7 - y7[ind] + vertical;
	 y8 = y8 - y8[ind] + vertical;
	 
	 
	 
###### panel (d) ######
	 
	
						
				#fe
				
    				#country x year FE
    				model1 <- summary(felm(as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
    				                                        regression_controls, "|", "fe_loc + fe_season + fe_country_year", sep = "")), 
    				                       data = data, weights = data[,"reg_wt"]))
    				#mother FE
    				model2 <- summary(felm(as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
    				                                        regression_controls, "|", "fe_mom + fe_season + fe_time", sep = "")), 
    				                       data = data, weights = data[,"reg_wt"]))
    				#survey-yr FE
    				model3 <- summary(felm(as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
    				                                        regression_controls, "|", "fe_svy_yr + fe_loc + fe_season + fe_time", sep = "")), 
    				                       data = data, weights = data[,"reg_wt"]))
    				
				
				
				
			
				#time trends	
					linTrend <- paste(paste0("trend",1:29,"+", collapse = ""), "trend30", sep = "")				
					fmla <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                               regression_controls, " + ", linTrend,"|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))							
	                model5 <- summary(felm(fmla, data = data, weights = data[,"reg_wt"]))
					
					QuadTrend <- paste(paste0("trendQuad", 1:29,"+", collapse = ""), "trendQuad30", sep = "")				
					fmla <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", 
	                               regression_controls, " + ", QuadTrend,"|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))							
					model6 <- summary(felm(fmla, data = data, weights = data[,"reg_wt"]))
					
					
					linTrend <-paste0("trend", 1:30,"+", collapse = "")				
					QuadTrend <- paste(paste0("trendQuad", 1:29,"+", collapse = ""), "trendQuad30", sep = "")				
					fmla <- as.formula(paste("child_die_age1 ~ pm25_post + pm25_pre + pm25_pre_squared + ", regression_controls, " + ",
						 				linTrend ,QuadTrend,"|", regression_fixed_effects  ,"| 0 |",regression_var_cluster , sep = ""))
					model7 <- summary(felm(fmla, data = data, weights = data[,"reg_wt"]))				
									
	
					
																	
						
						x<- response_a$x
						ind <- which(x==round(mean(data$pm25_post,na.rm = T)))
						
						yb1 =c((model1)$coefficients[1:1,"Estimate"])*x
						yb2 =c((model2)$coefficients[1:1,"Estimate"])*x
						yb3 =c((model3)$coefficients[1:1,"Estimate"])*x
						yb4 =c((model4)$coefficients[1:1,"Estimate"])*x
						yb5 =c((model5)$coefficients[1:1,"Estimate"])*x
						yb6 =c((model6)$coefficients[1:1,"Estimate"])*x
						yb7 =c((model7)$coefficients[1:1,"Estimate"])*x
						
						
					
						
						vertical <- mean(data$child_die_age1)
						
						yb1 = yb1 - yb1[ind] + vertical; yb2 = yb2 - yb2[ind] + vertical; yb3 = yb3 - yb3[ind] + vertical; 
						yb4 = yb4 - yb4[ind] + vertical; yb5 = yb5 - yb5[ind] + vertical; yb6 = yb6 - yb6[ind] + vertical; 
						yb7 = yb7 - yb7[ind] + vertical;
										
								
			
			
			
			
pdf(file = "figures/raw/FigED3_raw.pdf",width = 16, height= 12)							
		  
					  par(mfrow = c(2,2))
		        par(mar = c(4,7,1,1))

				#(a) polynomial panel
		
		        #main specification (from Fig 2.a) 
						plot(response_a$x,response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
						axes = F,xlab = "",ylab = "")
  						  polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha('#77AADD',0.25),border =NA)
  						  lines(response_a$x,response_a$y, lwd = 2, col = add.alpha('navyblue', 1))		
  						  plotHist(hist_ht_a,"dodgerblue", 0.2, 0, 0.025, 'white' )
  						
						axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.25)
						axis(2, tick = F, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=1.25)
						mtext(side = 2, text = "IMR (deaths per 1,000)",line = 3.5,cex = 0.75)
			
			      #alternative polynomials
						lines(x, y2, col = 'red3', lwd = 2, lty = 1) #quadratic
						lines(x, y3, col = 'gold', lwd = 2, lty = 1) #cubic
						lines(x, y4, col = 'forestgreen', lwd = 2, lty =1) #4th deg
						
						
				#(b) different FE 
						
						plot(response_a$x,response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
						axes = F,xlab = "",ylab = "")
						    polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha('#77AADD',0.25),border =NA)
						    lines(response_a$x,response_a$y, lwd = 2, col = add.alpha('navyblue', 1))		
						    plotHist(hist_ht_a,"dodgerblue", 0.2, 0, 0.025, 'white' )

						lines(x, yb1, col = 'red3', lwd = 2) #country x year
						lines(x, yb2, col = 'gold', lwd = 2) #mom fe
						lines(x, yb3, col = 'forestgreen', lwd = 2) #svy yr FE						    
						    						
						axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.25)
						axis(2, tick = F, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=1.25)
						mtext(side = 2, text = "IMR (deaths per 1,000)",line = 3.5,cex = 0.75)

				#(c) spline panel
						plot(response_a$x,response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
							axes = F,xlab = "",ylab = "")
						    polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha('#77AADD',0.25),border =NA)
						    lines(response_a$x,response_a$y, lwd = 2, col = add.alpha('navyblue', 1))		
						    plotHist(hist_ht_a,"dodgerblue", 0.2, 0, 0.025, 'white' )

						lines(x, y5, col = 'red3', lwd = 2, lty = 1) #ns 10
						lines(x, y7, col = 'forestgreen', lwd = 2, lty =1) #ns flex 4
						lines(x, y8, col = 'gold', lwd = 2, lty =1) #ns flex 4
						
						axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis = 1.25)
						axis(2, tick = F, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=1.25)
						mtext(side = 1, text = expression("PM"[2.5]~"Exposure"),line = 2.75,cex = 1)	
						mtext(side = 2, text = "IMR (deaths per 1,000)",line = 3.5,cex = 0.75)			

				#(d) different trend controls panel
				
						par(family = "Helvetica")
						plot(response_a$x,response_a$y,type = 'l',col = NA, lwd = 3,ylim = c(-0.0025,0.125),xlim = c(-1,71),
						    axes = F,xlab = "",ylab = "")
						    polygon(ci_polygon_a$px, ci_polygon_a$py, col = add.alpha('#77AADD',0.25),border =NA)
						    lines(response_a$x,response_a$y, lwd = 2, col = add.alpha('navyblue', 1))		
						    plotHist(hist_ht_a,"dodgerblue", 0.2, 0, 0.025, 'white' )

						lines(x, yb5, col = 'red3',lwd = 2)
						lines(x, yb6, col = 'forestgreen',lwd = 2)
						lines(x, yb7, col = 'gold',lwd = 2)
						    
						axis(1, tick=F, at = seq(0,80,10),col.axis='gray30',line=-1,cex.axis =1.25)
						axis(2, tick = F, at = seq(0, 0.125, .025),labels =seq(0, 0.125, .025)*1000, las =2 ,line = -1, cex.axis=1.25)
						mtext(side = 1, text = expression("PM"[2.5]~"Exposure"),line = 2.75,cex = 1)	
						mtext(side = 2, text = "IMR (deaths per 1,000)",line = 3.5,cex = 0.75)
			
			
			
dev.off()		




