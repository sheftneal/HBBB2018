source("scripts/helper_functions.R") 
load("data/figure_data/figED5_data.RData")							
							
							
						
pdf(file ="figures/raw/FigED5_raw.pdf",width =15, height = 6, useDingbats = F)	
				
		plot(-2:27, rep(0,30),axes=F, ylim = c(-.02, 0.04),xlim = c(-2.5,27.5),ylab = "",xlab = "", col=  NA)
		
		n = 1
		segments(x0 = -1:0 , lwd = 2,
					y0 = (hte[[n]]$est+2* hte[[n]]$se), y1 = (hte[[n]]$est-2*  hte[[n]]$se))
		points(-1:0, hte[[n]]$est,pch=16,cex=2)
		text(x = -1, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.01, label = "Later Child", las =1,srt=90)	
		text(x = 0, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.01, label = "First Child", las =1,srt=90)	
			
		n = 2	
		segments(x0 = 2:3 , lwd = 2,
					y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(2:3, hte[[n]]$est,pch=16, cex=2)
		text(x = 2, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.0075, label = "Urban", las =1,srt=90)	
		text(x = 3, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.0075, label = "Rural", las =1,srt=90)	
			
		n = 3	
		segments(x0 = 5:6 , lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(5:6, hte[[n]]$est,pch=16, cex=2)
		text(x = 5, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.0095 ,label = "Older Mom", las =1,srt=90)
		text(x = 6, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.0095, label = "Teen Mom", las =1,srt=90)	
					
		n = 4	
		segments(x0 = 8:9 , lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(8:9, hte[[n]]$est,pch=16, cex=2)
		text(x = 8, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.009, label = " Primary", las =1,srt=90)	
		text(x = 9, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.009, label = " Secondary", las =1,srt=90)	
	
		n = 5	
		segments(x0 = 11:12 , lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(11:12, hte[[n]]$est,pch=16, cex=2)
		text(x = 11, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.0075, label = "Female", las =1,srt=90)	
		text(x = 12, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.0075, label = "Male", las =1,srt=90)	

	
		n = 6	
		segments(x0 = 14:15 , lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(14:15, hte[[n]]$est,pch=16, cex=2)
		text(x = 14, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.005, label = "Low", las =1,srt=90)	
		text(x = 15, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.005, label = "High", las =1,srt=90)	

		n = 7	
		segments(x0 = 17:18 ,lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(17:18, hte[[n]]$est,pch=16, cex=2)
		text(x = 17, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.005, label = "Dirty", las =1,srt=90)	
		text(x = 18, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.005, label = "Clean", las =1,srt=90)	

		n = 8	
		segments(x0 = 20:21 , lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(20:21, hte[[n]]$est,pch=16, cex=2)
		text(x = 20, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.01, label = "Below Median", las =1,srt=90)	
		text(x = 21, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.01, label = "Above Median", las =1,srt=90)	

		n = 9	
		segments(x0 = 23:24 ,lwd = 2,
				y1 =  (hte[[n]]$est+2* hte[[n]]$se), y0 =  (hte[[n]]$est-2*  hte[[n]]$se))
		points(23:24, hte[[n]]$est,pch=16, cex=2)
		text(x = 23, y = hte[[n]]$est[1]+2* hte[[n]]$se[1] + 0.008, label = "Before 2006", las =1,srt=90)	
		text(x = 24, y = hte[[n]]$est[2]+2* hte[[n]]$se[2] + 0.008, label = "After 2006", las =1,srt=90)	

					
		#main effect
		segments(x0 = 26 ,lwd = 2,
				 y1 =  (results[1]+2* results[2]), y0 =  (results[1]-2* results[2]))
		points(26, results[1], pch=16, cex=2)
		
					
		#labels	
		segments(x0 = -2, x1= 27, y0 = 0, lwd = 0.5, lty=1, col = 'red')	
		axis(1, tick =F, at = c(-0.5,2.5, 5.5,8.5,11.5,14.5,17.5,20.5, 23.5, 26),line = 0, 
				labels = c("Child \n Order","Rural \n","Mother \n Age","Mother \n Ed","Sex of \n Child","Malaria\n Prevalence","Cook Fuel\n Type","Asset \n Index","Year","Main \n Effect"))
		axis(2, at = seq(-0.02, 0.04, 0.01), labels =seq(-0.02, 0.04, 0.01)*1000, tick = T, las =2,line = -1.5)						
	  mtext(side = 2, text = "Change in IMR (% per 10 ug/m3)",line=1)

	dev.off()							
		
					