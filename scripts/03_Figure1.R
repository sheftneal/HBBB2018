source("scripts/helper_functions.R")
load("data/figure_data/Fig1_data.RData")
load("data/inputs/map_boundaries.Rdata")



pdf("figures/raw/Fig1_raw.pdf",width =16, height = 6, family = "Helvetica", useDingbats = F)

	par(mfrow = c(1,3))
	par(oma = c(1,1,1,1))
	par(mar = c(1,1,1,1))

	#color palette for all panels 
    	#Derived from Paul Tol's palettes designed to be distinct in colour-blind vision
    	#https://personal.sron.nl/~pault/
  	  pal <- colorRampPalette(colors = add.alpha(c("#404096","#63AD99","#BEBC48","#E66B33","#D92120"), 0.5))	


	#panel (a)
	
		#plot map
			plot(africa, xlim = c(-10.5,45.5), ylim = c(-45,35), col = NA, border = NA, bg = NA)
			plot(pm_ave, add=T, col = pal(256),legend=F)
			plot(africa, add=T, border = 'gray80', lwd=  0.1)	
		
		#plot legend
			plotrix::gradient.rect(-15,-39.2,50,-36.7,col=plotrix::smoothColors(pal(5000)), gradient="x", border = NA)
    			text(x = -15 + 16.25*0 + 0.9, y = -41, labels = "0", col = 'gray30',cex = 1.8)
    			text(x = -15 + 16.25*1, y = -41, labels = "25", col = 'gray30',cex = 1.8)
    			text(x = -15 + 16.25*2, y = -41, labels = "50", col = 'gray30',cex = 1.8)
    			text(x = -15 + 16.25*3, y = -41, labels = "75", col = 'gray30',cex = 1.8)
    			text(x = -15 + 16.25*4 -2.5, y = -41, labels = ">100", col = 'gray30',cex = 1.8)	
			text(x = -14, y = -45.5, labels = "Average Annual PM2.5 Concentration (ug/m3)",cex =1.5, pos = 4)
			
		
	#panel (b)
	
		#plot map
  		plot(africa, xlim = c(-10.5,45.5), ylim = c(-45,35), col = NA, border = NA, bg = NA)
  		plot(pm_max, add=T, col = pal(256),legend=F)
  		plot(africa, add=T, border = 'gray80', lwd=  0.1)	

		#plot legend
  		plotrix::gradient.rect(-15,-39.2,50,-36.7,col= plotrix::smoothColors(pal(5000)), gradient="x", border = NA)
      		text(x = -15 + 16.25*0 + 0.9, y = -41, labels = "0", col = 'gray30',cex = 1.8)
      		text(x = -15 + 16.25*1, y = -41, labels = "25", col = 'gray30',cex = 1.8)
      		text(x = -15 + 16.25*2, y = -41, labels = "50", col = 'gray30',cex = 1.8)
      		text(x = -15 + 16.25*3, y = -41, labels = "75", col = 'gray30',cex = 1.8)
      		text(x = -15 + 16.25*4 - 2.5, y = -41, labels = ">100", col = 'gray30',cex = 1.8)	
  		text(x = -16, y = -45.5, labels ="Maximum Annual PM2.5 Concentration (ug/m3)",cex = 1.5, pos = 4)
	
	
	#panel (c)

		#plot map
		plot(africa, xlim = c(-10.5,45.5), ylim = c(-45,35), col = 'gray75', border = NA, bg = NA)
		
		#apply color scheme to data
		classint = classInt::classIntervals(imr_ave$value, style = "fixed", fixedBreaks = c(-1,seq(0,200,0.5),1000))
		col.plot = classInt::findColours(classint, pal(256))

		#replace pixels with population in bottom 5th percentile as gray
		col.plot[(imr_ave$pop<=0.000525055 | is.na(imr_ave$pop)) & imr_ave$country%in%c("AO","ML","NM")] = 'gray75'		

		#plot imr  
		points(imr_ave$x, imr_ave$y, col = col.plot ,pch=15, cex=0.2)
		plot(africa, border = 'white',col =NA,  add=T, lwd=0.1)
		plot(africa[africa@data$ISO2 %in% levels(imr_ave$country),], border= 'gray70', col = NA, add=T, lwd =0.1)

		#plot legend
		plotrix::gradient.rect(-15,-39.2,50,-36.7,col=plotrix::smoothColors(pal(5000)), gradient="x", border = NA)
    		text(x = -15 + 16.25*0 + 0.9, y = -41, labels = "0", col = 'gray30',cex = 1.8)
    		text(x = -15 + 16.25*1, y = -41, labels = "50", col = 'gray30',cex = 1.8)
    		text(x = -15 + 16.25*2, y = -41, labels = "100", col = 'gray30',cex = 1.8)
    		text(x = -15 + 16.25*3, y = -41, labels = "150", col = 'gray30',cex = 1.8)
    		text(x = -15 + 16.25*4 - 2.5, y = -41, labels = ">200", col = 'gray30',cex = 1.8)	
		text(x = -10, y = -45.5, labels = "Infant Mortality Rate (deaths per 1,000 births)",cex =1.5, pos = 4)
	

dev.off()	

	

	