source("scripts/helper_functions.R") 
load("data/figure_data/FigED6_data.RData")


pdf("figures/raw/FigED6_raw.pdf",width = 7, height = 7, useDingbats = F)	
			par(oma = c(1,1,0,0))
			par(mfrow = c(3,3))
			par(mar = c(4,4,4,0))

	for(y in 1:9){
		
			plot(yrs_all[[y]], treat[[y]][,"est"],
			pch=15, cex=2, col = NA,ylim = c(0.9, 1.3),
			ylab ="", xlab = "",bty = "n",axes=F,xlim =c(2000, 2014))
			
			segments(y0 = 1, y1 = 1, x0 = 2000, x1 = 2014, col = 'red3',lty=1,lwd=1)
			segments(x0 = yrs_all[[y]] ,  y1 = treat[[y]][,"upper"], y0 = treat[[y]][,"lower"], lwd = 2)
			points(yrs_all[[y]],  treat[[y]][,"est"], pch=16, cex=2)

			axis(2 ,tick=F,las=2,line = 0, cex.axis =1.1)
			axis(1, at = 2001:2013, las=2,line=-1.5,tick=F,cex.axis = 1)
	

		
	mtext(side = 3, text = paste(yrs_all[[y]][1], "-",yrs_all[[y]][length(yrs_all[[y]])], sep="" ), cex = 1.25)

	lines(yrs_all[[y]], (regs[y,1]+regs[y,2]*yrs_all[[y]]) , col = add.alpha('gray40', 0.5),lwd=1.5)	

	
	if(y %in% c(1,4,7)){	mtext(side = 2, text = "Relative Risk (per 10 ug/m3)",line=3.5,cex = 0.8)}
	if(y %in% 7:9){mtext(side = 1, text = "Year",cex = .9, line=3.5)}
	
	}
	
			
	dev.off()
		
		

				