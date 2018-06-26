source("scripts/helper_functions.R")
load("data/figure_data/Fig3_data.RData") #gbd RR curve as well as histgorams for gbd and ssa
load("data/RR_curve_HBBB2018.RData") #RR curve from our study


  #organize RR curve into confidence interval polygon and response line
  cix_ssa <- c(RR_curve$RRx[1], RR_curve$RRx, RR_curve$RRx[length(RR_curve$RRx):1])
  ciy_ssa <- c(RR_curve$RRlb90[1], RR_curve$RRub90, RR_curve$RRlb90[length(RR_curve$RRlb90):1]) #5-95th CI
  x_ssa <- RR_curve$RRx
  y_ssa <- RR_curve$RRy

  
  
   
  pdf(file = "figures/raw/Fig3_raw.pdf",width = 8, height= 6)	
  
          #initialize plot
          par(mar=c(3,4,1,4),las=1)
          plot(1,1,type="l",lwd=3,lty=1,col=NA,xlim=c(0,71),ylim=c(0.5,3),xlab="",ylab="",yaxt="n", axes=F)
          segments(x0 = 0, x1 = 73, y0=seq(1,5.5,.25),col=add.alpha('gray70', 0.5),lwd=1,lty=2)	
        
          
          #plot gbd RR curve
          lines(c(0,x_gbd),c(1,y_gbd),lty=1,lwd=2,col="red3")
        	polygon(cix_gbd, ciy_gbd, col = add.alpha('red3', 0.25), border = NA)
         
         #plot our curve for subsaharan africa
        	polygon(cix_ssa, ciy_ssa, col = add.alpha('dodgerblue', 0.25), border = NA)
         	lines(x_ssa, y_ssa, col = 'navyblue', lwd = 2)
          	lines(c(0, 3), c(1,1), col = 'navyblue', lwd = 2)
        
        #axis and labels
          axis(side=2,at=seq(1,3,.25),tick = F)
          axis(side=4,at=2.5*c(0, 0.05, 0.1, 0.15) + 0.55  ,labels=c("0","5","10","15"),tick = T)
          axis(1, tick = F, at = seq(0,70,10),line=-2)
          mtext("Relative risk", side=2,las=0,adj=0.75,padj=-5)
          mtext("Exposed Pop.",side=4,las=0,adj=0.05,padj=4)
          mtext(side = 1, text = "PM2.5 concentration (ug/m3)",line=1)
        
        #histograms below 
         rect(xleft = hist_ssa$pm - 0.5, xright = hist_ssa$pm + 0.5, ybottom = 0.55, ytop = 0.55 +(hist_ssa$ht)*2.5, col = add.alpha('dodgerblue',0.5), border = add.alpha('white', .1))
         rect(xleft = hist_gbd$pm - 0.5, xright = hist_gbd$pm + 0.5, ybottom = 0.55, ytop = 0.55+(hist_gbd$ht)*2.5, col= add.alpha('red3',0.25), border = add.alpha('white', 0.1))
         
        #curve labels
         text(x = 60,y = 1.4, labels = "GBD" )
         text(x = 53,y = 1.95, labels = "SSA, this study" )
         
         
         
   
  dev.off()
  

  