source("scripts/helper_functions.R")
load("data/figure_data/Fig4_data.RData") #objects loaded for plotting defined in "02_paper_calculations.R"
load("data/inputs/map_boundaries.Rdata")



### panel a	###
  
  pdf("figures/raw/Fig4_a_raw.pdf",width =6, height = 6, useDingbats=F)
  	par(oma = c(1,1,1,1))
  	par(mar = c(1,1,1,1))

  			#define color scheme
      	#Derived from Paul Tol's palettes designed to be distinct in colour-blind vision
      	#https://personal.sron.nl/~pault/  			
  	    col.pal <- colorRampPalette(colors = add.alpha(c("#404096","#63AD99","#BEBC48","#E66B33","#D92120"), 0.5))	
  			classint = classInt::classIntervals(cell_estimates$share_attr, style = "fixed", fixedBreaks = c(seq(-0.01,0.45,0.01), 1))
  			col.map = classInt::findColours(classint, col.pal(256))
    		
  			#plot
  			plot(africa, xlim = c(-10.5,45.5), ylim = c(-45,35), col = 'gray60', border = NA, bg = NA)
  			points(cell_estimates$x, cell_estimates$y, col = col.map ,pch=15, cex=0.2)
  	  	plot(africa, border = 'white',col =NA,  add=T, lwd=0.1)
  
  		  #legend
  		  plotrix::gradient.rect(-15,-39.2,50,-36.7,col=plotrix::smoothColors(col.pal(5000)), gradient="x", border = NA)
    		text(x = -15 + 16.25*0 + 2.2 , y = -41, labels = "0%", col = 'gray30',cex = 1.15)
    		text(x = -15 + 16.25*1, y = -41, labels = "10%", col = 'gray30',cex = 1.15)
    		text(x = -15 + 16.25*2, y = -41, labels = "20%", col = 'gray30',cex = 1.15)
    		text(x = -15 + 16.25*3, y = -41, labels = "30%", col = 'gray30',cex = 1.15)
    		text(x = -15 + 16.25*4 -3.5 , y = -41, labels = expression("">="40%"), col = 'gray30',cex = 1.15)	
    		text(x = -5, y = -46, labels = "Infant deaths attributable to PM2.5 (%)",cex = 0.9, pos = 4)
  dev.off()
  	
  	
### panel b	###
	
    	
    pdf("figures/raw/Fig4_b_raw.pdf",width =6, height = 6, useDingbats=F)
    	par(oma = c(1,1,1,1))
    	par(mar = c(4,4,2,0))
    
    	    #color scheme
    	      classint = classInt::classIntervals(country_estimates$share_attr, style = "fixed", fixedBreaks =c(seq(-0.01,0.45,0.01), 1))
    	      col = classInt::findColours(classint, col.pal(256))
    
          #bar plot
    				xloc = barplot( as.numeric(country_estimates$deaths),main = "",axes = F,
    						            names = as.character(country_estimates$country), las=2,ylim = c(0,100000),cex.names=0.75, col = col)
    				
    				axis(2, at = seq(0,100000,20000),labels = seq(0,100000,20000)/1000,tick=F,las=2,cex.axis=1,line = 1)
    				abline(h = seq(0,100000,20000), col = add.alpha('gray50',0.5),lty=3,lwd=0.5)
    				
    				text(trim(format(round(country_estimates$deaths/10,0)*10, big.mark=",")), 
    						x = xloc, y = as.numeric(country_estimates$deaths)+7000,
    						cex=0.75, srt=90)
    				
    				text(x = 30, y = 90000, labels = trim(format(round(country_estimates$deaths/10,0)*10, big.mark=","))[29],cex=0.75, srt=90)
    				mtext(side = 2, text = "Infant Deaths (thousands)",line=3.5)
    				
    dev.off()



### panel c	###

# plot effect sizes versus LiST estimates
#  LiST estimates constructed by Vincent Tanutama 9/20/2017 from the downloadable LiST model
#  Parameter assumptions:  100% coverage in 2016, baseline coverage in 2015, looking at effect on under-1 mortality rate
#  "average" is population-weighted treatment effect for study countries
      
      pdf(file="figures/raw/Fig4_c_raw.pdf",width=7,height=5,useDingbats = F)
      		
      		plot(1,xlim=c(1,nrow(inter_impact)+3.5),ylim=c(0,8),type="n",las=1,ylab="% decline in infant mortality",xlab="",xaxt="n")
      		for (i in 1:nrow(inter_impact)) {
      		  rect(i+0.1,0,i+0.9,inter_impact[i,2],border=NULL,col="lightblue")
      		}
      		i=nrow(inter_impact)+1.5
      		rect(i+0.1,0,i+0.9,effect_gbd,border=NULL,col="gray90")
      		i=nrow(inter_impact)+2.5
      		rect(i+0.1,0,i+0.9,effect_ssa,border=NULL,col='gray30')
      		segments(i+0.5,effect_low_ssa,i+0.5,effect_high_ssa,lwd=1.5)
      		text(1:nrow(inter_impact)+0.5,inter_impact[,2]+1,substr(inter_impact[,1],1,6),cex=0.4)
      		
      		
    dev.off()
      	
      
      






