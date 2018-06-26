source("scripts/helper_functions.R")

share_pm <- tibble::tibble(share = as.numeric(readr::read_rds("data/figure_data/figED7_data.rds"))) %>%
			        mutate(pm = c(2,2.4,3:5, 5.8, 7:10)) 


black <- 'black'
gray <- add.alpha('gray85', .75)


pdf(file = "figures/raw/FigED7_raw.pdf",useDingbats = F)			

		plot(share_pm$pm, share_pm$share, ylim = c(0, 0.25),pch = 16, axes = F, cex=1.5,
					col = as.character(c(rep(black, 2), rep(gray, 3), black, rep(gray, 3), black)),
					xlab = "Counterfactual PM2.5 Concentration", ylab = "Share of infant deaths attributable to PM2.5")
		lines(share_pm$pm, share_pm$share, lwd =0.5)
		
		segments(x0 = 2, x1 = 10, y0 = seq(0,.25,.05), col = 'gray80', lwd = .3)	
		
	axis(1,at = 2:10, tick = T)
	axis(2, tick = F, at = seq(0,.25,.05),labels = seq(0,.25,.05)*100,las= 2)
		
					
dev.off()