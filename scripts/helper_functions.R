library(raster)
library(classInt)
library(fields)
library(plotrix)
library(tidyverse)
library(lfe)
library(splines)
library(multcomp)
library(maptools)


#define functions for plotting data histograms at the bottom of dose-response plots:

  #step 1: create data frame with hist heights:  
  get_hst_obj <- function(values, min, max, width, cutoff = 1,type = "count"){
              x_hist_bar <- seq(min,max,width) 
              hist_ht <- data.frame(x_hist_bar- width/2 , x_hist_bar +width/2, NA)       
              for(i in 1:nrow(hist_ht)){hist_ht[i,3]<- sum(values> hist_ht[i,1] & values <= hist_ht[i,2], na.rm = T)}
              hist_ht[1,3] <- hist_ht[1,3] + sum(values < hist_ht[1,1], na.rm = T)
              hist_ht[nrow(hist_ht),3] <- hist_ht[nrow(hist_ht),3] +  sum(values > hist_ht[nrow(hist_ht),2], na.rm = T)
              if(type == "share"){hist_ht[,3]<-hist_ht[,3]/sum(hist_ht[,3],na.rm = T)}
              names(hist_ht)=c("left","right","count")
              hist_ht <- subset(hist_ht, right <= quantile(values, cutoff,na.rm = T))
              return(hist_ht)
    }
    
        #step 2: plot histogram at bottom of figure
        plotHist <- function(hst_obj, col, alpha, bottom, height, border.col = col){
              
              rect(xleft = hst_obj[,1], xright =  hst_obj[,2], 
                   ybottom =bottom, ytop = (hst_obj[,3]/max(hst_obj[,3]))*height + bottom, 
                   col = add.alpha(col, alpha), border = border.col) 		
          
        }

    
    #plotting error bars function
    error.bar <- function(x, y, upper, lower=upper, length=0.04, col = 'gray30'){
      arrows(x,upper, x, lower, angle=90, code=3, length=length, col = col)
    }		

      
      #add transparency to any color	
      add.alpha <- function(col, alpha=1){
        if(missing(col))
          stop("Please provide a vector of colours.")
        apply(sapply(col, col2rgb)/255, 2, 
              function(x) 
                rgb(x[1], x[2], x[3], alpha=alpha))  
      }	
      



#Routine for predict.felm that we use for generating confidence intervals from felm() regressions [code developed by Kendon Bell (https://github.com/kendonB)]
        
        predict.felm <- function(object, newdata, se.fit = FALSE,
                                 interval = "none",
                                 level = 0.95){
          if(missing(newdata)){
            stop("predict.felm requires newdata and predicts for all group effects = 0.")
          }
          
          tt <- terms(object)
          Terms <- delete.response(tt)
          attr(Terms, "intercept") <- 0
          
          m.mat <- model.matrix(Terms, data = newdata)
          m.coef <- as.numeric(object$coef)
          fit <- as.vector(m.mat %*% object$coef)
          fit <- data.frame(fit = fit)
          
          if(se.fit | interval != "none"){
            if(!is.null(object$clustervcv)){
              vcov_mat <- object$clustervcv
            } else if (!is.null(object$robustvcv)) {
              vcov_mat <- object$robustvcv
            } else if (!is.null(object$vcv)){
              vcov_mat <- object$vcv
            } else {
              stop("No vcv attached to felm object.")
            }
            se.fit_mat <- sqrt(diag(m.mat %*% vcov_mat %*% t(m.mat)))
          }
          if(interval == "confidence"){
            t_val <- qt((1 - level) / 2 + level, df = object$df.residual)
            fit$lwr <- fit$fit - t_val * se.fit_mat
            fit$upr <- fit$fit + t_val * se.fit_mat
          } else if (interval == "prediction"){
            stop("interval = \"prediction\" not yet implemented")
          }
          if(se.fit){
            return(list(fit=fit, se.fit=se.fit_mat))
          } else {
            return(fit)
          }
        }
        


#function for re-centering response functions
    center <- function(column, x.range, center.x, center.y){column - column[which(x.range==center.x)] + center.y}





#function for sampling groups in tidyverse to be used for bootstrap block sampling
#https://github.com/tidyverse/dplyr/issues/361

sample_n_groups = function(tbl, size, replace = FALSE, weight = NULL) {
  # regroup when done
  grps = tbl %>% groups %>% lapply(as.character) %>% unlist
  # check length of groups non-zero
  keep = tbl %>% summarise() %>% ungroup() %>% sample_n(size, replace, weight)
  # keep only selected groups, regroup because joins change count.
  # regrouping may be unnecessary but joins do something funky to grouping variable
  tbl %>% right_join(keep, by=grps) %>% group_by_(.dots = grps)
}

