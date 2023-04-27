
#bring in data
data <- read_rds("DHSdata_AfricanCountries_1960_2015.rds")
data <- filter(data, birth_year >= 1998 & birth_year < 2016)	#subset to years with pm25 data



##### PM 2.5 (including dust and salt)

      #Downloaded Here: http://fizz.phys.dal.ca/~atmos/martin/?page_id=140
      dat.files <- list.files("PM25", pattern = ".nc")	#get file names 
      #and put in order of years since file names change post-2007			
      yr <- as.numeric(substr(unlist(lapply(strsplit(dat.files, "_"),function(x){x[4]})),1,4))			
      dat.files <- dat.files[order(yr)] #sort file names since file naming convention changes post-2007 so not naturally in year order			
      dat.files2 <- dat.files[grep(x = dat.files, pattern = "_NoDust_NoSalt")]
      dat.files <- dat.files[dat.files%in% dat.files2==F]

#define raster brick to store all years
pm <- brick(x = raster(raster("PM25/GlobalGWRcwUni_PM25_GL_199801_199812-RH35_Median.nc")),nl = length(1998:2015))

#load years
for (f in 1:length(dat.files)){
  pm[[f]] <- raster(paste("PM25/",dat.files[f],sep=""))
}			

pmCell <- cellFromXY(pm, data[,c("lon","lat")]) #get pm cells for each DHS location
datpm <- pm[pmCell] #pull pm data
colnames(datpm) <- paste("pm",1998:2015, sep="") #names of obs




  #Define weights
  #weights naming convention -- (month refers to birth month)	
      #pre = in-utero (month - 9):(month)
      #post = 12 months following birth --> (month + 1):(month+12)
      #both = in utero + 12 months following birth --> (month - 9):(month + 12)	
      

      #Define pre weights
        
        #current year weight is just (month)/9 and if born Oct-Dec weight is 1	
        data$current_yr_weight_pre <- data$birth_month/9; 
        data$current_yr_weight_pre[data$current_yr_weight_pre>1]<-1 #dealing with Oct-Dec 
        
        data$last_yr_weight_pre <- (1 - data$current_yr_weight_pre) #last year is everything else for pre weight
        #1998 is first year of pm data so we have to set pre yr weight to 0 for those and put all wt in 1998. Later can drop these obs in utero regs if we want.
        data$last_yr_weight_pre[data$birth_year==1998] <- 0
        data$current_yr_weight_pre[data$birth_year==1998] <- 1
        
        #check that weights add to 1
        if(!all.equal(rep(1, nrow(data)), data$current_yr_weight_pre + data$last_yr_weight_pre)){stop("weights don't add up to 1")}
    
    
      #Define post weights	
          
          #next yr is (month birth)/12. 
          data$next_yr_weight_post <- data$birth_month/12
          #we don't have 2016 pm25 so 2015 births have to have 0 weights for 2016 pm. Can drop all 2015 births and/or can include then check
          data$next_yr_weight_post[data$birth_year == 2015]<-0
          
          #this yr is left over
          data$current_yr_weight_post <- 1-data$next_yr_weight_post
          
          #check that weights add to 1
          if(!all.equal(rep(1, nrow(data)), data$next_yr_weight_post + data$current_yr_weight_post)){stop("weights don't add up to 1")}
    


      #Define both period Weights
      
      data$last_yr_weight_both <- data$last_yr_weight_pre	#same as pre
      data$next_yr_weight_both <- data$next_yr_weight_post #same as post
      data$current_yr_weight_both <- 1 #(month - 9):(month + 12) will always cover 12 month in birth year
      #normalize the both-period weights to 1
      wt_sum <- data$last_yr_weight_both + data$current_yr_weight_both + data$next_yr_weight_both
      data$last_yr_weight_both <- data$last_yr_weight_both/wt_sum
      data$current_yr_weight_both <- data$current_yr_weight_both/wt_sum
      data$next_yr_weight_both <- data$next_yr_weight_both/wt_sum
      
      #check weights add to 1
      if(!all.equal(rep(1, nrow(data)), 
                    as.numeric(apply(data[,c("last_yr_weight_both","current_yr_weight_both","next_yr_weight_both")],1,sum)))){stop("weights don't add up to 1")}
      


# Create df to pull data from that has birth info, weights, and pm25 data in same df so we can apply() and pull all info from a single row
    
    data_pull <- data.frame(datpm, data) #datpm is pm25, data is dhs 
    row <- data_pull[runif(n = 1, min = 1, max = 500000),] #arbitrary row for testing function


      
      #Funtion 1: pre period
      
      get_pm_pre <- function(row){
        
        #gives us column number of birth year since data starts in 1998	
        index <- as.numeric(row["birth_year"]) - 1997 
        
        #calculate weighted average of pm values in pre period
        if(index>1){
          out <- as.numeric(row[index])*as.numeric(row["current_yr_weight_pre"]) + as.numeric(row[index - 1])*as.numeric(row["last_yr_weight_pre"])
        }else{
          out <- as.numeric(row[index])*as.numeric(row["current_yr_weight_pre"]) 
        }
        #return pm
        return(out)		
        
      }
      
      #execute function
      data$pm25_pre <- apply(data_pull, 1, get_pm_pre)



    #Funtion 2: post period
    
    get_pm_post <- function(row){ #have to handle 2015 births since post-period will go into 2016 for which we dont have pm data
      
      #gives us column number of birth year since data starts in 1998
      index <- as.numeric(row["birth_year"]) - 1997 
      
      if(index<18){
        out <- as.numeric(row[index])*as.numeric(row["current_yr_weight_post"]) + as.numeric(row[index + 1])*as.numeric(row["next_yr_weight_post"])
      }else{
        out <- as.numeric(row[index]) 
      }
    }
    
    #execute function
    data$pm25_post <- apply(data_pull, 1, get_pm_post)


    #Funtion 3: both periods
    get_pm_both <- function(row){ #have to handle 2015 births since post-period will go into 2016 for which we dont have pm data
      
      #gives us column number of birth year since data starts in 1998
      index <- as.numeric(row["birth_year"]) - 1997 
      
      if(index<18){
        out <- as.numeric(row[index])*as.numeric(row["current_yr_weight_both"]) + as.numeric(row[index - 1])*as.numeric(row["last_yr_weight_both"])+ as.numeric(row[index + 1])*as.numeric(row["next_yr_weight_both"])
      }else{
        out <- as.numeric(row[index]) 
        
      }}
    
    #execute function
    data$pm25_both <- apply(data_pull, 1, get_pm_both)




##############################################################################################################################
##############################################################################################################################

# Now do same thing with No dust No Salt version of PM data


pm2 <- brick(x = raster(raster("PM25/GlobalGWRcwUni_PM25_GL_200201_200212-RH35_Median.nc")),nl = length(1998:2015))

#load years
for (f in 1:length(dat.files2)){
  pm2[[f]] <- raster(paste("PM25/",dat.files2[f],sep=""))
}			

  pmCell2 <- cellFromXY(pm2, data[,c("lon","lat")]) #get pm cells for each DHS location (should be same but just to be safe redo)
  datpm2 <- pm2[pmCell2] #pull pm data
  colnames(datpm2) <- paste("pm", 1998:2015, sep="") #names of obs



#create df to pull data from
  data_pull <- data.frame(datpm2, data) #datpm is pm25, data is dhs 

#execute functions
    data$pm25_pre_anth <- apply(data_pull, 1, get_pm_pre)
    data$pm25_post_anth <- apply(data_pull, 1, get_pm_post)
    
    
    #non anthro
    data$pm25_post_nonanth <- data$pm25_post - data$pm25_post_anth
    data$pm25_pre_nonanth <- data$pm25_pre - data$pm25_pre_anth


#write out data    
write.rds(data, file = "RegressionData_pollution_AfricanCountries_1998_2015.rds")










