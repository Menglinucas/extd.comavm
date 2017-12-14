#' joining ha and price data
#'
#' This function can join price and ha info
#' @param tabln.vec basic data provided by \code{\link{loadData}}
#' @param proptype type code of properties.The defalut is 11 indiciting housing
#' @return data including ha info and ha price
ppsale<-function(tabln.vec=NULL,proptype='11',modelpath1=NULL,city_code=NULL)
{
  if(is.null(tabln.vec)){
    stop('must provide a tabln.vec!!!')
  }
  library(dplyr)
  # saleprice of property "11" (residence area)
  price_sale<-tabln.vec$ha_price%>%filter(proptype==proptype)
  # saleprice = 0 and Inf, not meaningful
  price_sale$saleprice[which(price_sale$saleprice<=0 | price_sale$saleprice>1000000)] <- NA
  # # mean saleprice of ha
  # price_sale <- price_sale[!duplicated(price_sale),] %>% group_by(ha_code) %>% summarise(saleprice=mean(saleprice,na.rm=T))
  # add pricesale to ha_info, note: mybe exist duplicated ha_code, that's not a wrong
  ppi_sale<-merge(price_sale,tabln.vec$ha_info.sp,by="ha_code",all.y=T)
  # ppi_sale<-ppi_sale%>%
  #   # mutate(year=floor(ymid/12),
  #   # month=ifelse(!ymid%%12,12,ymid%%12))%>%
  #   group_by(ha_code,x,y,name,ha_cl_code,ha_cl_name,dist_code,
  #            dist_name,buildyear,bldg_code) %>%
  #   summarise(saleprice=mean(saleprice,na.rm=T),
  #             volume_rate=mean(volume_rate,na.rm=T),greening_rate=mean(greening_rate,na.rm=T),
  #             edu.poi.sp_dens=mean(edu.poi.sp_dens,na.rm=T),hosi.poi.sp_dens=mean(hosi.poi.sp_dens,na.rm=T),
  #             trans.poi.sp_dens=mean(trans.poi.sp_dens,na.rm=T),busi.poi.sp_dens=mean(busi.poi.sp_dens,na.rm=T),
  #             poi.diversity.pts=mean(poi.diversity.pts,na.rm=T),
  #             edu.poi.sp_mindist=mean(edu.poi.sp_mindist,na.rm=T),hosi.poi.sp_mindist=mean(hosi.poi.sp_mindist,na.rm=T),
  #             trans.poi.sp_mindist=mean(trans.poi.sp_mindist,na.rm=T),busi.poi.sp_mindist=mean(busi.poi.sp_mindist,na.rm=T))%>%
  #   as.data.frame()
  ppi_sale[ppi_sale=='NaN'] <- NA
  # substitute the price by Community avm values, should ensure that comavm model contains all bldg_code of a community
  ppi_sale <- subsp(ppi_sale,modelpath1,city_code)
  return(ppi_sale)
}

# substitue saleprice of tabln$vec$ha_price by community avm
subsp <- function(sp,modelpath1,city_code){
  # library(rjson)
  filenames <- list.files(paste0(modelpath1,"/",city_code))
  filepos <- paste0(modelpath1,"/",city_code,"/",filenames)
  # number of files
  nha <- length(filenames)
  # create a dataframe to store all ha_code+saleprice of a city
  comsp <- data.frame("ha_code"=rep(NA,nha*4),"bldg_code"=rep(NA,nha*4),
                      "K0"=rep(0,nha*4),"Kbyear"=rep(0,nha*4),"Kheight"=rep(0,nha*4),"Ktime"=rep(0,nha*4),
                      "Kfloor"=rep(0,nha*4),"KbrArea"=rep(0,nha*4),"Kstru"=rep(0,nha*4),"Kface_through"=rep(0,nha*4),
                      "Kface_sun"=rep(0,nha*4),"Kdeco_if"=rep(0,nha*4),"Kdeco_good"=rep(0,nha*4),"Kprop_rt"=rep(0,nha*4))
  # extract the ha_code from filesnames, put in comsp@ha_code
  comsp$ha_code <- sapply(sapply(filenames,unlist(strsplit),split="[_]"),function(x) x[1])
  # extract the bldg_code from json, put in comsp@bldg_code
  comsp$bldg_code[1:nha] <- rep("11",nha)
  comsp$bldg_code[(nha+1):(2*nha)] <- rep("12",nha)
  comsp$bldg_code[(2*nha+1):(3*nha)] <- rep("13",nha)
  comsp$bldg_code[(3*nha+1):(4*nha)] <- rep("21",nha)
  # extract the coefficients from json
  features <- sapply(filepos,jsp,bldg_code='11')
  comsp[1:nha,"K0"] <- features[1,1:nha]
  comsp[1:nha,"Kbyear"] <- features[2,1:nha]
  comsp[1:nha,"Kheight"] <- features[3,1:nha]
  comsp[1:nha,"Ktime"] <- features[4,1:nha]
  comsp[1:nha,"Kfloor"] <- features[5,1:nha]
  comsp[1:nha,"KbrArea"] <- features[6,1:nha]
  comsp[1:nha,"Kstru"] <- features[7,1:nha]
  comsp[1:nha,"Kface_through"] <- features[8,1:nha]
  comsp[1:nha,"Kface_sun"] <- features[9,1:nha]
  comsp[1:nha,"Kdeco_if"] <- features[10,1:nha]
  comsp[1:nha,"Kdeco_good"] <- features[11,1:nha]
  comsp[1:nha,"Kprop_rt"] <- features[12,1:nha]
  
  features <- sapply(filepos,jsp,bldg_code='12')
  comsp[(nha+1):(2*nha),"K0"] <- features[1,1:nha]
  comsp[(nha+1):(2*nha),"Kbyear"] <- features[2,1:nha]
  comsp[(nha+1):(2*nha),"Kheight"] <- features[3,1:nha]
  comsp[(nha+1):(2*nha),"Ktime"] <- features[4,1:nha]
  comsp[(nha+1):(2*nha),"Kfloor"] <- features[5,1:nha]
  comsp[(nha+1):(2*nha),"KbrArea"] <- features[6,1:nha]
  comsp[(nha+1):(2*nha),"Kstru"] <- features[7,1:nha]
  comsp[(nha+1):(2*nha),"Kface_through"] <- features[8,1:nha]
  comsp[(nha+1):(2*nha),"Kface_sun"] <- features[9,1:nha]
  comsp[(nha+1):(2*nha),"Kdeco_if"] <- features[10,1:nha]
  comsp[(nha+1):(2*nha),"Kdeco_good"] <- features[11,1:nha]
  comsp[(nha+1):(2*nha),"Kprop_rt"] <- features[12,1:nha]
  
  features <- sapply(filepos,jsp,bldg_code='13')
  comsp[(2*nha+1):(3*nha),"K0"] <- features[1,1:nha]
  comsp[(2*nha+1):(3*nha),"Kbyear"] <- features[2,1:nha]
  comsp[(2*nha+1):(3*nha),"Kheight"] <- features[3,1:nha]
  comsp[(2*nha+1):(3*nha),"Ktime"] <- features[4,1:nha]
  comsp[(2*nha+1):(3*nha),"Kfloor"] <- features[5,1:nha]
  comsp[(2*nha+1):(3*nha),"KbrArea"] <- features[6,1:nha]
  comsp[(2*nha+1):(3*nha),"Kstru"] <- features[7,1:nha]
  comsp[(2*nha+1):(3*nha),"Kface_through"] <- features[8,1:nha]
  comsp[(2*nha+1):(3*nha),"Kface_sun"] <- features[9,1:nha]
  comsp[(2*nha+1):(3*nha),"Kdeco_if"] <- features[10,1:nha]
  comsp[(2*nha+1):(3*nha),"Kdeco_good"] <- features[11,1:nha]
  comsp[(2*nha+1):(3*nha),"Kprop_rt"] <- features[12,1:nha]
  
  features <- sapply(filepos,jsp,bldg_code='21')
  comsp[(3*nha+1):(4*nha),"K0"] <- features[1,1:nha]
  comsp[(3*nha+1):(4*nha),"Kbyear"] <- features[2,1:nha]
  comsp[(3*nha+1):(4*nha),"Kheight"] <- features[3,1:nha]
  comsp[(3*nha+1):(4*nha),"Ktime"] <- features[4,1:nha]
  comsp[(3*nha+1):(4*nha),"Kfloor"] <- features[5,1:nha]
  comsp[(3*nha+1):(4*nha),"KbrArea"] <- features[6,1:nha]
  comsp[(3*nha+1):(4*nha),"Kstru"] <- features[7,1:nha]
  comsp[(3*nha+1):(4*nha),"Kface_through"] <- features[8,1:nha]
  comsp[(3*nha+1):(4*nha),"Kface_sun"] <- features[9,1:nha]
  comsp[(3*nha+1):(4*nha),"Kdeco_if"] <- features[10,1:nha]
  comsp[(3*nha+1):(4*nha),"Kdeco_good"] <- features[11,1:nha]
  comsp[(3*nha+1):(4*nha),"Kprop_rt"] <- features[12,1:nha]
  
  # saleprice = 0 and Inf, not meaningful
  comsp$K0[which(comsp$K0<=6 | comsp$K0>14)] <- NA
  # delete no price data
  comsp <- na.omit(comsp)
  # delete the duplicated row
  comsp <- comsp[!duplicated(subset(comsp,select=c(ha_code,bldg_code))),]
  # dataframe: ha_code, bldg_code, ..., bc11, bc12, bc13, bc21
  comsp$bc11 <- 0
  comsp$bc12 <- 0
  comsp$bc13 <- 0
  comsp$bc21 <- 0
  idy <- sapply(1:nrow(comsp), 
                function(i,df=comsp){
                  id <- which(substr(names(df),3,4)==df[i,'bldg_code'])}
  )
  for (i in 1:nrow(comsp)){
    comsp[i,idy[i]] <- 1
  }
  # the coefficients sould not be too large
  comsp[(which(abs(comsp[4])>1)),4] <- sign(comsp[(which(abs(comsp[4])>1)),4])
  comsp[(which(abs(comsp[5])>1)),5] <- sign(comsp[(which(abs(comsp[5])>1)),5])
  comsp[(which(abs(comsp[6])>1)),6] <- sign(comsp[(which(abs(comsp[6])>1)),6])
  comsp[(which(abs(comsp[7])>1)),7] <- sign(comsp[(which(abs(comsp[7])>1)),7])
  comsp[(which(abs(comsp[8])>1)),8] <- sign(comsp[(which(abs(comsp[8])>1)),8])
  comsp[(which(abs(comsp[9])>1)),9] <- sign(comsp[(which(abs(comsp[9])>1)),9])
  comsp[(which(abs(comsp[10])>1)),10] <- sign(comsp[(which(abs(comsp[10])>1)),10])
  comsp[(which(abs(comsp[11])>1)),11] <- sign(comsp[(which(abs(comsp[11])>1)),11])
  comsp[(which(abs(comsp[12])>1)),12] <- sign(comsp[(which(abs(comsp[12])>1)),12])
  comsp[(which(abs(comsp[13])>1)),13] <- sign(comsp[(which(abs(comsp[13])>1)),13])
  comsp[(which(abs(comsp[14])>1)),14] <- sign(comsp[(which(abs(comsp[14])>1)),14])
  # price_sale <-  group_by(comsp,ha_code) %>% summarise(saleprice=mean(saleprice,na.rm=T)) %>% as.data.frame()
  # bldg <- table(comsp[1:2]) %>% unclass() %>% as.data.frame()
  # names(bldg) <- paste0("bc",names(bldg))
  # bldg$ha_code <- row.names(bldg)
  # row.names(bldg) <- c(1:nrow(bldg))
  # price_sale <- merge(bldg,price_sale,by="ha_code")
  # replace saleprice
  
  sp <- subset(sp,select=-c(saleprice,bldg_code))
  result <- merge(comsp,sp,by='ha_code')
  # the last checking
  result <- result[!duplicated(result) & !is.na(result$K0),]
  return(result)
}

# read (building year, height) and all coefficients from json file based on bldg_code
jsp <- function(filepos,bldg_code){
  # all the variables
  # standard variables including time(1:5), floor, brArea, stru, face_through, face_sun, deco_if, deco_good and prop_rt
  coeffs <- c("price","buildyear","height",
              "time","floor","brArea","stru","face_through","face_sun","deco_if","deco_good","prop_rt")
  # the values of coefficients
  coeffsValue <- c(rep(0,length(coeffs)))
  
  # read Json
  temp <- fromJSON(file = filepos)
  # if the json file is null
  if (length(temp)!=0){
    # if existing the bldg_code in the json file
    if (bldg_code %in% names(temp[[1]])){
      bldgfeatures <- temp[[1]][[which(names(temp[[1]])==bldg_code)]]
      features <- bldgfeatures$listFeature
      # update time of the model
      t1 <- as.Date(bldgfeatures$modelUpdateDate)
      # the first day of this month
      t2 <- lubridate::floor_date(Sys.Date(),unit='month')
      
      # extract the coefficients of standard variables
      featurenames <- c()
      for (i in 1:length(features))
      {
        featurenames[i] <- features[[i]]$featureName
      }
      
      # select the time segment (0-?)
      idt <- which(featurenames=='time')
      if (length(idt) > 0){
        for (i in 1:length(idt))
        {
          if (features[[idt[i]]]$min<2) {  #the minimum time value is less than 2, maybe 0 or 1 in json file
            idt_select <- idt[i]
            coeffsValue[4] <- features[[idt_select]]$beta
          } 
        }
      }
      if (coeffsValue[4]!=0){
        stdtime <- as.numeric(t1-t2)/features[[idt_select]]$max
        coeffsValue[4] <- coeffsValue[4] * stdtime
      }
      
      # select the floor segment (1-?)
      idflr <- which(featurenames=='floor')
      if (length(idflr) > 0){
        for (i in 1:length(idflr))
        {
          if (features[[idflr[i]]]$min==1) {  #the minimum floor value is 1
            idflr_select <- idflr[i]
            coeffsValue[5] <- features[[idflr_select]]$beta
          } 
        }
      }
      # stdflr == 0
      
      # select the area segment (?<90<?)
      idarea <- which(featurenames=='brArea')
      if (length(idarea) > 0){
        for (i in 1:length(idarea))
        {
          if (features[[idarea[i]]]$min<90 & features[[idarea[i]]]$max>90) {  # contain area = 90 m^2
            idarea_select <- idarea[i]
            coeffsValue[6] <- features[[idarea_select]]$beta
          } 
        }
      }
      # stdarea == log(90)/log(500)
      
      # # the maximum and minimum value of building year
      # idbuildyear <- which(featurenames=='buildyear')  # must be a 1 dim
      # if (length(idbuildyear)==1){
      #   buildyear_max <- features[[idbuildyear]]$max
      #   buildyear_min <- features[[idbuildyear]]$min
      #   stdbuildyear <- (buildyear-buildyear_min)/(buildyear_max-buildyear_min)
      # }else{stdbuildyear <- 0}
      
      # # the maximum and minimum value of height
      # idheight <- which(featurenames=='height')  # must be a 1 dim
      # if (length(idheight)==1){
      #   height_max <- features[[idheight]]$max
      #   height_min <- features[[idheight]]$min
      #   stdheight <- (height-height_min)/(height_max-height_min)
      # }else{stdheight <- 0}
      
      # else coefficients extraction
      idf <- which(featurenames!='time' & featurenames!='floor' & featurenames!='brArea')
      featurenames <- featurenames[idf]
      if (length(featurenames)>0){
        id <- sapply(1:length(featurenames),function(i) which(coeffs==featurenames[i]))
        coeffsValue[id] <- sapply(1:length(featurenames),function(i) features[[idf[i]]]$beta)
      }
      
      return(coeffsValue)
    }else{return(coeffsValue)}
  }else{return(coeffsValue)}
}
