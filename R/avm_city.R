#' @title AVM modeling of each city
#'
#' @description AVM modeling of each city
#' @param city_code City's abbr
#' @param proptype Property type of house area, 11---residence, and else like offical area
#' @param model.list all models contain: ols, rlm, svm, rft, xgb
#' @param host Host of the server, character
#' @param port Port of the server, numeric
#' @param user User of the server, character
#' @param password Password of the server, character
#' @param dbname Database name of city, character
#' @param modelpath1 The path storing models and tables with ha/poi information
#' @param modelpath2 The path storing our final models, extending community models, e.g. community + city models
#' @export
#' @return write tif files
avm_city <- function(city_code,proptype,model.list,host,port,user,password,dbname,modelpath1,modelpath2){

############################################################## run example ##############################################################
# avm_city(city_code='qd',proptype='11',model.list=list(ols=T,rlm=T,svm=T,rft=T,xgb=T),                                                 #
# host="192.168.3.142",port=3306,user="edi", password="cityhouse",dbname="cityre_qingdao",modelpath1 = ".",modelpath2 = "./models.avm") #
#########################################################################################################################################
  cat("*******************************************************\n")
  # link the database
  cat("Linking the database ... ...")
  con<-dbconnect(host=host, port=port,user=user, password=password, dbname=dbname)
  cat("Succeed!\n")

  # read data from database
  cat("Reading data from databse ... ...")
  tabln.vec<-loadData(con=con,month_offset = 0)
  cat("Succeed!\n")
  
  # delete the data out of a city
  tabln.vec$ha_info.sp<-remove.spatial.outlier(object = tabln.vec$ha_info.sp,city_code = city_code)
  
  # poi raster
  cat("Rasterizing POI ... ...")
  poi.data.list<-getpoi.ras(con=con,tabln.vec = tabln.vec)
  cat("Succeed!\n")
  
  # put ha_info to ha_info.sp
  tabln.vec$ha_info.sp<-poi.data.list$ha_info.sp
  
  # prepare the data set for modeling
  cat("Preparing the data set for modeling ... ...")
  data.sets.all<-avm.data.train(tabln.vec,proptype = proptype, modelpath1 = modelpath1, city_code = city_code)
  cat("Succeed!\n")
  # put data.sets.all to tabln.vec
  tabln.vec$data.sets.all<-data.sets.all
  
  # split the training data and testing data. Maybe the testing data will not be used
  data.sets<-data.split(data =data.sets.all,train.rate = 1)
  
  # get the dependent variable names of train data
  independentX <- formula_rvars(data.sets$train)
  
  # definate the dependent variable names
  dependentY <- c("K0","Kbyear","Kheight","Ktime","Kfloor","KbrArea","Kstru","Kface_through","Kface_sun","Kdeco_if","Kdeco_good","Kprop_rt")
  
  # ensure modelpath2 exist
  if (!file.exists(modelpath2)) dir.create(modelpath2)
  
  # save the ha_info and poi_info
  cat("Saving coarse data of information and POI ... ...")
  saveRDS(tabln.vec,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_","tabha.rds"))
  saveRDS(poi.data.list,paste0(modelpath2,"/",city_code,"_",format(Sys.Date(),"%Y%m"),"_","poiha.rds"))
  cat("Succeed!\n")
  
  # definite the train models
                                          #############################
                                          # train.models[[1]] --- ols #
                                          # train.models[[2]] --- rlm #
                                          # train.models[[3]] --- svm #
                                          # train.models[[4]] --- rft #
                                          # train.models[[5]] --- xgb #
                                          #############################
  train.models <- vector(mode = 'list',length = length(model.list[model.list==TRUE]))
  names(train.models)<-names(model.list[model.list==TRUE])#c('ols','rlm','svm','rft','xgb')
  
  # calculate all the models in model.list
  cat("Modelling ... ...")
  for (i in 1:length(train.models)){
    train.models[[i]] <- vector(mode = 'list',length = length(dependentY))
    # create the model
    train.models[[i]] <- lapply(1:length(dependentY), function(j) {avm.fit(data.train=data.sets$train,dependentY=dependentY[j],independentX,method=names(model.list)[i])})
    names(train.models[[i]]) <- dependentY
  }
  cat("Succeed!\n")
  
  #######################################################################################################################################
  # prediction example 1: calculate with a method ******************************************************************
  # suppressWarnings(extd.comavm::avm.pred.data(train.models=train.models[[1]],
  #                                             tabln.vec=tabln.vec,
  #                                             poi.data.list=poi.data.list,
  #                                             method='ols',
  #                                             proptype='11',
  #                                             x=120.1776,
  #                                             y=35.94424,
  #                                             bldg_code='11',
  #                                             byear=5,height=18,floor=3,brArea=90,stru=TRUE,face_through=TRUE,
  #                                             face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,
  #                                             byearmin=3,byearmax=6,heightmin=7,heightmax=33))
  ########################################################################################################################################
  # prediction example 2: calculate with all methods ***************************************************************
  # sapply(1:5, function(i) {suppressWarnings(extd.comavm::avm.pred.data(train.models=train.models[[i]],
  #                                                                      tabln.vec=tabln.vec,
  #                                                                      poi.data.list=poi.data.list,
  #                                                                      method=names(train.models[i]),
  #                                                                      proptype='11',
  #                                                                      x=120.1776,
  #                                                                      y=35.94424,
  #                                                                      bldg_code='11',
  #                                                                      byear=5,height=18,floor=3,brArea=90,stru=TRUE,face_through=TRUE, 
  #                                                                      face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,
  #                                                                      byearmin=3,byearmax=6,heightmin=7,heightmax=33))})
  ########################################################################################################################################
  cat("Testing ... ...\n")
  test <- data.sets.all[which(!duplicated(data.sets.all$ha_code)),]
  
  # test by bldg_code ='11'
  cat("\t multi-story building: code = 11 ... ...")
  test1 <- subset(test,bc11==1)
  if(nrow(test1>0)){
    test1$K0 <- exp(test1$K0)
    test1$ols<-sapply(1:nrow(test1), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$ols,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='ols',
                                 proptype='11',x=test1$x[i],y=test1$y[i],bldg_code='11',byear=10,height=6,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=4,heightmax=8))})
    test1$ols <- abs(test1$ols-test1$K0)/test1$K0
    
    test1$rlm<-sapply(1:nrow(test1), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rlm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rlm',
                                   proptype='11',x=test1$x[i],y=test1$y[i],bldg_code='11',byear=10,height=6,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=4,heightmax=8))})
    test1$rlm <- abs(test1$rlm-test1$K0)/test1$K0
    
    test1$svm<-sapply(1:nrow(test1), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$svm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='svm',
                                   proptype='11',x=test1$x[i],y=test1$y[i],bldg_code='11',byear=10,height=6,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=4,heightmax=8))})
    test1$svm <- abs(test1$svm-test1$K0)/test1$K0
    
    test1$rft<-sapply(1:nrow(test1), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rft,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rft',
                                 proptype='11',x=test1$x[i],y=test1$y[i],bldg_code='11',byear=10,height=6,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=4,heightmax=8))})
    test1$rft <- abs(test1$rft-test1$K0)/test1$K0
    
    test1$xgb<-sapply(1:nrow(test1), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$xgb,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='xgb',
                                 proptype='11',x=test1$x[i],y=test1$y[i],bldg_code='11',byear=10,height=6,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=4,heightmax=8))})
    test1$xgb <- abs(test1$xgb-test1$K0)/test1$K0
  }
  cat("succeed!\n")
  
  # test by bldg_code ='12'
  cat("\t small high-rise building: code = 12 ... ...")
  test2 <- subset(test,bc12==1)
  if(nrow(test2>0)){
    test2$K0 <- exp(test2$K0)
    test2$ols<-sapply(1:nrow(test2), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$ols,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='ols',
                                 proptype='11',x=test2$x[i],y=test2$y[i],bldg_code='12',byear=10,height=11,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=9,heightmax=13))})
    test2$ols <- abs(test2$ols-test2$K0)/test2$K0
    
    test2$rlm<-sapply(1:nrow(test2), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rlm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rlm',
                                 proptype='11',x=test2$x[i],y=test2$y[i],bldg_code='12',byear=10,height=11,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=9,heightmax=13))})
    test2$rlm <- abs(test2$rlm-test2$K0)/test2$K0
    
    test2$svm<-sapply(1:nrow(test2), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$svm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='svm',
                                 proptype='11',x=test2$x[i],y=test2$y[i],bldg_code='12',byear=10,height=11,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=9,heightmax=13))})
    test2$svm <- abs(test2$svm-test2$K0)/test2$K0
    
    test2$rft<-sapply(1:nrow(test2), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rft,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rft',
                                 proptype='11',x=test2$x[i],y=test2$y[i],bldg_code='12',byear=10,height=11,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=9,heightmax=13))})
    test2$rft <- abs(test2$rft-test2$K0)/test2$K0
    
    test2$xgb<-sapply(1:nrow(test2), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$xgb,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='xgb',
                                 proptype='11',x=test2$x[i],y=test2$y[i],bldg_code='12',byear=10,height=11,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=9,heightmax=13))})
    test2$xgb <- abs(test2$xgb-test2$K0)/test2$K0
  }
  cat("succeed!\n")
  
  # test by bldg_code ='13'
  cat("\t high-rise building: code = 13 ... ...")
  test3 <- subset(test,bc13==1)
  if(nrow(test3>0)){
    test3$K0 <- exp(test3$K0)
    test3$ols<-sapply(1:nrow(test3), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$ols,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='ols',
                                 proptype='11',x=test3$x[i],y=test3$y[i],bldg_code='13',byear=10,height=20,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=14,heightmax=30))})
    test3$ols <- abs(test3$ols-test3$K0)/test3$K0
    
    test3$rlm<-sapply(1:nrow(test3), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rlm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rlm',
                                 proptype='11',x=test3$x[i],y=test3$y[i],bldg_code='13',byear=10,height=20,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=14,heightmax=30))})
    test3$rlm <- abs(test3$rlm-test3$K0)/test3$K0
    
    test3$svm<-sapply(1:nrow(test3), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$svm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='svm',
                                 proptype='11',x=test3$x[i],y=test3$y[i],bldg_code='13',byear=10,height=20,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=14,heightmax=30))})
    test3$svm <- abs(test3$svm-test3$K0)/test3$K0
    
    test3$rft<-sapply(1:nrow(test3), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rft,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rft',
                                 proptype='11',x=test3$x[i],y=test3$y[i],bldg_code='13',byear=10,height=20,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=14,heightmax=30))})
    test3$rft <- abs(test3$rft-test3$K0)/test3$K0
    
    test3$xgb<-sapply(1:nrow(test3), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$xgb,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='xgb',
                                 proptype='11',x=test3$x[i],y=test3$y[i],bldg_code='13',byear=10,height=20,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=14,heightmax=30))})
    test3$xgb <- abs(test3$xgb-test3$K0)/test3$K0
  }
  cat("succeed!\n")
  
  # test by bldg_code ='21'
  cat("\t cottage: code = 21 ... ...")
  test4 <- subset(test,bc21==1)
  if(nrow(test4>0)){
    test4$K0 <- exp(test4$K0)
    test4$ols<-sapply(1:nrow(test4), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$ols,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='ols',
                                 proptype='11',x=test4$x[i],y=test4$y[i],bldg_code='21',byear=10,height=2,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=1,heightmax=3))})
    test4$ols <- abs(test4$ols-test4$K0)/test4$K0
    
    test4$rlm<-sapply(1:nrow(test4), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rlm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rlm',
                                 proptype='11',x=test4$x[i],y=test4$y[i],bldg_code='21',byear=10,height=2,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=1,heightmax=3))})
    test4$rlm <- abs(test4$rlm-test4$K0)/test4$K0
    
    test4$svm<-sapply(1:nrow(test4), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$svm,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='svm',
                                 proptype='11',x=test4$x[i],y=test4$y[i],bldg_code='21',byear=10,height=2,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=1,heightmax=3))})
    test4$svm <- abs(test4$svm-test4$K0)/test4$K0
    
    test4$rft<-sapply(1:nrow(test4), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$rft,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='rft',
                                 proptype='11',x=test4$x[i],y=test4$y[i],bldg_code='21',byear=10,height=2,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=1,heightmax=3))})
    test4$rft <- abs(test4$rft-test4$K0)/test4$K0
    
    test4$xgb<-sapply(1:nrow(test4), function(i){suppressWarnings(
      extd.comavm::avm.pred.data(train.model=train.models$xgb,tabln.vec=tabln.vec,poi.data.list=poi.data.list,method='xgb',
                                 proptype='11',x=test4$x[i],y=test4$y[i],bldg_code='21',byear=10,height=2,floor=1,brArea=90,
                                 stru=TRUE,face_through=TRUE,face_sun=TRUE,deco_if=TRUE,deco_good=TRUE,prop_rt=TRUE,byearmin=0,
                                 byearmax=25,heightmin=1,heightmax=3))})
    test4$xgb <- abs(test4$xgb-test4$K0)/test4$K0
  }
  cat("succeed!\n")
  cat("\ttesting Succeed!\n")
  
  testall <- rbind(test1,test2,test3,test4)
  temptest <- subset(testall,select=c(ols,rlm,svm,rft,xgb))
  testall$model<-sapply(1:nrow(temptest),function(i){names(temptest)[which(temptest[i,]==min(temptest[i,]))]})
  
  # put the optimum method of each train-data-point into models
  train.models$method <- subset(testall,select=c(ha_code,x,y,model))
  
  # save the total model
  cat("Saving the final model ... ...")
  saveRDS(train.models,paste0(modelpath2,"/",city_code,"_coeffs_",proptype,"_",format(Sys.Date(),"%Y%m")))
  cat("Succeed!\n")
  cat("*******************************************************\n")
  
  # close all databases
  killcons <- killDbConnections()
  
  cat("*******************************************************\n")
  cat("The End!")
}

