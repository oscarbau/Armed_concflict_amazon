### Amazon conflict####
library(foreign)

#dn<-read.dbf("C:/ITC/Thesis/gisdata/1km/Variable_sample_10000_amz.dbf")
#dc <-read.dbf("C:/ITC/Thesis/gisdata/1km/Coca_sample.dbf")
#dc<- as.data.frame(dc$coca_rate)
#mines<-read.dbf("C:/ITC/Thesis/gisdata/1km/Dist_mines_sample.dbf")
#mines<- as.data.frame(mines$dist_mines)

#colnames(mines)<-"dist_mines"
#colnames(dc)<-"coca_rate"
#View(mines)
#View(dc)

#View(dn)
#dn2<-dn[,6:17]
#View(dn2)
#dn2<-cbind(dn2,dc,mines)

dn2<-read.dbf("C:/ITC/Thesis/gisdata/1km/Variable_sample_amazon_10k.dbf")
dn2<-dn2[,4:18]
colnames(dn)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                           "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                           "coca_bin_c","coca_rate","dist_mines")
summary(dn2)

#VIFcalc(dn2)
#View(VIF)
#write.csv2(VIF_cr, "C:/ITC/Thesis/gisdata/VIF_cr.csv")


#### subset train and test sets
dn2$random<-runif(1:length(dn2$deforested))
dn2<-dn2[dn2$deforested==1 | (dn2$deforested==0 & dn2$random<0.9917),]
dn2.test<-dn2[dn2$random<0.30,(1:(length(dn2)-1))]
dn2.train<-dn2[dn2$random>=0.30,(1:(length(dn2)-1))]
summary(dn2.train)
write.csv2(dn2.test,"C:/ITC/Thesis/gisdata/Amazon/conflict/amazon_test_conf.csv")
write.csv2(dn2.train,"C:/ITC/Thesis/gisdata/Amazon/conflict/amazon_train_conf.csv")
#dn2.test<-read.csv2("C:/ITC/Thesis/gisdata/Amazon/conflict/amazon_test_conf.csv")
#dn2.train<-read.csv2("C:/ITC/Thesis/gisdata/Amazon/conflict/amazon_train_conf.csv")

##### prepare maps
require(raster)
require(rgdal)

deforestation.rs<-raster("C:/ITC/Thesis/gisdata/1km/deforestation1k.tif")

dist_conflict.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_conflict1k.tif")
gini.rs<-raster("C:/ITC/Thesis/gisdata/1km/Gini1k.tif")
population.rs<-raster("C:/ITC/Thesis/gisdata/1km/pop1k.tif")
dist_roads.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_roads1k.tif")
dist_riv.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_rivers1k.tif")
dist_towns.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_settlements1k.tif")
dist_forest.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_forest1k.tif")
elevation.rs<-raster("C:/ITC/Thesis/gisdata/1km/Elevation1k.tif")
slope.rs<-raster("C:/ITC/Thesis/gisdata/1km/slope1k.tif")
rain.rs<-raster("C:/ITC/Thesis/gisdata/1km/annual_1k_prec_cyl_eq.tif")
Pas.rs<-raster("C:/ITC/Thesis/gisdata/1km/prot_ar1k.tif")
coca_b.rs<-raster("C:/ITC/Thesis/gisdata/1km/coca1k.tif")
dist_mines.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_mines1k.tif")
coca_rate.rs<-raster("C:/ITC/Thesis/gisdata/1km/coca_rate1k.tif")

deforestation.df<-as.data.frame(deforestation.rs)
dist_conflict.df<-as.data.frame(dist_conflict.rs)
gini.df<-as.data.frame(gini.rs)
population.df<-as.data.frame(population.rs)
dist_roads.df<-as.data.frame(dist_roads.rs)
dist_riv.df<-as.data.frame(dist_riv.rs)
dist_towns.df<-as.data.frame(dist_towns.rs)
dist_forest.df<-as.data.frame(dist_forest.rs)
elevation.df<-as.data.frame(elevation.rs)
slope.df<-as.data.frame(slope.rs)
rain.df<-as.data.frame(rain.rs)
Pas.df<-as.data.frame(Pas.rs)
coca_b.df<-as.data.frame(coca_b.rs)
dist_mines.df<-as.data.frame(dist_mines.rs)
coca_rate.df<-as.data.frame(coca_rate.rs)

map.df.cr.all<-data.frame(deforested=deforestation.df,
                          dist_udcp_=dist_conflict.df,
                          gini_avg_l=gini.df,
                          wpop_avg_1=population.df,
                          dist_roads=dist_roads.df,
                          dist_riv_1=dist_riv.df,
                          dist_towns=dist_towns.df,
                          dist_fores=dist_forest.df,
                          SRTM_mean_=elevation.df,
                          slope_mean=slope.df,
                          annual_1k_=rain.df,
                          bin_PAs_am=Pas.df,
                          coca_bin_am=coca_b.df,
                          coca_rate=coca_rate.df,
                          dist_mines=dist_mines.df)
colnames(map.df.cr.all)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                           "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                           "coca_bin_c","coca_rate","dist_mines")


bootstrap_amz <-function(d,bootSize,set)
{
  nv<-length(names(d))
  # print(names)
  models<-as.data.frame(matrix(data=NA,nrow=bootSize,ncol=2*nv))
  names(models)<-c("(Intercept)",names(d)[2:nv],"(Intercept)",names(d)[2:nv])
  selection<-c(1:length(d[,1]))
  r.squared=0
  for(i in 1:bootSize)
  {
    dx<-d[sample(selection,replace=T),]
    # small<-lm(d2[,1]~1)
    model.small <-glm(deforested ~ dist_roads,data=dx,family = "binomial")
    model.large<-glm(deforested ~ .,data=dx,family = "binomial")#lm(d2.train[,1]~ .,data=d2[,2:nv])
    model<-step(model.large,scope=list(lower=model.small,upper=model.large),direction="both")
    
    set$predictions<-predict(model,type="response",newdata=set)
    predictions.matrix<-matrix(set$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
    
    predictions.rs<-raster(predictions.matrix,
                           crs=deforestation.rs@crs, ## the coordinate reference system
                           xmn=deforestation.rs@extent@xmin,## the outer coordinates of the bounding box
                           ymn=deforestation.rs@extent@ymin,
                           xmx=deforestation.rs@extent@xmax,
                           ymx=deforestation.rs@extent@ymax)
    nam <- paste("C:/ITC/Thesis/gisdata/Amazon/Prob", i,".tif", sep = "")
    writeRaster(predictions.rs, nam,overwrite=T)
    # r.squared=r.squared+summary(model)$AIC
    for(j in 1:length(summary(model)$coefficients[,4]))
    {
      for(k in 1:(nv))
      {
        if(names(models)[k]==row.names(summary(model)$coefficients)[j])
        {
          models[i,k]=summary(model)$coefficients[j,4]
          models[i,nv+k]=summary(model)$coefficients[j,1]
        }
      }
    }
    # modelsc$AIC[i]=AIC(model)
    # modelsc$RMSE[i]=sqrt(sum((d2.train[,1]-predict(model))^2))
  }
  # print(paste("Average R-squared is:", r.squared/bootSize))
  write.csv2(models,"C:/ITC/Thesis/gisdata/Amazon/conflict/boot_reg_amz_conf.csv" )
  return(models)
}

Model_Amz<-bootstrap_amz(dn2.train,100,map.df.cr.all)

colnames(Model_Amz)<-c("Constant", "Dist Conflict","Land Inequality", "Population", "Dist roads", "Dist rivers","Dist Towns", 
                       "Dist Forest", "Elevation", "Slope", "Precipitation", "Prot. Areas", "Coca density", "Dist mines",
                       "Constant", "Conflict","Land Inequality", "Population", "Dist roads", "Dist rivers", "Dist Towns", 
                       "Dist Forest", "Elevation", "Slope", "Precipitation", "Prot. Areas", "Coca density", "Dist mines")

Model_Amz[is.na(Model_Amz)] <- -1


plotBootsTrap <-function(models,alpha,alpha1)
{
  dtp<-matrix(data=0,ncol=(length(names(models)))/2,nrow=3)#DataToPlot
  # print(1)
  for(i in 1:((length(names(models)))/2))
  {
    for(j in 1:length(models[,1]))
    {
      if(models[j,i]<=alpha1 & (models[j,i]>=0))
        dtp[1,i]= dtp[1,i]+1
      else if((models[j,i]>alpha1) & (models[j,i]<=alpha))
        dtp[2,i]= dtp[2,i]+1
      else if(models[j,i]>alpha)
        dtp[3,i]= dtp[3,i]+1
    }
  }
  # print(2)
  x<-length(names(models))/2
  dtp<-as.data.frame(dtp)
  colnames(dtp)<-(names(models[,(1:x)]))
  dtp<-dtp[,2:x]
  var_nam<-colnames(models[,c(2:x)])
  tab1<-as.data.frame(t(dtp))
  tab1$tot<- rowSums(tab1)
  rownames(tab1) <- var_nam
  tab1<-tab1[with(tab1, order(tab1[,1],tab1[,2], decreasing = T)),]
  newnam<- row.names(tab1)
  dtp<-as.matrix(t(tab1))
  dtp<-dtp[(1:3),]
  
  lgnd1 <- paste("P <=",alpha1)
  lgnd2<-paste("P > ",alpha1,"& P <=",alpha)
  lgnd3<-paste("P >",alpha)
  lgnd <-c(lgnd1,lgnd2, lgnd3)
  
  barplot(dtp, las=2, ylab= "Number of times included in model",
          #legend.text=c("Significant","Non significant"),
          legend.text=lgnd,
          args.legend = list(x ='right',y="top", bty='n', inset=c(-0.3,0), cex=0.8),
          names.arg=names(dtp))
  # print(3)
  #dtp<-t(dtp)
  #dtp<-as.data.frame(dtp)
  #names(dtp)<-c("sign","n.sign")
  #row.names(dtp)<-names(models)[1:((length(names(models)))/2)]
  #dtp$total<-dtp$sign+dtp$n.sign
  return(dtp)
  
}
## prepare canvas for plot
op<-par(no.readonly=TRUE)
par(mar=c(7,4,3,9))
plot_boot <-plotBootsTrap(Model_Amz,0.05, 0.01)
abline(h=50,col="black", lty=3)
rm(op)

### new model AMAZON excluiding non significant covariates####
model.Amz <-glm(deforested ~  dist_roads + dist_mines + annual_1k_ + wpop_avg_1 + dist_udcp_ + dist_fores +gini_avg_l + dist_towns +
                  SRTM_mean_,
                data=dn2.train,family = "binomial")

amz_mod_conf<-summary(model.Amz)
write.csv2(amz_mod_conf$coefficients,"C:/ITC/Thesis/gisdata/Amazon/Conflict/mod_amz_conf.csv")

########## map amazon conflict ##########


map.df.amz.conf<-map.df.cr.all#[,c(1,2,3,4,5,7,9,11,15)]#6,8,10,12,13,14 out

map.df.amz.conf$predictions<-predict(model.Amz,type="response",newdata=map.df.amz.conf)
predictions.matrix<-matrix(map.df.amz.conf$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
predictions.rs.amz.conf<-raster(predictions.matrix,
                                crs=deforestation.rs@crs, ## the coordinate reference system
                                xmn=deforestation.rs@extent@xmin,## the outer coorcinates of the bounding box
                                ymn=deforestation.rs@extent@ymin,
                                xmx=deforestation.rs@extent@xmax,
                                ymx=deforestation.rs@extent@ymax)
plot(predictions.rs.amz.conf)

writeRaster(predictions.rs.amz.conf, "C:/ITC/Thesis/gisdata/Amazon/Conflict/prob_amz_conf.tif", overwrite = T)




############ frontier zone conflict ############

library(foreign)

# dnzoom<-read.dbf("C:/ITC/Thesis/gisdata/1km/Variable_sample_10000_zoom.dbf")
# dc1 <-read.dbf("C:/ITC/Thesis/gisdata/1km/Coca_sample_zoom.dbf")
# dc1<- as.data.frame(dc1$coca_rate)
# colnames(dc1)<-"coca_rate"
# 
# minesfront<-read.dbf("C:/ITC/Thesis/gisdata/1km/Dist_mines_zoom.dbf")
# minesfront<- as.data.frame(minesfront$dist_mines)
# colnames(minesfront)<-"dist_mines"



# dnzoom1<-dnzoom[,6:17]
# dnzoom1<-cbind(dnzoom1,dc1,minesfront)
#View(dnzoom1)

dnzoom<-read.dbf("C:/ITC/Thesis/gisdata/1km/Variable_sample_frontier_8k.dbf")
dnzoom1<-dnzoom[,4:18]
colnames(dnzoom1)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                "coca_bin_c","coca_rate","dist_mines")
summary(dnzoom1)
hist(dnzoom1$deforested, breaks=2)


#### subset train and test sets
dnzoom1$random<-runif(1:length(dnzoom1$deforested))
dnzoom1.test<-dnzoom1[dnzoom1$random<0.30,(1:(length(dnzoom1)-1))]
dnzoom1.train<-dnzoom1[dnzoom1$random>=0.30,(1:(length(dnzoom1)-1))]
 
write.csv2(dnzoom1.test,"C:/ITC/Thesis/gisdata/frontier/conflict/frontier_test_conf.csv")
write.csv2(dnzoom1.train,"C:/ITC/Thesis/gisdata/frontier/conflict/frontier_train_conf.csv")

#VIFcalc(dnzoom1.test)
#View(VIF)
#write.csv2(VIF, "C:/ITC/Thesis/gisdata/VIF_AMZ.csv")

###### prepare maps fontier zone conflict##########
require(raster)
require(rgdal)

deforestation.rs<-raster("C:/ITC/Thesis/gisdata/1km/deforestation1k.tif")
dist_conflict.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_conflict1k.tif")
gini.rs<-raster("C:/ITC/Thesis/gisdata/1km/Gini1k.tif")
population.rs<-raster("C:/ITC/Thesis/gisdata/1km/pop1k.tif")
dist_roads.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_roads1k.tif")
dist_riv.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_rivers1k.tif")
dist_towns.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_settlements1k.tif")
dist_forest.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_forest1k.tif")
elevation.rs<-raster("C:/ITC/Thesis/gisdata/1km/Elevation1k.tif")
slope.rs<-raster("C:/ITC/Thesis/gisdata/1km/slope1k.tif")
rain.rs<-raster("C:/ITC/Thesis/gisdata/1km/annual_1k_prec_cyl_eq.tif")
Pas.rs<-raster("C:/ITC/Thesis/gisdata/1km/prot_ar1k.tif")
coca_b.rs<-raster("C:/ITC/Thesis/gisdata/1km/coca1k.tif")
dist_mines.rs<-raster("C:/ITC/Thesis/gisdata/1km/dist_mines1k.tif")
coca_rate.rs<-raster("C:/ITC/Thesis/gisdata/1km/coca_rate1k.tif")

deforestation.df<-as.data.frame(deforestation.rs)
dist_conflict.df<-as.data.frame(dist_conflict.rs)
gini.df<-as.data.frame(gini.rs)
population.df<-as.data.frame(population.rs)
dist_roads.df<-as.data.frame(dist_roads.rs)
dist_riv.df<-as.data.frame(dist_riv.rs)
dist_towns.df<-as.data.frame(dist_towns.rs)
dist_forest.df<-as.data.frame(dist_forest.rs)
elevation.df<-as.data.frame(elevation.rs)
slope.df<-as.data.frame(slope.rs)
rain.df<-as.data.frame(rain.rs)
Pas.df<-as.data.frame(Pas.rs)
coca_b.df<-as.data.frame(coca_b.rs)
coca_rate.df<-as.data.frame(coca_rate.rs)
dist_mines.df<-as.data.frame(dist_mines.rs)

map.df.cr.all<-data.frame(deforested=deforestation.df,
                          dist_udcp_=dist_conflict.df,
                          gini_avg_l=gini.df,
                          wpop_avg_1=population.df,
                          dist_roads=dist_roads.df,
                          dist_riv_1=dist_riv.df,
                          dist_towns=dist_towns.df,
                          dist_fores=dist_forest.df,
                          SRTM_mean_=elevation.df,
                          slope_mean=slope.df,
                          annual_1k=rain.df,
                          bin_PAs_am=Pas.df,
                          coca_bin_a=coca_b.df,
                          coca_rate=coca_rate.df,
                          dist_mines=dist_mines.df)

colnames(map.df.cr.all)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                           "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                           "coca_bin_c", "coca_rate", "dist_mines")

#### bootstrap AMAZON#########
bootstrap_front <-function(d,bootSize,set)
{
  nv<-length(names(d))
  # print(names)
  models<-as.data.frame(matrix(data=NA,nrow=bootSize,ncol=2*nv))
  names(models)<-c("(Constant)",names(d)[2:nv],"(Constant)",names(d)[2:nv])
  selection<-c(1:length(d[,1]))
  r.squared=0
  for(i in 1:bootSize)
  {
    dx<-d[sample(selection,replace=T),]
    # small<-lm(d2[,1]~1)
    model.small <-glm(deforested ~ dist_roads,data=dx,family = "binomial")
    model.large<-glm(deforested ~ .,data=dx,family = "binomial")#lm(d2.train[,1]~ .,data=d2[,2:nv])
    model<-step(model.large,scope=list(lower=model.small,upper=model.large),direction="both")
    
    set$predictions<-predict(model,type="response",newdata=set)
    predictions.matrix<-matrix(set$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
    
    predictions.rs<-raster(predictions.matrix,
                           crs=deforestation.rs@crs, ## the coordinate reference system
                           xmn=deforestation.rs@extent@xmin,## the outer coordinates of the bounding box
                           ymn=deforestation.rs@extent@ymin,
                           xmx=deforestation.rs@extent@xmax,
                           ymx=deforestation.rs@extent@ymax)
    nam <- paste("C:/ITC/Thesis/gisdata/Frontier/Prob", i,".tif", sep = "")
    writeRaster(predictions.rs, nam,overwrite=T)
    # r.squared=r.squared+summary(model)$AIC
    for(j in 1:length(summary(model)$coefficients[,4]))
    {
      for(k in 1:(nv))
      {
        if(names(models)[k]==row.names(summary(model)$coefficients)[j])
        {
          models[i,k]=summary(model)$coefficients[j,4]
          models[i,nv+k]=summary(model)$coefficients[j,1]
        }
      }
    }
    # modelsc$AIC[i]=AIC(model)
    # modelsc$RMSE[i]=sqrt(sum((d2.train[,1]-predict(model))^2))
  }
  # print(paste("Average R-squared is:", r.squared/bootSize))
  write.csv2(models,"C:/ITC/Thesis/gisdata/frontier/conflict/boot_reg_front_conf.csv" )
  return(models)
}

boot_Model_front<-bootstrap_front(dnzoom1.train,100,map.df.cr.all)

colnames(boot_Model_front)<-c("Constant", "Dist Conflict","Land inequality", "Population", "Dist roads", "Dist rivers","Dist Towns", 
                              "Dist Forest", "Elevation", "Slope", "Precipitation", "Prot. Areas", "Coca density", "Dist mines",
                              "Constant", "Dist Conflict","Land inequality", "Population", "Dist roads", "Dist rivers","Dist Towns", 
                              "Dist Forest", "Elevation", "Slope", "Precipitation", "Prot. Areas", "Coca density", "Dist mines")

boot_Model_front[is.na(boot_Model_front)] <- -1

plotBootsTrap <-function(models,alpha,alpha1)
{
  dtp<-matrix(data=0,ncol=(length(names(models)))/2,nrow=3)#DataToPlot
  # print(1)
  for(i in 1:((length(names(models)))/2))
  {
    for(j in 1:length(models[,1]))
    {
      if(models[j,i]<=alpha1 & (models[j,i]>=0))
        dtp[1,i]= dtp[1,i]+1
      else if((models[j,i]>alpha1) & (models[j,i]<=alpha))
        dtp[2,i]= dtp[2,i]+1
      else if(models[j,i]>alpha)
        dtp[3,i]= dtp[3,i]+1
    }
  }
  # print(2)
  x<-length(names(models))/2
  dtp<-as.data.frame(dtp)
  colnames(dtp)<-(names(models[,(1:x)]))
  dtp<-dtp[,2:x]
  var_nam<-colnames(models[,c(2:x)])
  tab1<-as.data.frame(t(dtp))
  tab1$tot<- rowSums(tab1)
  rownames(tab1) <- var_nam
  tab1<-tab1[with(tab1, order(tab1[,1],tab1[,2], decreasing = T)),]
  newnam<- row.names(tab1)
  dtp<-as.matrix(t(tab1))
  dtp<-dtp[(1:3),]
  
  lgnd1 <- paste("P <=",alpha1)
  lgnd2<-paste("P > ",alpha1,"& P <=",alpha)
  lgnd3<-paste("P >",alpha)
  lgnd <-c(lgnd1,lgnd2, lgnd3)
  
  barplot(dtp, las=2, ylab= "Number of times included in model",
          #legend.text=c("Significant","Non significant"),
          legend.text=lgnd,
          args.legend = list(x ='right',y="top", bty='n', inset=c(-0.3,0), cex=0.8),
          names.arg=names(dtp))
  # print(3)
  #dtp<-t(dtp)
  #dtp<-as.data.frame(dtp)
  #names(dtp)<-c("sign","n.sign")
  #row.names(dtp)<-names(models)[1:((length(names(models)))/2)]
  #dtp$total<-dtp$sign+dtp$n.sign
  return(dtp)
  
}

op<-par(no.readonly=TRUE)
par(mar=c(7,4,3,9))
plot_boot_front <-plotBootsTrap(boot_Model_front,0.05, 0.01)
abline(h=50,col="black", lty=3)
rm(op)

#### new model frontier excluiding non significant covariates####
model.front <-glm(deforested ~ wpop_avg_1 + dist_roads + dist_riv_1 +  dist_towns +  dist_fores + SRTM_mean_ +
                    annual_1k_ + dist_udcp_ + coca_rate,
                  data=dnzoom1.train,family = "binomial")

front_mod_conf<-summary(model.front)
write.csv2(front_mod_conf$coefficients,"C:/ITC/Thesis/gisdata/Frontier/Conflict/mod_front_conf.csv")

##### map frontier Conflict ####

map.df.front_conf<-map.df.cr.all #[,c(1,2,4,5,6,7,8,9,11,12,14)]# 3,10,13,15

map.df.front_conf$predictions<-predict(model.front,type="response",newdata=map.df.front_conf)
predictions.matrix<-matrix(map.df.front_conf$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
predictions.rs.front.conf<-raster(predictions.matrix,
                                  crs=deforestation.rs@crs, ## the coordinate reference system
                                  xmn=deforestation.rs@extent@xmin,## the outer coorcinates of the bounding box
                                  ymn=deforestation.rs@extent@ymin,
                                  xmx=deforestation.rs@extent@xmax,
                                  ymx=deforestation.rs@extent@ymax)
plot(predictions.rs.front.conf)

writeRaster(predictions.rs.front.conf, "C:/ITC/Thesis/gisdata/Frontier/Conflict/prob_front_conf.tif", overwrite = T)


##### Plot ROC and sens AMZ + FRONT####
require(PresenceAbsence)
test.datan1<-data.frame(ID=1:length(dn2.test$deforested),
                        Observed=dn2.test$deforested,
                        model=predict(model.Amz,type="response",newdata=dn2.test))

op<-par(no.readonly=TRUE)
par(mar=c(4,4,4,4))
par(mfrow = c(1, 2))

auc.roc.plot(test.datan1, color = T, main = "ROC curve Amazon")



#### ROC frontier
test.datazoom1<-data.frame(ID=1:length(dnzoom1.test$deforested),
                           Observed=dnzoom1.test$deforested,
                           model=predict(model.front,type="response",newdata=dnzoom1.test))

auc.roc.plot(test.datazoom1, color = T, main = "ROC curve frontier")


### plot max kappa Amazon

optimal.thresholds(test.datan1)
presence.absence.accuracy(test.datan1, threshold=0.04)

error.threshold.plot(test.datan1,which.model=1,legend.cex=0.8, 
                     color= T, add.opt.legend= T,opt.thresholds = T, plot.it = T,
                     add.legend=T, main = "Amazon")

### plot max kappa Amazon frontier
optimal.thresholds(test.datazoom1)
presence.absence.accuracy(test.datazoom1, threshold=0.12)

error.threshold.plot(test.datazoom1,which.model=1,legend.cex=0.8, 
                     color= T, add.opt.legend= T,opt.thresholds = T, plot.it = T,
                     add.legend=T, main = "Frontier")
par(mfrow = c(1, 1))
rm(op)

############ Non-conflict data preparation#########
#### change conflict dataset to non-conflict
#max_2d<-872880*2
# dist_conflict.df<-as.data.frame((dist_conflict.df+1)/(dist_conflict.df+1))*max_2d
# dist_mines.df<-as.data.frame((dist_mines.df+1)/(dist_mines.df+1))*max_2d
dist_conflict.df<-as.data.frame((dist_conflict.df+1)/(dist_conflict.df+1))*max(dn2.train$dist_udcp_)
dist_mines.df<-as.data.frame((dist_mines.df+1)/(dist_mines.df+1))*max(dn2.train$dist_mines)
coca_b.df<-as.data.frame(coca_b.df*0)
coca_rate.df<-as.data.frame(coca_rate.df*0)

### create dataframe
map.df.non_conf<-data.frame(deforested=deforestation.df,
                            dist_udcp_=dist_conflict.df,
                            gini_avg_l=gini.df,
                            wpop_avg_1=population.df,
                            dist_roads=dist_roads.df,
                            dist_riv_1=dist_riv.df,
                            dist_towns=dist_towns.df,
                            dist_fores=dist_forest.df,
                            SRTM_mean_=elevation.df,
                            slope_mean=slope.df,
                            annual_1k=rain.df,
                            bin_PAs_am=Pas.df,
                            coca_bin_am=coca_b.df,
                            coca_rate=coca_rate.df,
                            dist_mines=dist_mines.df)

check_map.non_conf<-map.df.non_conf
summary(check_map.non_conf)

colnames(map.df.non_conf)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                             "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                             "coca_bin_c","coca_rate","dist_mines")

############ Amazon non conflict #####

map.df.amz.nc<-map.df.non_conf#[,c(1,2,3,4,5,7,8,9,11,15)]#6,10,12,13,14 out
summary(map.df.amz.nc)

map.df.amz.nc$predictions<-predict(model.Amz,type="response",newdata=map.df.amz.nc)

predictions.matrix<-matrix(map.df.amz.nc$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
predictions.rs.amz.nc<-raster(predictions.matrix,
                              crs=deforestation.rs@crs, ## the coordinate reference system
                              xmn=deforestation.rs@extent@xmin,## the outer coorcinates of the bounding box
                              ymn=deforestation.rs@extent@ymin,
                              xmx=deforestation.rs@extent@xmax,
                              ymx=deforestation.rs@extent@ymax)
plot(predictions.rs.amz.nc, main= "non-conflict amazon max")
summary (map.df.amz.nc)
writeRaster(predictions.rs.amz.nc, "C:/ITC/Thesis/gisdata/Amazon/Non_conflict/prob_amz_non_conflict.tif", overwrite=T)


#### modify conflict variables frontier zone####
dist_conflict.df<-as.data.frame((dist_conflict.df+1)/(dist_conflict.df+1))*max(dnzoom1.train$dist_udcp_)
dist_mines.df<-as.data.frame((dist_mines.df+1)/(dist_mines.df+1))*max(dnzoom1.train$dist_mines)
coca_b.df<-as.data.frame(coca_b.df*0)
coca_rate.df<-as.data.frame(coca_rate.df*0)

### create dataframe
map.df.front.non_conf<-data.frame(deforested=deforestation.df,
                            dist_udcp_=dist_conflict.df,
                            gini_avg_l=gini.df,
                            wpop_avg_1=population.df,
                            dist_roads=dist_roads.df,
                            dist_riv_1=dist_riv.df,
                            dist_towns=dist_towns.df,
                            dist_fores=dist_forest.df,
                            SRTM_mean_=elevation.df,
                            slope_mean=slope.df,
                            annual_1k=rain.df,
                            bin_PAs_am=Pas.df,
                            coca_bin_am=coca_b.df,
                            coca_rate=coca_rate.df,
                            dist_mines=dist_mines.df)

check_map.non_conf<-map.df.front.non_conf
summary(check_map.non_conf)

colnames(map.df.front.non_conf)<-c("deforested", "dist_udcp_", "gini_avg_l", "wpop_avg_1", "dist_roads", "dist_riv_1",
                             "dist_towns", "dist_fores", "SRTM_mean_","slope_mean", "annual_1k_", "bin_PAs_am",
                             "coca_bin_c","coca_rate","dist_mines")
############ Non-conflict Frontier #########
map.df.front.nc<-map.df.front.non_conf#[,c(1,2,4,5,6,7,8,9,11,12,14)]# 3,10,13,15

check.front.non_conf<-map.df.front.nc

map.df.front.nc$predictions<-predict(model.front,type="response",newdata=map.df.front.nc)
predictions.matrix<-matrix(map.df.front.nc$predictions,nrow=deforestation.rs@nrows,ncol=deforestation.rs@ncols,byrow=TRUE)
predictions.rs.front.nc<-raster(predictions.matrix,
                                crs=deforestation.rs@crs, ## the coordinate reference system
                                xmn=deforestation.rs@extent@xmin,## the outer coorcinates of the bounding box
                                ymn=deforestation.rs@extent@ymin,
                                xmx=deforestation.rs@extent@xmax,
                                ymx=deforestation.rs@extent@ymax)
plot(predictions.rs.front.nc, main="frontier non-conflict max")

writeRaster(predictions.rs.front.nc, "C:/ITC/Thesis/gisdata/Frontier/Non_conflict/prob_front_non_conf.tif", overwrite=T)



