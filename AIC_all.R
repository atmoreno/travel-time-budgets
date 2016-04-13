
#Calculate travel time budget 

#-------loading libraries in R----------------------------
library("MASS", lib.loc="C:/Program Files/R/R-3.0.3/library")
library("ggplot2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("plyr", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("leaps", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("gvlma", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("reshape2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("survival", lib.loc="C:/Program Files/R/R-3.0.3/library")

#-------------------------------------------------------
#------------HOME-BASED EDUCATION TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_base$HH_size_L<-as.factor(data_base$HH_size_L)
  data_SCHOOL<-subset(data_base,TT_HBSCHOOL>0)
  data_SCHOOL$HH_income_inv<-1/(data_SCHOOL$HH_income)^0.5
  
  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_HBSCHOOL, HBSCHOOL) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_SCHOOL,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_HBSCHOOL, HBSCHOOL) ~   HH_size2 + HH_size3 +  HH_size4 + HH_size51 + Children + Young + HH_licensed + HH_income_inv + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL , data = data_SCHOOL, dist = "weibull")
  summary(out.weib)
  
  data_SCHOOL$fit<-predict(out.weib)
  cor(data_SCHOOL$fit,data_SCHOOL$TT_HBSCHOOL)
  extractAIC(out.weib)
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSCHOOL_v5.csv")
  
  #-----------------graphs----------------------------------
  data_SCHOOL$HH_size_L<-as.factor(data_SCHOOL$HH_size_L)
  d1<-ggplot(data_SCHOOL,aes(x=TT_HBSCHOOL,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for HBE (min)")+ylab("Predicted travel time for HBE (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d1
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/1.jpeg")
  d1
  dev.off()

#-------------------------------------------------------
#------------HOME-BASED SHOP TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_shop<-subset(data_base,TT_HBSHOP>0)
  data_shop$HH_income_inv<-1/(data_shop$HH_income)^0.5

  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_HBSHOP, HBSHOP) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_shop,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(formula = Surv(TT_HBSHOP, HBSHOP) ~ HH_size2 + HH_size3 + HH_size4 + HH_size51 + Females + Children + HH_licensed + HH_income_inv + HH_area2 + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL, data = data_shop, dist = "weibull")
  summary(out.weib)
  
  data_shop$fit<-predict(out.weib)
  cor(data_shop$fit,data_shop$TT_HBSHOP)
  extractAIC(out.weib)
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSHOP_v5.csv")
  
  #-----------------graphs----------------------------------
  data_shop$HH_size_L<-as.factor(data_shop$HH_size_L)
  d2<-ggplot(data_shop,aes(x=TT_HBSHOP,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for HBS (min)")+ylab("Predicted travel time for HBS (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d2
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/2.jpeg")
  d2
  dev.off()
  
#-------------------------------------------------------
#------------HOME-BASED OTHER TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_base$HH_size_L<-as.factor(data_base$HH_size_L)
  data_other<-subset(data_base,TT_HBOTHER>0)
  data_other$HH_income_inv<-1/(data_other$HH_income)^0.5
  
  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_HBOTHER, HBOTHER) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_other,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_HBOTHER, HBOTHER) ~ HH_size2 + HH_size3 + HH_size4 + HH_size51 + Females + Children + Young + HH_income_inv + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL + T_NHBWORK + T_NHBOTHER, data = data_other, dist = "weibull")  summary(out.weib)
    
  data_other$fit<-predict(out.weib)
  cor(data_other$fit,data_other$TT_HBOTHER)
  extractAIC(out.weib)  
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBOTHER5.csv")
  
  #-----------------graphs----------------------------------
 data_other$HH_size_L<-as.factor(data_other$HH_size_L)
  d3<-ggplot(data_other,aes(x=TT_HBOTHER,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for HBO (min)")+ylab("Predicted travel time for HBO (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d3
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/3.jpeg")
  d3
  dev.off()
  
#-------------------------------------------------------
#------------NON HOME-BASED OTHER TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_base$HH_size_L<-as.factor(data_base$HH_size_L)
  data_nother<-subset(data_base,TT_NHBOTHER>0)
  data_nother$HH_income_inv<-1/(data_nother$HH_income)^0.5
  
  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_NHBOTHER, NHBOTHER) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_nother,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_NHBOTHER, NHBOTHER) ~  HH_size2 + HH_size3 + HH_size4 + Children + Young + HH_workers + HH_cars + HH_income_inv + HH_area2 + HH_area3 + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL + T_NHBWORK + T_NHBOTHER, data = data_nother, dist = "weibull")
      
  data_nother$fit<-predict(out.weib)
  cor(data_nother$fit,data_nother$TT_NHBOTHER)
  extractAIC(out.weib)
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBOTHER5.csv")
  
  #-----------------graphs----------------------------------
  d4<-ggplot(data_nother,aes(x=TT_NHBOTHER,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for NHBO (min)")+ylab("Predicted travel time for NHBO (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d4
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/4.jpeg")
  d4
  dev.off()

#-------------------------------------------------------
#------------NON HOME-BASED WORK TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_base$HH_size_L<-as.factor(data_base$HH_size_L)
  data_nwork<-subset(data_base,TT_NHBWORK>0)
  data_nwork$HH_income_inv<-1/(data_nwork$HH_income)^0.5
  
  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_NHBWORK, NHBWORK) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_nwork,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_NHBWORK, NHBWORK) ~ HH_size3 + Young + Retired + HH_workers + HH_students + HH_cars + HH_licensed + HH_area2 + HH_area3 + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL + T_NHBWORK + T_NHBOTHER, data = data_nwork, dist = "weibull")
  
  data_nwork$fit<-predict(out.weib)
  cor(data_nwork$fit,data_nwork$TT_NHBWORK)
  extractAIC(out.weib)
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBWORK_5.csv")

  #-----------------graphs----------------------------------
  data_nwork$HH_size_L<-as.factor(data_nwork$HH_size_L)
  d5<-ggplot(data_nwork,aes(x=TT_NHBWORK,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for NHBW (min)")+ylab("Predicted travel time for NHBW (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d5
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/5.jpeg")
  d5
  dev.off()

#-------------------------------------------------------
#------------HOME-BASED WORK TRIP PURPOSE--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_base$HH_size_L<-as.factor(data_base$HH_size_L)
  data_work<-subset(data_base,TT_HBWORK>0)
  data_work$HH_income_inv<-1/(data_work$HH_income)^0.5

  
  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_HBWORK, HBWORK) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_work,dist='weibull'))  
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_HBWORK, HBWORK) ~  HH_size2 + HH_size3 + HH_size4 + HH_size51 + Retired + HH_workers + HH_cars + HH_licensed + HH_income_inv + HH_area2 + HH_area3 + T_HBWORK + T_HBSHOP + T_HBOTHER +  T_NHBWORK + T_NHBOTHER, data = data_work, dist = "weibull")
  
  data_work$fit<-predict(out.weib)
  cor(data_work$fit,data_work$TT_HBWORK)
  extractAIC(out.weib)
  summary(out.weib)
  
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBWORK_v5.csv")

  #-----------------graphs----------------------------------
  data_work$HH_size_L<-as.factor(data_work$HH_size_L)
  d6<-ggplot(data_work,aes(x=TT_HBWORK,y=fit,colour=HH_size_L))+geom_point(size=2)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed travel time for HBW (min)")+ylab("Predicted travel time for HBW (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+ylim(0,500)+ geom_abline(intercept = 0, slope = 1)
  d6
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/6.jpeg")
  d6
  dev.off()
  
#-------------------------------------------------------
#------------ALL TRIP PURPOSES--------------------------
#-------------------------------------------------------

  #-------read the data-------------------------------------
  data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/DDBB_HH1.csv") 
  data_TOTAL<-subset(data_base,TT_TOTAL>0) #subset to consider only the households that have travelled at least once
  data_TOTAL$HH_income_inv<-1/(data_TOTAL$HH_income)^0.5 #the best fit to describe continuously the level of income was the inverse of the square root (^0.5)

  #-------------------using step AIC-------------------------
  #call to estimate total travel time depending on the independent variables. Stepwise analysis based on AIC
  stepAIC(survreg(Surv(TT_TOTAL, TOTAL) ~ HH_size2+HH_size3+HH_size4+HH_size51+Females+Children+Young+Retired+HH_workers+HH_students+HH_cars+HH_licensed+HH_income_inv+HH_area2+HH_area3+T_HBWORK+T_HBSHOP+T_HBOTHER+T_HBSCHOOL+T_NHBWORK+T_NHBOTHER,data = data_TOTAL,dist='weibull'))
  
  #Call for the best model that was obtained on the previous call
  out.weib<-survreg(Surv(TT_TOTAL, TOTAL) ~ HH_size2 + HH_size3 + HH_size4 + HH_size51 + Young + Retired + HH_workers + HH_students + HH_cars + HH_licensed + HH_income_inv + HH_area2 + HH_area3 + T_HBWORK + T_HBSHOP + T_HBOTHER + T_HBSCHOOL + T_NHBWORK + T_NHBOTHER, data = data_TOTAL, dist = "weibull")
  summary(out.weib)
  
  data_TOTAL$fit<-predict(out.weib)
  cor(data_TOTAL$fit,data_TOTAL$TT_TOTAL)
  extractAIC(out.weib)
  summary(out.weib)
  
  #write the predicted values for each one of the households
  write.csv(data_shop,file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_TOTAL_v5.csv")
  
  #-----------------graphs----------------------------------
  #graphs of the total travel time depending on the household size
  data_TOTAL$HH_size_L<-as.factor(data_TOTAL$HH_size_L)
  d7<-ggplot(data_TOTAL,aes(x=TT_TOTAL,y=fit,colour=HH_size_L))+geom_point(size=1.5)+scale_colour_manual(name="Household size",values=c("#D7191C", "orange", "yellow" ,"#ABDDA4" ,"#2B83BA"),labels=c("1", "2","3","4","5+"))+xlab("Observed total travel time (min)")+ylab("Predicted total travel time (min)")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,1500)+ylim(0,1500)+ geom_abline(intercept = 0, slope = 1)+facet_wrap(~HH_size_L)
  d7
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/7.jpeg")
  d7
  dev.off()

