#Code to plot the differences on travel time budget between the survey data and the model estimates


#-------loading libraries in R----------------------------
library("MASS", lib.loc="C:/Program Files/R/R-3.0.3/library")
library("ggplot2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("plyr", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("leaps", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("gvlma", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("reshape2", lib.loc="C:/Users/AnaTsui/Documents/r/win-library/3.0")
library("survival", lib.loc="C:/Program Files/R/R-3.0.3/library")

#--------------------------------------------------------
#------------Individual Plots-----------------------------
#--------------------------------------------------------

#-------hbschool-------------------------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSCHOOL_v4.csv") 
data_base<-subset(data_base,TT_HBSCHOOL>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,"TT_HBOTHER" ,  "TT_HBSHOP" ,"TT_HBWORK","TT_NHBOTHER", "TT_NHBWORK" ,"TT_TOTAL"  ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2" ))

names(bbdd_)[47]<-paste("Model")
names(bbdd_)[48]<-paste("TT_SCHOOL")

dif1<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SCHOOL,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for HBE")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1500)
dif1

#-------------hbshop--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSHOP_v4.csv") 
data_base<-subset(data_base,TT_HBSHOP>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,"TT_HBOTHER" ,  "TT_HBSCHOOL" ,"TT_HBWORK","TT_NHBOTHER", "TT_NHBWORK" ,"TT_TOTAL"  ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2","HH_income_inv" ))

names(bbdd_)[48]<-paste("Model")
names(bbdd_)[49]<-paste("TT_SHOP")

dif2<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for HBS")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1200)
dif2

#-------------hbother--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBOTHER4.csv") 
data_base<-subset(data_base,TT_HBOTHER>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,  "TT_HBSCHOOL" ,"TT_HBSHOP" ,"TT_HBWORK","TT_NHBOTHER", "TT_NHBWORK" ,"TT_TOTAL"  ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2" ))

names(bbdd_)[47]<-paste("Model")
names(bbdd_)[48]<-paste("TT_SHOP")

dif3<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for HBO")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1200)
dif3

#-------------nhbother--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBOTHER4.csv") 
data_base<-subset(data_base,TT_NHBOTHER>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,  "TT_HBSCHOOL" ,"TT_HBSHOP" ,"TT_HBWORK","TT_NHBOTHER", "TT_NHBWORK" ,"TT_TOTAL"  ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2","HH_income_inv" ))

names(bbdd_)[48]<-paste("Model")
names(bbdd_)[49]<-paste("TT_SHOP")

dif4<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for NHBO")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1200)
dif4

#-------------nhbwork--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBWORK_2.csv") 
data_base<-subset(data_base,TT_NHBWORK>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,  "TT_HBOTHER", "TT_HBSCHOOL" ,"TT_HBSHOP" ,"TT_HBWORK","TT_NHBOTHER", "TT_TOTAL"  ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2"))

names(bbdd_)[47]<-paste("Model")
names(bbdd_)[48]<-paste("TT_SHOP")

dif5<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for NHBW")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1200)
dif5


#-------------hbwork--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBWORK_v4.csv") 
data_base<-subset(data_base,TT_HBWORK>0)
bbdd_<-melt(data_base,id=c("X" ,            "HouseholdID" ,  "TT_HBOTHER"  ,  "TT_HBSCHOOL"  , "TT_HBSHOP" ,    "TT_NHBOTHER"  ,"TT_NHBWORK"  ,  "TT_TOTAL"   ,   "T_HBOTHER"     ,"T_HBSCHOOL"  ,  "T_HBSHOP"   ,   "T_HBWORK" ,     "T_NHBOTHER"   , "T_NHBWORK"  ,   "T_TOTAL"   ,    "HH_size",       "Females"     ,  "Children"    ,  "Young"   ,      "Retired"    , "HH_workers" ,   "HH_students"   ,"HH_cars"     ,  "HH_licensed",   "HH_area2"  ,    "HH_area3",      "HH_income2"   , "HH_income3"   , "HH_income4"   , "HH_income5" ,   "HH_income6" ,   "HH_income7"   , "HH_income8"   , "HH_income9" , "HH_income10"  , "HH_income11"  , "HH_income12"  , "HBOTHER"  ,     "HBSCHOOL" ,     "HBSHOP",       "HBWORK"       , "NHBOTHER"    ,  "NHBWORK"     ,  "TOTAL"    ,     "HH_income" ,    "HH_income_2" ,   "HH_income_inv"))

names(bbdd_)[48]<-paste("Model")
names(bbdd_)[49]<-paste("TT_SHOP")

dif6<-ggplot(bbdd_,aes(x=TT_TOTAL,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Travel time for HBW")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)+xlim(0,1200)
dif6

#--------------------------------------------------------
#------------Multiplot-----------------------------
#--------------------------------------------------------

#--------call the multiplot function---------------
multiplot(dif2, dif3, dif1, dif5, dif4,dif6,cols=2)


#--------------------------------------------------------
#------------Individual plot Total TTB---------------------------
#--------------------------------------------------------

#-------------TOTAL TRAVEL TIME BUDGET--------------------
data_base<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_TOTAL_v2.csv") 
data_base<-subset(data_base,TT_TOTAL>0)
bbdd_<-melt(data_base,id=c("X","HouseholdID" ,  "TT_HBOTHER", "TT_HBSCHOOL" ,"TT_HBSHOP" ,"TT_HBWORK","TT_NHBOTHER", "TT_NHBWORK" ,  "T_HBOTHER"  , "T_HBSCHOOL" , "T_HBSHOP"   , "T_HBWORK"  ,  "T_NHBOTHER" , "T_NHBWORK" ,"T_TOTAL","HH_size"  ,   "Females" ,    "Children"  ,  "Young"      , "Retired"    , "HH_workers" , "HH_students", "HH_cars" ,"HH_licensed" ,"HH_area2"   , "HH_area3" ,   "HH_income2" , "HH_income3" , "HH_income4" , "HH_income5", "HH_income6" ,"HH_income7" , "HH_income8",  "HH_income9" , "HH_income10" ,"HH_income11" ,"HH_income12" ,"HBOTHER"  ,   "HBSCHOOL"   ,"HBSHOP"    ,  "HBWORK"   ,   "NHBOTHER"  ,  "NHBWORK"   ,  "TOTAL"    ,   "HH_income" ,  "HH_income_2"))

names(bbdd_)[47]<-paste("Model")
names(bbdd_)[48]<-paste("TT_SHOP")

dif7<-ggplot(bbdd_,aes(x=HH_size,y=TT_SHOP,colour=Model))+geom_point(size=1.5)+facet_wrap(~HH_area2)+scale_colour_manual(name="Source",values=c("darkslateblue", "darkorange"),labels=c("Observed", "Model"))+xlab("Total travel time")+ylab("Total travel time")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=5),ncol=2))+theme(plot.title = element_text(lineheight=.8, face="bold"))+ylim(0,500)
dif7

