
#Code to plot the cumulative density function of estimated travel time and survey travel time

#---------HB Education----------------------
  data_SCHOOL<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSCHOOL_v5.csv")
  bbdd_<-melt(data_SCHOOL,id=c("HouseholdID","TT_HBOTHER",    "TT_HBSHOP"    ,  "TT_HBWORK"     , "TT_NHBOTHER"  ,  "TT_NHBWORK"    ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q1<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for HBE (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)
  q1
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q1.jpeg")
  q1
  dev.off()

#---------HB SHOP----------------------
  data_shop<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBSHOP_v5.csv")
  bbdd_<-melt(data_shop,id=c("HouseholdID","TT_HBOTHER",    "TT_HBSCHOOL"   ,  "TT_HBWORK"     , "TT_NHBOTHER"  ,  "TT_NHBWORK"    ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q2<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for HBS (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)
  q2
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q2.jpeg")
  q2
  dev.off()

#---------HB oTHER----------------------
  data_other<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBOTHER5.csv")
  bbdd_<-melt(data_other,id=c("HouseholdID",  "TT_HBSCHOOL" , "TT_HBSHOP"   ,  "TT_HBWORK"     , "TT_NHBOTHER"  ,  "TT_NHBWORK"    ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q3<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for HBO (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)
  q3
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q3.jpeg")
  q3
  dev.off()

#---------NHB WORK----------------------
  data_nwork<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBWORK_5.csv")
  bbdd_<-melt(data_nwork,id=c("HouseholdID", "TT_HBOTHER", "TT_HBSCHOOL" , "TT_HBSHOP"   ,  "TT_HBWORK"     , "TT_NHBOTHER"  ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv","p_NHBWORK" ))
  
  q5<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for NHBW (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)
  q5
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q5.jpeg")
  q5
  dev.off()

#---------NHB OTHER----------------------
  data_nother<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_NHBOTHER5.csv")
  bbdd_<-melt(data_nother,id=c("HouseholdID", "TT_HBOTHER",  "TT_HBSCHOOL" , "TT_HBSHOP"   ,  "TT_HBWORK" ,     "TT_NHBWORK"    ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q4<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for NHBO (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)+geom_point(data=data_nother,aes(x=TT_NHBOTHER,y=perc),size=2)
  q4
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q4.jpeg")
  q4
  dev.off()

#---------HB WORK----------------------
  data_work<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_HBWORK_v5.csv")
  bbdd_<-melt(data_work,id=c("HouseholdID", "TT_HBOTHER",  "TT_HBSCHOOL" , "TT_HBSHOP"   ,     "TT_NHBWORK"    ,"TT_NHBOTHER"  ,"TT_TOTAL"     ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,      "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q6<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Travel time for NHBW (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,500)
  q6
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q6.jpeg")
  q6
  dev.off()

#---------total TTB----------------------
  data_TOTAL<-read.csv("C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/DDBB_HH_TOTAL_v5.csv")
  bbdd_<-melt(data_TOTAL,id=c("HouseholdID","TT_HBOTHER",   "TT_HBSCHOOL", "TT_HBSHOP"    ,  "TT_HBWORK"     , "TT_NHBOTHER"  ,  "TT_NHBWORK"      ,  "T_HBOTHER"     , "T_HBSCHOOL"    , "T_HBSHOP"  ,     "T_HBWORK",  "T_NHBOTHER"   ,  "T_NHBWORK"     , "T_TOTAL"    ,    "HH_size"  ,      "Females"     ,   "Children"     ,  "Young"    ,      "Retired"      ,  "HH_workers"  , "HH_students"  ,  "HH_cars"  ,      "HH_licensed"  ,  "HH_area2"  ,"HH_area3" ,     "HH_income2"    , "HH_income3"  , "HH_income4"   ,  "HH_income5"  ,   "HH_income6"     ,"HH_income7"  ,   "HH_income8"    , "HH_income9"    , "HH_income10" , "HH_income11"  ,  "HH_income12"   , "HBOTHER"  ,     "HBSCHOOL"      , "HBSHOP"   ,      "HBWORK"      ,   "NHBOTHER"    , "NHBWORK"    ,    "TOTAL"      ,    "HH_income"    ,  "HH_income_2" ,   "HH_income_sqrt" ,"HH_size2"  ,     "HH_size3"  ,"HH_size4"    ,   "HH_size5"  ,     "HH_size6"    ,   "HH_size7"  ,     "HH_size8"   ,    "HH_size51",      "Area"     , "HH_size_L"    ,  "HH_income_inv" ))
  
  q7<-ggplot(bbdd_,aes(value,colour=variable))+stat_ecdf(size=1.5)+scale_colour_manual(name="Source",values=c("darkblue", "orange"),labels=c("Survey", "Model"))+xlab("Total travel time (min)")+ylab("Cumulative density function")+theme_bw()+theme(legend.position="bottom",legend.text=element_text(size=12,face="bold"),legend.title=element_text(size=12,face="bold"))+theme(axis.title.x=element_text(face="bold",size=16),axis.text.x=element_text(face="bold",size=16),axis.title.y=element_text(face="bold",size=16),axis.text.y=element_text(face="bold",size=16))+ guides(colour = guide_legend(override.aes = list(size=2)))+theme(plot.title = element_text(lineheight=.8, face="bold"))+xlim(0,1500)
  q7
  
  jpeg(file="C:/Users/AnaTsui/Desktop/Maryland DB/R_HH/q7.jpeg")
  q7
  dev.off()

