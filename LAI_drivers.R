#### Libraries ####
library (ggplot2)
library (ggpubr)
library (cowplot)
library (patchwork)
library (nnet)
library (nlme)
library (psych)
library (plyr)
library (dplyr)
library (tidyverse)
library (scales)
library (piecewiseSEM)
library (MuMIn)
library (GGally)
library (RColorBrewer)
library (stats)
library(car)


#### load data ######## 
  setwd("D:/OneDrive - FURB/Doutorado/Estudos/Forçantes ambientais")
  wdname<-"D:/OneDrive - FURB/Doutorado/Estudos/Forçantes ambientais"
  df<-read.csv("data.csv",sep=";")

df[,c(12,14:16)]<-list(NULL) #dropping cons_ind, LanPro3,EucNeaDis3 and NumOfPat3 - unused variables 

#creating and ordering forest types as factors
FT1 <- factor(df$FT,levels=c("RES","RF","RF/AF","AF",'AF/DF',"DF" ),ordered=TRUE) 
FT1 <- revalue(FT1, c("RES"="Restinga","RF"="Rainforest","RF/AF"="Rainforest/Araucaria Forest",
               "AF"="Araucaria Forest","AF/DF"="Araucaria/Semi-Deciduous Forest","DF"="Semi-Decidious Forest"))

#check distributions
# lapply(df[,c(2,3,5:14)], function(x) hist(x)) #histogram of all continuous variables in the dataframe
# lapply(df[,c(2,3,5:14)], function(x){ 
#   (qqnorm(x))
#   qqline(x)}) #qqnorm and line of all continuous variables in the dataframe
# 
# dfs<-df[,c(2,3,5:14)] #select only continuous variables to the new df
# dfs$agri_pas_p <- dfs$agri_pas_p*100 #from 0-1 to 0-100 for log transformation
# 
# dfs<-data.frame(lapply(dfs[,c(4,7,8,9,11,12)], function(x) log1p(x))) #apply log transformation to non-normal variables
# head(dfs)
# 
# #check the new distribuitions
# lapply(dfs, function(x) hist(x)) #histogram of all continuous variables in the dataframe
# lapply(dfs, function(x){ 
#   (qqnorm(x))
#   qqline(x)})
# 
# #joining the two data frames
# dfs<-cbind(df[,c(1:5,7,8,12)],dfs) 
# head(dfs)
# 

dfs<-df
dfp<-dfs[,c(1,4)] #temporary for non-continuous variables

#rescaling normal variables 
dfs <- data.frame(lapply(dfs[,c(2,3,5:14)], function(x) rescale(x,to=c(0,1)))) 
head(dfs)
dfs<-cbind(dfp,dfs) #joinng dataframes
names(dfs)

##end of scaling and transformation



#### select variables #########

#checking colinearity in predictors
ggcorr(dfs[,c(3:14)], label=TRUE)

corr.test(dfs[,c(3:14)]) #correlation in the dataframe

dfs[,c(6)]<-list(NULL) #droping min_temp 

cor(dfs$LAI,dfs[,c(5:13)]) #correlation between LAI and predictors
pairs(dfs[,c(5:13)])

## VIF test ##
dfv<-dfs
names(dfv)
dfv[,c(1,2,4,12)]<-list(NULL)
names(dfv)


# abiotic: min_rain, aridity, slope, altitude
# human: press_50, dist_city, dist_road, agri_pas_p
# fragmentation: dist_edge
# forest: FT

#unused model averaging aproach
#abiotic
abiotic<-lm(LAI~altitude+slope+min_rain+aridity, data=dfs,na.action = "na.fail")
abiotic_select <-dredge(abiotic,beta="partial.sd", evaluate=TRUE,fixed = NULL, rank="AICc",subset=TRUE,trace=2)
avg_abiotic <- model.avg(abiotic_select, beta="partial.sd", subset = delta<2, fit = TRUE)
summary(avg_abiotic)


#human
human<-lm(LAI~press_50+dist_city+dist_road+agri_pas_p, data=dfs,na.action = "na.fail")
human_select <-dredge(human,beta="partial.sd", evaluate=TRUE,fixed = NULL, rank="AICc",subset=TRUE,trace=2)
avg_human <- model.avg(human_select, beta="partial.sd", subset = delta<2, fit = TRUE)
summary(avg_human)


#### Structured equation modeling (SEM) #########

# Hipotesys #

#Forest type = aridity + altitude 
#Pop pressure = slope + dist. city + dist. road + 
#Agriculture % = Pop press + slope
#Dist. edge = Agriculture %
#LAI/Fcover = dist. edge + pop pressure + foret type

#log transform predictors to linearize the relationship with the response variable
dfs$press_50l<-log1p(df$press_50)

dfs$agri_l<-log1p(df$agri_pas_p)

dfs$dist_edgel<-log1p(df$dist_edge)


#anova and barlett test
fm1 <- aov(LAI~FT, data=dfs)
anova(fm1)
posthoc <- TukeyHSD(fm1)


## BASE MODEL
LAIsem <- psem(
              lme(press_50l~slope+dist_city+dist_road,random=~1|FT,data=dfs),
              lme(agri_l~press_50l+slope+min_rain,random=~1|FT,data=dfs),
              lme(dist_edgel~agri_l,random=~1|FT,data=dfs),
              lme(LAI~press_50l+dist_edgel,random=~1|FT,data=dfs)
)

summary(LAIsem)

## IMPROVED MODEL
LAIsem2 <- psem(
  lme(press_50l~slope+dist_city+dist_road,random=~1|FT,data=dfs),
  lme(agri_l~press_50l+slope+dist_city+min_rain,random=~1|FT,data=dfs),
  lme(dist_edgel~agri_l+slope+min_rain,random=~1|FT,data=dfs),
  lme(LAI~press_50l+dist_edgel+min_rain,random=~1|FT,data=dfs)
)

summary(LAIsem2)

## VIF
lm1<-lme(press_50l~slope+dist_city+dist_road,random=~1|FT,data=dfs)
lm2<-lme(agri_l~press_50l+slope+dist_city+min_rain,random=~1|FT,data=dfs)
lm3<-lme(dist_edgel~agri_l+slope+min_rain,random=~1|FT,data=dfs)
lm4<-lme(LAI~press_50l+dist_edgel+min_rain,random=~1|FT,data=dfs)


#check for colinearity 
car::vif(lm1)
car::vif(lm2)
car::vif(lm3)
car::vif(lm4)


##### GRAPHS ####
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



###### boxplot - LAI between forest types
ggplot(data=df, aes(LAI))+
  geom_boxplot(aes(x=FT1,y=LAI,fill=FT1), na.omit(df), )+ scale_fill_manual(values=cbPalette)+
  theme_pubr()+ylab("LAI")+xlab("")+theme(axis.text.x=element_blank(),legend.title=element_blank())

ggplot(data=df, aes(LAI))+
  geom_boxplot(aes(x=FT1,y=LAI), na.omit(df), )+
  theme_pubr()+ylab("LAI")+xlab("")+coord_flip()+theme(legend.title=element_blank())




#### scatter plots of individual relationships between LAI and predictors

lai.mirain<-ggplot(data=df, aes(min_rain))+
  geom_point(aes(x=min_rain,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  #geom_smooth(aes(x=min_rain,y=LAI),method="lm", se=FALSE)+ 
  theme_pubr()+
  stat_cor(
    aes(x=min_rain,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 50,label.y = 1) +
  theme(legend.position="none") +xlab("Rainfall (mm/mo)")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))
  

lai.slope<-ggplot(data=df, aes(slope))+
  geom_point(aes(x=slope,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  geom_smooth(aes(x=slope,y=LAI),method="lm", se=FALSE, color="black")+ theme_pubr()+
  stat_cor(
    aes(x=slope,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 5,label.y = 1) +
  theme(legend.position="none") +xlab("Slope (º)") +ylab("")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

lai.city<-ggplot(data=df, aes(dist_city))+
  geom_point(aes(x=dist_city,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  #geom_smooth(aes(x=dist_city,y=LAI),method="lm", se=FALSE)+ 
  theme_pubr()+ 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  stat_cor(
    aes(x=dist_city,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 5000,label.y = 1) +
  theme(legend.position="none") +xlab("Distance to City (m)") +ylab("")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

lai.road<-ggplot(data=df, aes(dist_road))+
  geom_point(aes(x=dist_road,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  #geom_smooth(aes(x=dist_road,y=LAI),method="lm", se=FALSE)+ 
  theme_pubr()+
  stat_cor(
    aes(x=dist_road,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 2500,label.y = 1) +
  theme(legend.position="none") +xlab("Distance to Road (m)") +ylab("")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

lai.press<-ggplot(data=dfs, aes(press_50l))+
  geom_point(aes(x=press_50l,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  geom_smooth(aes(x=press_50l,y=LAI),method="lm", se=FALSE, color="black")+ 
  xlim(11,14)+ theme_pubr()+
  stat_cor(
    aes(x=press_50l,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 11,label.y = 0) +
  theme(legend.position="none")+xlab("log (Population Pressure)") +ylab("")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

lai.edge<-ggplot(data=dfs, aes(dist_edgel))+
  geom_point(aes(x=dist_edgel,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette) +
  geom_smooth(aes(x=dist_edgel,y=LAI),method="lm", se=FALSE, color="black")+
  xlim(0,7.2)+ theme_pubr()+
  stat_cor(
    aes(x=dist_edgel,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 1,label.y = 0) +
  theme(legend.position="none")+xlab("log [Distance to Edge (m)]")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

lai.agri<-ggplot(data=dfs, aes(agri_l))+
  geom_point(aes(x=agri_l,y=LAI, color=FT1,size = 0.4),alpha = 1/1.6,stroke = 1.2)+ scale_colour_manual(values=cbPalette)+
  xlim(0,0.75)+ theme_pubr()+
  stat_cor(
    aes(x=agri_l,y=LAI,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
    label.x = 0.0,label.y = 0) +
  theme(legend.position="none")+xlab("log [Cropland in Buffer (%)]") +ylab("")+
  theme(axis.title.y = element_text(size = 15))+
  theme(axis.title.x = element_text(size = 15))

prow=plot_grid(lai.mirain, lai.slope, lai.city, lai.road, lai.edge, lai.agri,lai.press, nrow=2,ncol=4)

prow2 = ((lai.mirain | lai.slope | lai.city | lai.road) / (lai.press | lai.edge | lai.agri))
legend_b <- get_legend(
  lai.mirain + 
    guides(color = guide_legend(nrow = 2,override.aes = list(size=5))) +
    theme(legend.position = "bottom",legend.title=element_blank(),legend.text=element_text(size=15))
    
)
plot_grid(prow, legend_b, ncol = 1, rel_heights = c(1, .2))

#unused tests
# t1<-lm(press_50l~slope+dist_city+dist_road,data=dfs)
# summary(t1)
# plot(t1)
# t2<-lm(agri_l~press_50l+slope+dist_city+min_rain,data=dfs)
# summary(t2)
# plot(t2)
# t3<-lm(dist_edgel~agri_l+slope+min_rain,data=dfs)
# summary(t3)
# plot(t3)

