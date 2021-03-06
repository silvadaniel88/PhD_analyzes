#### Libraries ####
library (devtools)
library (ncf)
library (nlme)
library(MuMIn)
library(ggplot2)
library(ggpubr)
library(ggridges)
library(reshape2)
library(plyr)
library(dplyr)
library(interactions)
library(dotwhisker)
library(tidyverse)
library(cowplot)
library(GGally)
library(ggiraphExtra)
library(gridExtra)

#Anova com corre��o espacial
#https://github.com/pedroeisenlohr/spatialANOVA


#### Data ####
setwd("folder")
wdname<-"folder"
df<-read.csv("data.csv",sep=";", fileEncoding="UTF-8-BOM")
names(df)
dfs <- df[-100, ] #take out primary forest plot from dataset
dfs$dist_edgel<-log1p(dfs$dist_edge) #log transform distance to edge

symbols(x = df$long, y = df$lat, circles = df$agb_t, inches = 0.1)
ggcorr(dfs[,c(8:23)], label=TRUE) #visualize correlation between variables


#### linear relationships ####
ggplot(dfs, aes(dist_edge,agb_t)) + 
  geom_point() + 
  geom_smooth()#method=lm
ggplot(dfs, aes(dist_edgel,agb_t)) + 
  geom_point() + 
  geom_smooth()

summary(lm(agb_t~dist_edgel,data=dfs))
plot(lm(agb_t~dist_edgel,data=dfs))
summary(lm(agb_t~dist_edge,data=dfs))
plot(lm(agb_t~dist_edge,data=dfs))

ggplot(dfs, aes(dist_edge,gpp_5_yr)) + 
  geom_point() + 
  geom_smooth()#method=lm
ggplot(dfs, aes(dist_edgel,gpp_5_yr)) + 
  geom_point() + 
  geom_smooth()#method=lm

summary(lm(gpp_5_yr~dist_edge,data=dfs))
plot(lm(gpp_5_yr~dist_edge,data=dfs))
summary(lm(gpp_5_yr~dist_edgel,data=dfs))
plot(lm(gpp_5_yr~dist_edgel,data=dfs))

#### Model selection AGB STOCK ####
global_agb<-lm(agb_t~lai+r*dist_edgel+rc*dist_edgel,data=dfs,na.action = "na.fail")
sel_agb<-dredge(global_agb,beta="partial.sd", evaluate=TRUE,fixed = NULL, 
                rank="AICc",subset=TRUE,trace=2)
# The global, or base model, is the best model according to the dredge function

# Unused model averaging
# avg_agb<- model.avg(sel_agb, beta="partial.sd", subset = delta<2, fit = TRUE)
# summary(avg_agb)
#write.csv(sel_agb,"folder_path/sel_agb.csv")

#### spatial correlation AGB STOCK ####

#find the best variogram
dfs$grp<-rep(1,dim(dfs)[1]) #creates a colum with #1

null.model<-lme(agb_t~1,data=dfs,random=~1|grp) #creates a null model
summary(null.model)
exp.sp<-update(null.model, correlation = corExp(1, form = ~ long + lat), method = "ML")
summary(exp.sp) # exponential variogram
gau.sp<-update(null.model, correlation = corGaus(1, form = ~ long + lat), method = "ML")
summary(gau.sp) # gaussian variogram
sph.sp<-update(null.model, correlation = corSpher(1, form = ~ long + lat), method = "ML")
summary(sph.sp) # spherical variogram
ln.sp<-update(null.model, correlation = corLin(1, form = ~ long + lat), method = "ML")
summary(ln.sp) # linear variogram - not running
rat.sp<-update(null.model, correlation = corRatio(1, form = ~ long + lat), method = "ML")
summary(rat.sp) # Rational quadratic variogram
#Based on AIC, the spherical variogram is the best one for AGB


fit1<-lm(agb_t~lai+r*dist_edgel+rc*dist_edgel,data=dfs) #base model
summary(fit1)
plot(fit1)
  cres = spline.correlog(x = df$long, y = df$lat, z = resid(fit1), resamp = 0)
  plot(cres) #visualize spatial autocorrelation
  

fit2<-lme(agb_t~lai+rc*dist_edgel+r*dist_edgel,data=dfs,random=~1|grp,
          corr=corSpatial(form=~long+lat,type='s',nugget=F),method='ML')
summary(fit2) #model with spatial correlation term
plot_model(fit2)

fit3<-lme(agb_t~lai+rc*dist_edgel+r*dist_edgel,data=dfs,random=~1|grp,method='ML') #model w/o spatial correlation term
anova(fit2,fit3) #compare the two models (w and w/o spatial correlation terms)
summary(fit3)

#there is no difference between the model accounting for the spatial correlation and the model
#w/o the spatial correlation term

#### Model selection AGB CHANGE ####
global_gpp<-lm(gpp_5_yr~lai+dist_edgel*rp+dist_edgel*r,data=dfs,na.action = "na.fail")
sel_gpp<-dredge(global_gpp,beta="partial.sd", evaluate=TRUE,fixed = NULL, 
                rank="AICc",subset=TRUE,trace=2)
# The best model according to the dredge function is the one including only distance to edge and richenss of pionneer species

# unused model averaging
#avg_gpp<- model.avg(sel_gpp, beta="partial.sd", subset = delta<5, fit = TRUE)
#summary(avg_gpp)

##### spatial correlation AGB CHANGE ####
null.model<-lme(gpp_5_yr~1,data=dfs,random=~1|grp) #creates a null model
summary(null.model)
exp.sp<-update(null.model, correlation = corExp(1, form = ~ long + lat), method = "ML")
summary(exp.sp) # exponential variogram
gau.sp<-update(null.model, correlation = corGaus(1, form = ~ long + lat), method = "ML")
summary(gau.sp) # gaussian variogram
sph.sp<-update(null.model, correlation = corSpher(1, form = ~ long + lat), method = "ML")
summary(sph.sp) # spherical variogram
ln.sp<-update(null.model, correlation = corLin(1, form = ~ long + lat), method = "ML")
summary(ln.sp) # linear variogram - not running
rat.sp<-update(null.model, correlation = corRatio(1, form = ~ long + lat), method = "ML")
summary(rat.sp) # Rational quadratic variogram


fit10<-lm(gpp_5_yr~rp+dist_edgel,data=dfs) #base model
summary(fit10)
plot(fit10)
cres = spline.correlog(x = df$long, y = df$lat, z = resid(fit10), resamp = 0)
plot(cres) #visualize spatial autocorrelation

fit20<-lme(gpp_5_yr~rp+dist_edgel,data=dfs,random=~1|grp,
          corr=corSpatial(form=~long+lat,type='s',nugget=F),method='ML')
summary(fit20) #model with spatial correlation term

fit30<-lme(gpp_5_yr~rp+dist_edgel,data=dfs,random=~1|grp,method='ML') #model w/o spatial correlation term
anova(fit20,fit30) #compare the two models (w and w/o spatial correlation terms
summary(fit30)

#There is no significant difference between the models with and w/o spatial correlation term


##### Unused visualizations ########

# #INTERACTION RICHNESS DISTANCE TO EDGE
# interact_plot(fit1,pred=rc,modx=dist_edge,plot.points = TRUE,
#               x.label = "Climax species Richness", y.label = "AGB (Mg ha-1)",
#               legend.main = "Distance to edge (m)",
#               colors = "seagreen")+theme_classic()#,interval = TRUE,int.width = 0.8) linearity.check = TRUE,
# 
# #INTERACTION CLIMAX RICHNESS DISTANCE TO EDGE
# interact_plot(fit1,pred=r,modx=dist_edge,plot.points = TRUE,
#               x.label = "Species Richness", y.label = "AGB (Mg ha-1)",
#               legend.main = "Distance to edge (m)",
#               colors = "seagreen")+theme_classic()
# 
# ggplot(df,aes(y=agb_t,x=1))+
#   geom_boxplot(outlier.shape=NA)+
#   geom_jitter(position=position_jitter(width=.1, height=0))
# 
# ggplot(df,aes(y=rc,x=1))+
#   geom_boxplot(outlier.shape=NA)+
#   geom_jitter(position=position_jitter(width=.1, height=0))
# 
# de<-df$dist_edge
# ggplot(data=df,aes(y=gpp_5_yr,x=agb_t))+
#   geom_smooth(color = 'blue',size=0.1,se=T,method='lm')+
#   geom_point(aes(fill=de),shape=22,color='black',
#              alpha = 0.7,stroke = 1.5,size = 3.0)+
#   scale_fill_gradient(low = "yellow", high = "blue", na.value = NA)+
#   geom_point(data=df[100,],aes(y=gpp_5_yr,x=agb_t),shape=22,fill='red',color='black',
#              alpha = 1,stroke = 1.5,size = 3.0)+
#   xlab(bquote("AGB stock"~(Mg.ha^-1))) + 
#   ylab(bquote("AGB change"~(Mg.ha.5yr^-1)))+
#   labs(fill = 'Distance to edge (m)')+
#   theme(legend.position = 'bottom')


# ggplot(data=df)+
#   geom_density(aes(agb_t))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(gpp_5_yr))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(lai))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(r))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(rc))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(rs))+theme_pubr()
# ggplot(data=df)+
#   geom_density(aes(rp))+theme_pubr()
# ggplot(data=df)+
#    geom_density(aes(dist_edge))+theme_pubr()
# 
# ggplot(data=df, aes(lai))+ #AGB LAI
#   geom_point(aes(x=lai,y=agb_t,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   geom_smooth(aes(x=lai,y=agb_t),method=lm, alpha = 0.2,colour='black')+
#   scale_y_continuous(limits = c(0, 450))+theme_classic()+
#   stat_cor(
#     aes(x=lai,y=agb_t,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 3,label.y = 0) +
#   theme(legend.position="none") +xlab("LAI")+ylab("AGB (Mg ha-1)")+ 
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))
# 
# ggplot(data=df, aes(dist_edge))+ #AGB DIST EDGE
#   geom_point(aes(x=dist_edge,y=agb_t,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   geom_smooth(aes(x=dist_edge,y=agb_t),method=lm, alpha = 0.2,colour='black')+
#   theme_classic()+
#   stat_cor(
#     aes(x=dist_edge,y=agb_t,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 350,label.y = 1) +
#   theme(legend.position="none") +xlab("Distance to edge (m)")+ylab("AGB (Mg ha-1)")+
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))
# 
# ggplot(data=df, aes(r))+ #AGB RICHNESS
#   geom_point(aes(x=r,y=agb_t,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   geom_smooth(aes(x=r,y=agb_t),method=lm, alpha = 0.2,colour='black')+
#   theme_classic()+
#   stat_cor(
#     aes(x=r,y=agb_t,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 25,label.y = 1) +
#   theme(legend.position="none") +xlab("Species richness")+ylab("AGB (Mg ha-1)")+
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))
#   
# ggplot(data=df, aes(rc))+ #AGB CLIMAX RICHNESS
#   geom_point(aes(x=rc,y=agb_t,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   #geom_smooth(aes(x=rc,y=agb_t),method=lm, alpha = 0.2,colour='black')+
#   theme_classic()+
#   #stat_cor(
#   #  aes(x=rc,y=agb_t,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#   #  label.x = 25,label.y = 1) +
#   theme(legend.position="none") +xlab("Climax Species richness")+ylab("AGB (Mg ha-1)")+
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))
# 
# ggplot(data=df, aes(rp))+ #GPP PIONEER RICHNESS
#   geom_point(aes(x=rp,y=gpp_5_yr,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   geom_smooth(aes(x=rp,y=gpp_5_yr),method=lm, alpha = 0.2,colour='black')+
#   theme_classic()+
#   stat_cor(
#     aes(x=rp,y=gpp_5_yr,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 10,label.y = -25) +
#   theme(legend.position="none") +xlab("Pioneer Species richness")+ylab("AGB change (Mg ha-1)")+
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))
# 
# ggplot(data=df, aes(dist_edge))+ #GPP DISTANCE TO EDGE
#   geom_point(aes(x=dist_edge,y=gpp_5_yr,size = 0.1),colour='olivedrab4',alpha = 0.8,stroke = 1)+
#   geom_smooth(aes(x=dist_edge,y=gpp_5_yr),method=lm, alpha = 0.2,colour='black')+
#   theme_classic()+
#   stat_cor(
#     aes(x=dist_edge,y=gpp_5_yr,label = paste(..rr.label.., ..p.label.., sep = "~`,`~")),
#     label.x = 250,label.y = -25) +
#   theme(legend.position="none") +xlab("Distance to edge (m)")+ylab("AGB change (Mg ha-1)")+
#   theme(axis.title.y = element_text(size = 15))+
#   theme(axis.title.x = element_text(size = 15))



##### ANOVA AGB ####
#creating columns for proportion of agb
dfs$agb_cp<-dfs$agb_c/dfs$agb_t*100
dfs$agb_sp<-dfs$agb_s/dfs$agb_t*100
dfs$agb_pp<-dfs$agb_p/dfs$agb_t*100
dfs$agb_ncp<-dfs$agb_nc/dfs$agb_t*100

#data wrangling to perform ANOVA
dfm<-melt(dfs,id.vars ='ua',measure.vars =c('agb_p','agb_s','agb_c','agb_nc'))
na.exclude(dfm)
dfm2<-melt(dfs,id.vars ='ua',measure.vars =c('agb_pp','agb_sp','agb_cp','agb_ncp'))
na.exclude(dfm2)

#ANOVA
fm1 <- aov(value~variable, data=dfm)
anova(fm1)
posthoc <- TukeyHSD(fm1)
plot(fm1)

#homogenety of variance
df2<-melt(dfs,id.vars ='ua',measure.vars =c('agb_p','agb_s'))
var.test(value~variable,data=df2)

df3<-melt(dfs,id.vars ='ua',measure.vars =c('agb_c','agb_s'))
var.test(value~variable,data=df3)

bartlett.test(value~variable,data=dfm) #the variance between ecological groups is not homogeneous 


fm1 <- oneway.test(value~variable, data=dfm, var.equal = FALSE) #one-way ANOVA with welch correction

library(userfriendlyscience)
posthocTGH(dfm$value,dfm$variable, method="games-howell",
           conf.level = 0.95, digits=2, p.adjust="none",
           formatPvalue = TRUE) #post-hoc test Games_Howell for groups with differing variations


###### Dashboard with main results 
ge<-dfm$variable
ge<-revalue(ge,c('agb_p'='Pioneiras','agb_s'='Secundarias','agb_c'='Clim�cicas','agb_nc'='N�o Classificadas'))
ge_agb<-dfm$value

ge2<-dfm2$variable
ge2<-revalue(ge2,c('agb_pp'='Pioneiras','agb_sp'='Secundarias','agb_cp'='Clim�cicas','agb_ncp'='N�o Classificadas'))
ge_agb2<-dfm2$value


prim <- df[100, ]
prim <- melt(prim,id.vars ='ua',measure.vars =c('agb_p','agb_s','agb_c','agb_nc'))
gem<-prim$variable
gem<-revalue(gem,c('agb_p'='Pioneiras','agb_s'='Secundarias','agb_c'='Clim�cicas','agb_nc'='N�o Classificadas'))
ge_agbm<-prim$value


#boxplot agb
box_agb<-ggplot(data=dfm, aes(ge_agb),na.omit(dfm),show.legend=F)+
  geom_boxplot(aes(x=ge,y=,ge_agb),lwd = 0.8, fatten = 0.4, width=0.9, cex=0.3,outlier.colour=NA, 
               fill="#165e82",colour = 'black', position = position_dodge(.2),show.legend=F)+
  stat_summary(aes(x=ge,y=ge_agb),fun.y=mean, geom="point", shape=15, size=1, 
               color="black", fill="black") +
  geom_point(data=prim,aes(x=gem,y=ge_agbm),shape=22,fill='red',color='black',
              alpha = 1,stroke = 1.5,size = 2.0)+
  annotate('text',label = 'b', x=1, y=65,size = 4)+
  annotate('text',label = 'a', x=2, y=155,size = 4)+
  annotate('text',label = 'b', x=3, y=77,size = 4)+
  annotate('text',label = 'c', x=4, y=36,size = 4)+
  ylab(bquote("BAS"~(Mg.ha^-1)))+xlab("")+scale_x_discrete()+coord_cartesian(ylim = c(0, 200))
# 
# annotate('text',label = 'b', x=1, y=140,size = 4)+
#   annotate('text',label = 'a', x=2, y=370,size = 4)+
#   annotate('text',label = 'b', x=3, y=230,size = 4)+
#   annotate('text',label = 'c', x=4, y=120,size = 4)+
#   


#agb ecological group facet
agbc<-ggplot(dfs,aes(x=ua,y=agb_cp))+geom_bar(stat="identity", fill="#165e82")+ #,fill=dist_edge
  # scale_fill_gradient(
  #   low = "#9fcbe2",
  #   high = "#002742",
  #   space = "Lab",
  #   na.value = "grey50",
  #   guide = "colourbar",
  #   aesthetics = "fill")+
  labs(x='',y='',title='Clim�cicas')+ylim(0,100)+
  geom_hline(yintercept = 50, colour = "grey60", linetype = 2)+
  guides(fill=guide_coloursteps (title="Dist�ncia para borda (m)"))+
  theme(legend.position="none",axis.text.x=element_blank(),
        plot.title = element_text(size=10,hjust=0.5,vjust=0))

agbs<-ggplot(dfs,aes(x=ua,y=agb_sp))+geom_bar(stat="identity", fill="#165e82")+ #,fill=dist_edge
  # scale_fill_gradient(
  #   low = "#9fcbe2",
  #   high = "#002742",
  #   space = "Lab",
  #   na.value = "grey50",
  #   guide = "colourbar",
  #   aesthetics = "fill" )+
  labs(x='',y='',title='Secundarias')+ylim(0,100)+
  geom_hline(yintercept = 50, colour = "grey60", linetype = 2)+
  guides(fill=guide_coloursteps (title="Dist�ncia para borda (m)"))+
  theme(legend.position="none",axis.text.x=element_blank(),
        plot.title = element_text(size=10,hjust=0.5,vjust=0))

agbp<-ggplot(dfs,aes(x=ua,y=agb_pp))+geom_bar(stat="identity", fill="#165e82")+ #,fill=dist_edge
  # scale_fill_gradient(
  #   low = "#9fcbe2",
  #   high = "#002742",
  #   space = "Lab",
  #   na.value = "grey50",
  #   guide = "colourbar",
  #   aesthetics = "fill")+
  labs(x='',y='',title='Pioneiras')+ylim(0,100)+
  geom_hline(yintercept = 50, colour = "grey60", linetype = 2)+
  guides(fill=guide_coloursteps (title="Dist�ncia para borda (m)"))+
  theme(legend.position="none",axis.text.x=element_blank(),
        plot.title = element_text(size=10,hjust=0.5,vjust=0))

agbnc<-ggplot(dfs,aes(x=ua,y=agb_ncp))+geom_bar(stat="identity", fill="#165e82")+ #,fill=dist_edge
  # scale_fill_gradient(
  #   low = "#9fcbe2",
  #   high = "#002742",
  #   space = "Lab",
  #   na.value = "grey50",
  #   guide = "colourbar",
  #   aesthetics = "fill")+
  labs(x='',y='',title='N�o Classificadas')+ylim(0,100)+
  geom_hline(yintercept = 50, colour = "grey60", linetype = 2)+
  guides(fill=guide_coloursteps,(title="Dist�ncia para borda (m)"))+
  theme(legend.position="none",axis.text.x=element_blank(),
        plot.title = element_text(size=10,hjust=0.5,vjust=0))

legend <- get_legend(agbc+theme(legend.position="bottom",legend.title=element_text(size=10)))

prow<- plot_grid(agbc,agbs,agbp,agbnc,nrow=4,ncol=1, #legend, ,rel_heights = c(2,2,2,2,.6)
                 labels='D')+
  draw_label("BAS (%)", x=  0, y=0.53, vjust= 1.5, angle=90,size=9)+
  draw_label("Unidade Amostral", x=0.56, y=  0, vjust=-1, angle= 0,size=10)

### histograma da mudan�a de AGB
histpro <- ggplot(dfs,aes(x=gpp_5_yr))+
  geom_histogram(binwidth=8,boundary=0,fill="#165e82",colour='black',lwd=0.8)+
  geom_vline(xintercept = 9.36,colour='blue',lwd=0.8)+
  #geom_vline(xintercept = 45.40,colour='red',lwd=0.8)+
  scale_x_continuous(breaks = c(-40,-30,-20,-10,0,10,20,30,40))+
  xlab(bquote("BAS"~(Mg.ha.5yr^-1)))

##### AGB por classe de DAP e GE
dbh<-read.csv("dbh_agb.csv",sep=";", fileEncoding="UTF-8-BOM")
names(dbh)
dbh <- filter(dbh,ua != 1025)

dbh$dap_class <- cut(dbh$dap,breaks=c(0,15,25,35,45,55,65,75,85,100,230))
dbh$agb_per <- dbh$agb/sum(dbh$agb)*100
dbh$ge <- factor(dbh$ge, levels = c('C','SE','P','NC'))

soma = group_by(dbh, dap_class) %>% summarise(agbclass = sum(agb),agbper = sum(agb_per))


dap<-ggplot(dbh, aes(x=dap_class, y=agb_per,fill=ge))+geom_bar(stat = 'identity')+
  ylab('BAS (%)')+xlab('Classe diam�trica (cm)')+scale_fill_manual(values = c("#00314f","#784585","#e9526b","#ffa600"))+ #scale_fill_brewer(palette="YlGnBu")+
  labs(fill = 'Grupo Ecol�gico')+
  theme(axis.text.x = element_text(angle = 45,size = 8,hjust = 1),legend.position="top")


prow2<- plot_grid(box_agb,dap,histpro,ncol=1,labels = c('A', 'B','C'))

grid4<-plot_grid(prow2,prow,ncol = 2)
ggsave(filename = "results.tiff", 
       plot = grid4, 
       device = "tiff", 
       dpi = 600, 
       width = 227.99,
       height = 203.67, 
       units = "mm")
# End of Dashboard

##### PLOT MODELS ####

#plots with measured vs. estimated values and predictors coefficients for both agb stock and change

fit1df<-tidy(fit1)
#plot coefficients 
estimates1<-dwplot(fit1df, show_intercept = TRUE,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))%>% 
       relabel_predictors(lai = 'IAF',
                       rc = 'Riqueza de esp�cies clim�cicas',
                       dist_edgel = 'Distancia para borda',
                       r = 'Riqueza total de esp�cies')+
  scale_colour_grey(start = 0.1, end = 0.1)+
  theme(legend.position="none") + xlab("Coeficiente") + ylab("")+
  annotate(geom='text',x=5,y=7.3,size=3, label=paste('22,94 (7,86)*'))+
  annotate(geom='text',x=5,y=6.3,size=3, label=paste('7,92 (3,32)*'))+
  annotate(geom='text',x=5,y=5.3,size=3, label=paste('-2.98 (12,87)ns'))+
  annotate(geom='text',x=5,y=4.3,size=3, label=paste('1,31 (1,22)ns'))+
  annotate(geom='text',x=5,y=3.3,size=3, label=paste('68,72 (53,88)ns'))+
  annotate(geom='text',x=5,y=2.3,size=3, label=paste('0,59 (0,23)*'))+
  annotate(geom='text',x=5,y=1.3,size=3, label=paste('-1,76 (0,68)*'))

pred_agb <- predict(fit1, newdata = dfs) #predict agb with fit1 model
dfs$pred_agb<-pred_agb #add prediction to df


plot_fit1<-ggplot(data=dfs, aes(x=agb_t, y=pred_agb))+
  geom_abline(intercept = 0)+xlim(1,415)+ylim(1,415)+
  geom_smooth(method='lm',color = 'blue',size=0.1,se=T)+
  geom_point(aes(x=agb_t, y=pred_agb),shape=22,fill='#165e82',color='black',
             alpha = 0.6,stroke = 1.5,size = 3.0)+
  #geom_point(data=df[100,],aes(x=agb_t, y=pred_agb),shape=22,fill='red',color='black',
             #alpha = 0.6,stroke = 1.5,size = 3.0)+
  xlab(bquote("BAS medido"~(Mg.ha^-1))) + 
  ylab(bquote("BAS predito"~(Mg.ha^-1)))+
  annotate(geom='text',x=300,y=10,size=5,label=paste('R2 = 0.36;   p < 0.05'))

grid1<-plot_grid(plot_fit1,estimates1, nrow=1,ncol=2)

# ggsave(filename = "agb.tiff", 
#        plot = grid1, 
#        device = "tiff", 
#        dpi = 600, 
#        width = 185.41,
#        height = 93.63, 
#        units = "mm")

fit10df<-tidy(fit10)

estimates<-dwplot(fit10df,show_intercept = TRUE,
       vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2))%>% 
  relabel_predictors(rp = 'Riqueza de esp�cies pioneiras',
                     dist_edgel = 'Distancia para borda')+
  scale_colour_grey(start = 0.1, end = 0.1)+
  theme(legend.position="none") + xlab("Coeficiente") + ylab("")+
  annotate(geom='text',x=0.25,y=2.2,size=3.5, label=paste('3,34 (0,88)*'))+
  annotate(geom='text',x=0.25,y=3.2,size=3.5, label=paste('0,49 (0,28)ns'))+
  annotate(geom='text',x=0.25,y=1.2,size=3.5, label=paste('-13,60 (5,30)*'))
  
  
pred_gpp <- predict(fit10, newdata = dfs) #predict agb with fit1 model
dfs$pred_gpp<-pred_gpp #dd prediction to df

plot_fit10<-ggplot(data=dfs, aes(x=gpp_5_yr, y=pred_gpp))+
  geom_abline(intercept = 0)+xlim(-38,50)+ylim(-38,50)+
  geom_smooth(method='lm',color = 'blue',size=0.1,se=T)+
  geom_point(aes(x=gpp_5_yr, y=pred_gpp),shape=22,fill='#165e82',color='black',
             alpha = 0.5,stroke = 1.5,size = 3)+
  #geom_point(data=df[100,],aes(x=gpp_5_yr, y=pred_gpp),shape=22,fill='red',color='black',
  #          alpha = 0.6,stroke = 1.5,size = 3.0)+
  xlab(bquote('Mudan�a no BAS medida'~(Mg.ha.5anos^-1))) + 
  ylab(bquote('Mudan�a no BAS predita'~(Mg.ha.5anos^-1)))+
  #annotate(geom='text',x=19,y=40,label=paste('AGBc = -2.84 + rp x 0.641 + de x 0.016'))+
  annotate(geom='text',x=10,y=40,size=5,label=paste('R2 = 0.14;   p < 0.05'))

  
grid2<-plot_grid(plot_fit10,estimates, nrow=1,ncol=2)
# ggsave(filename = "agbc.tiff", 
#        plot = grid2, 
#        device = "tiff", 
#        dpi = 600, 
#        width = 185.41,
#        height = 93.63, 
#        units = "mm")


##### PLOT INTERACTIONS #####

# ploting the interaction terms of the AGB stock model

fiti1<-lm(agb_t~r*dist_edgel,data=dfs)
p1<-ggPredict(fiti1,se=T,interactive=TRUE)
ggsave(filename = "interact1.tiff", 
       plot = p1, 
       device = "tiff", 
       dpi = 600, 
       width = 177,
       height = 177, 
       units = "mm")

# reddinteract<-ggPredict(fiti1,se=T,interactive=TRUE)
# ggplot(reddinteract)+theme_bw()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+  xlab("Distance to Lake")+ylab("Red brocket deer relative abundance")
# fiti1<-fiti1+ theme(axis.text.y=element_text(family="Times",size=20,color="black"),
#                   axis.text.x=element_text(family="Times",size=20,face="bold",color="black",angle = 45,hjust = 1),
#                   axis.title.y=element_text(family="Times",size=25,face="bold"),
#                   axis.title.x=element_text(family="Times",size=25,face="bold"),
#                   legend.title=element_blank(),legend.text = element_text(family="Times",colour="black", size = 10))


fiti2<-lm(agb_t~dist_edgel*rc,data=dfs)
p2<-ggPredict(fiti2,se=T,interactive=TRUE)

grid3<-plot_grid(p1,p2,ncol=2)






