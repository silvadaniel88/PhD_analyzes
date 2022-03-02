#### LIBRARIES ####
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(RColorBrewer)
library(nlme)
library(broom)
library(purrr)
library(stringr)
library(ggiraphExtra)
library(cowplot)
library(nlme)
library(ggpmisc)



####LOAD and clean DATA ####
setwd("folder path")
wdname<-"folder path"
canopy_rf<-read.csv("bases/ua14_rf_400.csv",sep=";", fileEncoding="UTF-8-BOM")
canopy_af<-read.csv("bases/ua14_afdf_400.csv",sep=";", fileEncoding="UTF-8-BOM")

com_07_rf<-read.csv("bases/ua07_rf_0714.csv",sep=";", fileEncoding="UTF-8-BOM")
com_14_rf<-read.csv("bases/ua14_rf_0714.csv",sep=";", fileEncoding="UTF-8-BOM")

com_07_af<-read.csv("bases/ua07_afdf_sr3.csv",sep=";", fileEncoding="UTF-8-BOM")
com_14_af<-read.csv("bases/ua14_afdf_sr3.csv",sep=";", fileEncoding="UTF-8-BOM")

species_rf <- read.csv("bases/sp_rf_0714.csv",sep=";", fileEncoding="UTF-8-BOM")
species_others <- read.csv("bases/sp_afdf_sr3.csv",sep=",", fileEncoding="UTF-8-BOM")

ge<-read.csv("bases/ge.csv",sep=";", fileEncoding="UTF-8-BOM")


# replace NAs with ZERO
canopy_rf[is.na(canopy_rf)] = 0
canopy_af[is.na(canopy_af)] = 0
com_07_af[is.na(com_07_af)] = 0
com_14_af[is.na(com_14_af)] = 0
species_rf[is.na(species_rf)] = 0
species_others <- species_others %>% fill(ft)
species_others[is.na(species_others)] = 0

# take out unused columns in com_14_rf and _af
com_14_rf <- com_14_rf %>% 
  select(-c(co,mean_fcover,sd_fcover,area))
com_14_rf$ft <- 'RF'
com_14_af <- select(com_14_af,-area)

#combining all the "com_" dataframes into a single one 
com_rf <- inner_join(com_07_rf, com_14_rf, by = 'ua')
com_rf <- com_rf %>% 
  mutate(density07 = density07 * 0.25,
         density07_C = density07_C * 0.25,
         density07_SE = density07_SE * 0.25,
         density07_P = density07_P * 0.25,
         density07_NC = density07_NC * 0.25,
         density = density * 0.25,
         density_C = density_C * 0.25,
         density_SE = density_SE * 0.25,
         density_P = density_P * 0.25,
         density_NC = density_NC * 0.25,
         density07_ane = density07_ane * 0.25,
         density07_ane_aut = density07_ane_aut * 0.25,
         density07_zoo = density07_zoo * 0.25,
         density07_NA = density07_NA * 0.25,
         density07_aut = density07_aut * 0.25,
         density_ane = density_ane * 0.25,
         density_ane_aut = density_ane_aut * 0.25,
         density_zoo = density_zoo * 0.25,
         density_NA = density_NA * 0.25,
         density_aut = density_aut * 0.25) #padroniza o valor da densidade para 100m² a partir de 400 m²

com_af <- inner_join(com_07_af, com_14_af, by = 'ua')
# com_af <- com_af %>% 
#   mutate(density07 = density07 * 1,
#          density07_C = density07_C * 1,
#          density07_SE = density07_SE * 1,
#          density07_P = density07_P * 1,
#          density07_NC = density07_NC * 1,
#          density = density * 1,
#          density_C = density_C * 1,
#          density_SE = density_SE * 1,
#          density_P = density_P * 1,
#          density_NC = density_NC * 1) #padroniza o valor da densidade para 100m² a partir de 100 m²


com_all <- bind_rows(com_rf, com_af) #dataframe final


#create final df
write.csv(com_all, "folder path/comparativo0714final.csv",row.names = FALSE)


# combining canopy rf and af
canopy = bind_rows(canopy_rf,canopy_af)
#add the rarefied richness to canopy df
canopy <- canopy %>% 
  mutate(density = density * 0.25,
         density_C = density_C * 0.25,
         density_SE = density_SE * 0.25,
         density_P = density_P * 0.25,
         density_NC = density_NC * 0.25,
         density_ane = density_ane * 0.25,
         density_ane_aut = density_ane_aut * 0.25,
         density_zoo = density_zoo * 0.25,
         density_NA = density_NA * 0.25,
         density_aut = density_aut * 0.25) #padroniza o valor da densidade para 100m² a partir de 400 m²)
#create final df
write.csv(canopy, "folder path/canopy2014final.csv",row.names = FALSE)

# juntando canopy com com_all para analisar o efeito do dossel na variação dos dados da reg

com_can <- select(canopy,
                  ua,mean_fcover,sd_fcover )
com_can <- left_join(com_all, com_can, by = 'ua')
#create final df
write.csv(com_can, "folder path/canopy0714final.csv",row.names = FALSE)


# including GE to the species dataframes
ge <- ge %>% 
  group_by(sp) %>% 
  summarize(ge = first(ge),
            lf = first(lf),
            ds = first(ds)) #clean ge data, excluding duplicated values

species_rf <- left_join(species_rf,ge, by = 'sp') #include ge column to species dataframe
species_others <- left_join(species_others,ge, by = 'sp') #include ge column to species dataframe

species_rf[is.na(species_rf)] = 0 #re-replacing NA values with ZERO
species_others[is.na(species_others)] = 0 #re-replacing NA values with ZERO

#combining the sp dataframes
species_rf <- species_rf %>% 
  mutate(density07 = density07 * 0.25,
         density14 = density14 * 0.25)#padroniza o valor da densidade para 100m² a partir de 400 m²
den_rf <- select(com_07_rf,ua,density07) %>% rename(dt07 = density07) #cria DF com densidade total
species_rf <- left_join(species_rf, den_rf) #junta com o dataframe das especies
species_rf <- mutate (species_rf, dt07 = dt07 * 0.25) #padroniza o valor 

species_others <- species_others %>% 
  mutate(density07 = density07 * 1,
         density14 = density14 * 1)#padroniza o valor da densidade para 100m² a partir de 100 m²

den_af <- select(com_07_af,ua,density07) %>% rename(dt07 = density07)
species_af <- filter (species_others,ft == 'AF')
species_af <-left_join(species_af,den_af)


species_df <- filter (species_others,ft == 'DF')
species_df <-left_join(species_df,den_af)


write.csv(species_rf, "folder path/species_rf.csv",row.names = FALSE)
write.csv(species_af, "folder path/species_af.csv",row.names = FALSE)
write.csv(species_df, "folder path/species_df.csv",row.names = FALSE)



# FINAL DATA ----
setwd("folder path/")
wdname<-"folder path/"

canopy14 <- read.csv("canopy2014final.csv",sep=";", fileEncoding="UTF-8-BOM")
canopy0714 <- read.csv("canopy0714final.csv",sep=";", fileEncoding="UTF-8-BOM")

comp <- read.csv("comparativo0714final.csv",sep=",", fileEncoding="UTF-8-BOM")

species <- read.csv("species_final.csv",sep=";", fileEncoding="UTF-8-BOM")
species_rf <- read.csv("species_rf.csv",sep=",", fileEncoding="UTF-8-BOM")
species_af <- read.csv("species_af.csv",sep=",", fileEncoding="UTF-8-BOM")
species_df <- read.csv("species_df.csv",sep=",", fileEncoding="UTF-8-BOM")


fcover <- read.csv("fcover_ua.csv",sep=",", fileEncoding="UTF-8-BOM")

fcover$ua <-as.integer(fcover$ua)
canopy14 <- left_join(canopy14,fcover, by = 'ua')
canopy0714 <- left_join(canopy0714,fcover, by = 'ua')

canopy0714$edgecat <- cut(canopy0714$edge, c(0,100,300,1500))
canopy0714$edgecat[is.na(canopy0714$edgecat)] = '(0,100]'
summary(canopy0714$edgecat) #split the distance to edge data into distance classes

# Canopy ----
# fcover médio vs. altura
ggplot(canopy0714)+
  geom_smooth(method='loess',se=F,colour='black',aes(x=mean_fcover, y=ht-ht07))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=ht-ht07))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=ht_P-ht07_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=ht_P-ht07_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=ht_C-ht07_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=ht_C-ht07_C))+
  geom_smooth(method='loess',se=F, colour='blue', aes(x=mean_fcover, y=ht_SE-ht07_SE))+
  geom_point(colour='blue', alpha = 0.1, aes(x=mean_fcover, y=ht_SE-ht07_SE))+
  geom_hline(yintercept = 0)+ facet_wrap(~ft)


# fcover médio vs. variação na densidade
ggplot(canopy0714,na.rm=T)+
  geom_smooth(method='loess',se=F,colour='black',aes(x=mean_fcover, y=density-density07))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=density-density07))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=density_P-density07_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=density_P-density07_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=density_C-density07_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=density_C-density07_C))+
  geom_smooth(method='loess',se=F, colour='blue', aes(x=mean_fcover, y=density_SE-density07_SE))+
  geom_point(colour='blue', alpha = 0.1, aes(x=mean_fcover, y=density_SE-density07_SE))+
  geom_hline(yintercept = 0)+ facet_wrap(~ft)

# fcover médio vs. riqueza total
ggplot(canopy0714)+
  geom_smooth(method='lm',se=F, colour='black', aes(x=mean_fcover, y=r))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=r))+
  facet_wrap(~ft)

# fcover médio vs. riqueza rareficada variação %
# ggplot(canopy0714)+
#   geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=rr_p/rr-rr_p07/rr07))+
#   geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=rr_p/rr-rr_p07/rr07))+
#   geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=rr_c/rr-rr_c07/rr07))+
#   geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=rr_c/rr-rr_c07/rr07))+
#   geom_smooth(method='loess',se=F, colour='blue', aes(x=mean_fcover, y=rr_se/rr-rr_se07/rr07))+
#   geom_point(colour='blue', alpha = 0.1, aes(x=mean_fcover, y=rr_se/rr-rr_se07/rr07))+
#   geom_hline(yintercept = 0)+ facet_wrap(~ft)

# fcover variação vs. riqueza total
ggplot(canopy0714)+
  geom_smooth(method='lm',se=F, colour='black', aes(x=sd_fcover, y=r))+
  geom_point(colour='black', alpha = 0.1, aes(x=sd_fcover, y=r))+
  facet_wrap(~ft)

# fcover skew vs. riqueza
ggplot(canopy0714)+
  geom_smooth(method='lm',se=F, colour='black', aes(x=skew, y=r))+
  geom_point(colour='black', alpha = 0.1, aes(x=skew, y=r))+
  facet_wrap(~ft)

# Comparing 07 with 14 ----
#densidade 07 vs. variação na densidade 07-14 ge/plot level
ggplot(comp)+
  geom_smooth(method='loess', se=F,  colour='green', aes(x=density07_C, y=density_C-density07_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=density07_C, y=density_C-density07_C))+
  geom_smooth(method='loess', se=F,  colour='red', aes(x=density07_P, y=density_P-density07_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=density07_P, y=density_P-density07_P))+
  geom_smooth(method='loess', se=F, colour='blue', aes(x=density07_SE, y=density_SE-density07_SE))+
  geom_point(colour='blue', alpha = 0.1, aes(x=density07_SE, y=density_SE-density07_SE))+
  geom_hline(yintercept = 0)+#xlim(0,0)+ylim(1,24000)+
  geom_smooth(method='loess', se=F,  colour='black', aes(x=density07, y=density-density07))+
  geom_point(colour='black', alpha = 0.1, aes(x=density07, y=density-density07))+
  facet_wrap(~ft)

# riqueza rarificada 07 vs  riqueza rarificada 14
ggplot(comp)+
  geom_smooth(method='loess', se=F,  colour='green', aes(x=rr_c07, y=rr_c-rr_c07))+
  geom_point(colour='green', alpha = 0.1, aes(x=rr_c07, y=rr_c-rr_c07))+
  geom_smooth(method='loess', se=F,  colour='red', aes(x=rr_p07, y=rr_p-rr_p07))+
  geom_point(colour='red', alpha = 0.1, aes(x=rr_p07, y=rr_p-rr_p07))+
  geom_smooth(method='loess', se=F, colour='blue', aes(x=rr_se07, y=rr_se-rr_se07))+
  geom_point(colour='blue', alpha = 0.1, aes(x=rr_se07, y=rr_se-rr_se07))+
  geom_abline(intercept = 0)+xlim(1,40)+ylim(1,40)+
  geom_smooth(method='loess', se=F,  colour='black', aes(x=rr07, y=rr-rr07))+
  geom_point(colour='black', alpha = 0.1, aes(x=rr07, y=rr-rr07))+
  facet_wrap(~ft)



#densidade 07 vs. densidade 14 sp/plot level
ggplot(species)+
  geom_point(alpha = 0.3, aes(x = density07, y = density14-density07, color = ge))+
  geom_smooth(method = 'loess', se = F, aes(x = density07, y = density14-density07, color = ge))+
  xlim(0,50)+ylim(-50,50)+geom_hline(yintercept = 0) + scale_color_brewer(palette="Set1")+
  facet_wrap(~ft)

#destacando a araucaria
araucaria <- species %>% 
  mutate(ge = ifelse(sp == 'Araucaria angustifolia', 'Araucaria',ge)) #muda o 'ge' das A. angustifolia para Araucaria

ggplot(species)+
  geom_point(alpha = 0.3, aes(x = density07, y = density14-density07, color = ge))+
  geom_smooth(alpha = 0.3, method = 'loess', se = F, aes(x = density07, y = density14-density07, color = ge))+
  geom_smooth(alpha = 0.1, data = subset(araucaria, ge =='Araucaria'),aes(x = density07, y = density14), #cria um subset apenas com a araucaria
              size = 2, color = 'black',se = F)+
  xlim(0,50)+ylim(-50,50)+geom_hline(yintercept = 0) + scale_color_brewer(palette="Set1")+
  facet_wrap(~ft)


#dinamica da altura e densidade sp/plot level
ggplot(species)+
  geom_point(alpha = 0.1, aes(x = density14-density07, y = ht14-ht07, color = ge))+
  geom_smooth(method = 'lm', se = F, aes(x = density14-density07, y = ht14-ht07, color = ge))+
  #xlim(1,100)+ylim(1,15)+geom_abline(intercept = 0) + scale_color_brewer(palette="Set1")+
  scale_color_brewer(palette="Set1") +facet_wrap(~ft)



#ANALYSIS ----

#densidade
canopy0714$d_change <- (canopy0714$density-canopy0714$density07)/canopy0714$density07*100

changet <- canopy0714 %>% group_by(ft) %>% 
  summarise(mean_change = mean (d_change),
            max_change = max (d_change),
            min_change = min (abs(d_change))
  )
data.change <- tibble(x = 3, y = 500, tb = list(changet))

#riqueza
canopy0714$r_change <- (canopy0714$r-canopy0714$r07)/canopy0714$r07*100

changer <- canopy0714 %>% group_by(ft) %>% 
  summarise(mean_change = mean (r_change),
            max_change = max (r_change),
            min_change = min (abs(r_change))
  )
data.rchange <- tibble(x = 3, y = 500, tb = list(changer))

canopy0714$ft <- revalue(canopy0714$ft, c('AF' = 'Araucaria Forest', 'DF' = 'Semi-Deciduous Forest', 'RF' = 'Evergreen Rain Forest')) #rename variables



# ft               mean_change max_change min_change sd_change
# <chr>                  <dbl>      <dbl>      <dbl>     <dbl>
# 1 Araucaria Forest       154.       2250       -94.7     350. 
# 2 Deciduous Forest       105.        208.      -45.9      85.7
# 3 Rain Forest             10.1       308.      -66.9      71.5
  
dfr <- select(canrf,ua,density07,density)
dfa <- select(canaf,ua,density07,density)
dfd <- select(candf,ua,density07,density)

library(reshape2)
melt_dfr <-melt(dfr,id.vars = 'ua')
melt_dfa <-melt(dfa,id.vars = 'ua')
melt_dfd <-melt(dfd,id.vars = 'ua')

d1 <- ggplot(aes(x = variable, y = value), data = melt_dfr)+
  geom_boxplot(outlier.shape = NA, fill = 'white', color = '#4d5c15',notch=FALSE,
               notchwidth = 0.8,lwd = 1)+
  geom_jitter(aes(group=ua), position = position_dodge(0.3), alpha = 0.3, show.legend = FALSE,
              shape = 21, size = 3, fill = '#4d5c15')+
  geom_line(aes(group=ua),color = '#4d5c15', size = 2,position = position_dodge(0.3), alpha = 0.1)+
  xlab('') + theme_classic()+
  ylab(bquote("Density"~(n.100m^-2)))+  
  labs(title = 'Evergreen Rain Forest')+
  scale_x_discrete(labels = c(expression(T[0]), expression(T[1])))

d2 <- ggplot(aes(x = variable, y = value, fill = variable), data = melt_dfa)+
  geom_boxplot(outlier.shape = NA, fill = 'white', color = '#ffa600',notch=FALSE,
               notchwidth = 0.8,lwd = 1)+
  geom_jitter(aes(group=ua), position = position_dodge(0.3), alpha = 0.3, show.legend = FALSE,
              shape = 21, size = 3, fill = '#ffa600')+
  geom_line(aes(group=ua),color = '#ffa600', size = 2,position = position_dodge(0.3), alpha = 0.1)+
  xlab('') + theme_classic()+
  ylab(bquote("Density"~(n.100m^-2)))+  
  labs(title = 'Araucaria Forest')+
  scale_x_discrete(labels = c(expression(T[0]), expression(T[1])))

d3 <- ggplot(aes(x = variable, y = value, fill = variable), data = melt_dfd)+
  geom_boxplot(outlier.shape = NA, fill = 'white', color = '#998600',notch=FALSE,
               notchwidth = 0.9,lwd = 1)+
  geom_jitter(aes(group=ua), position = position_dodge(0.3), alpha = 0.3, show.legend = FALSE,
              shape = 21, size = 3, fill = '#ffa600')+
  geom_line(aes(group=ua),color = '#998600', size = 2, position = position_dodge(0.3), alpha = 0.1)+
  xlab('') + theme_classic()+
  ylab(bquote("Density"~(n.100m^-2)))+  
  labs(title = 'Semi-Deciduous Forest')+
  scale_x_discrete(labels = c(expression(T[0]), expression(T[1])))
plot_grid(d1,d2,d3, nrow = 1, labels = 'AUTO')

# a <- ggplot(aes(x = ft, y = d_change, fill = ft), data = canopy0714)+
#   geom_boxplot(show.legend = F, outlier.shape = NA)+ 
#   scale_fill_manual(values = c('#ffa600','#4d5c15','#998600'))+
#   ylab('Density change (%)')+ xlab('')+ 
#   geom_jitter (show.legend = F,alpha = 0.3,stroke = 0.5,size = 1.5 )+ 
#   geom_table(data = data.change, aes(x = 3.5, y = 2000, label = tb), size = 3, 
#              stat = "fmt_tb", tb.vars = c("Forest type" = "ft", 
#                                           "Mean change (%)" = "mean_change",
#                                           "Max change (%)" = "max_change", 
#                                           "Min change (%)" = "min_change"))+
#   theme_classic()
# 
# b <- ggplot(aes(x = ft, y = r_change, fill = ft), data = canopy0714)+
#   geom_boxplot(show.legend = F, outlier.shape = NA)+ 
#   scale_fill_manual(values = c('#ffa600','#4d5c15','#998600'))+
#   ylab('Richness change (%)')+ xlab('')+ 
#   geom_jitter (show.legend = F,alpha = 0.3,stroke = 0.5,size = 1.5 )+ 
#   geom_table(data = data.rchange, aes(x = 3.5, y = 700, label = tb), size = 3, 
#              stat = "fmt_tb", tb.vars = c("Forest type" = "ft", 
#                                           "Mean change (%)" = "mean_change",
#                                           "Max change (%)" = "max_change", 
#                                           "Min change (%)" = "min_change"))+
#   theme_classic()

t.test(canrf$density, canrf$density07, paired = TRUE, alternative = "two.sided") #ERF D
#t = -3.0269, df = 71, p-value = 0.003441
t.test(canaf$density, canaf$density07, paired = TRUE, alternative = "two.sided") #AF D
#t = 3.4139, df = 74, p-value = 0.001042
t.test(candf$density, candf$density07, paired = TRUE, alternative = "two.sided") #SF D
#t = 3.3902, df = 11, p-value = 0.006032

t.test(canrf$r, canrf$r07, paired = TRUE, alternative = "two.sided") #ERF r
#t = 1.8303, df = 71, p-value = 0.07141
t.test(canaf$r, canaf$r07, paired = TRUE, alternative = "two.sided") #AF r
#t = 5.9567, df = 74, p-value = 8.039e-08
t.test(candf$r, candf$r07, paired = TRUE, alternative = "two.sided") #SF r
#3.8075, df = 11, p-value = 0.002905

#HIPOTHESYS A - areas with higher canopy opening will have denser and taller regeneration,----
#               with higher representation of pioneer species 

#filter data frame
canrf <- filter(canopy0714, ft == 'RF')
canrf [is.na(canrf)] <- 0
canrf$altitudecat <- cut(canrf$altitude, c(0,100,600,2000), labels = c('Terras baixas', 'Submontana', 'Montana'))
summary(canrf$altitudecat) #split the altitude data into ecological formations

canaf <- filter(canopy0714, ft == 'AF')
canaf [is.na(canaf)] <- 0
canaf$altitudecat <- cut(canaf$altitude, c(0,30,400,1500,2000), labels = c('Aluvial','Submontana', 'Montana','Altomontana'))
summary(canaf$altitudecat) #split the altitude data into ecological formations

candf <- filter(canopy0714, ft == 'DF')
candf [is.na(candf)] <- 0
candf$altitudecat <- cut(candf$altitude, c(0,50,500,1500), labels = c('Terras baixas','Submontana', 'Montana'))
summary(candf$altitudecat) #split the altitude data into ecological formations

gc()

rm(canopy14)
rm(canopy0714)
rm(comp)
rm(fcover)
rm(species)
rm(change)
rm(data.change)
rm(changet)


#densidade
#rain forest
#total
glance(lm(density-density07 ~ mean_fcover, data = canrf)) #0.04 AIC 755
glance(lm(density-density07 ~ edge, data = canrf)) #0.05 AIC 755
glance(lm(density-density07 ~ mean_fcover*edge, data = canrf)) #0.05 AIC 755
 
m1 <-lm(density-density07 ~ mean_fcover, data = canrf)
cooksd <- cooks.distance(m1)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(canrf)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf1 <- canrf[-influential, ]
m1.2<-lm( density-density07 ~ mean_fcover,data=canrf1)
summary(m1.2)
plot(m1.2)
glance(m1.2)
tidy(m1.2)


#pioneer
glance(lm(density_P-density07_P ~ mean_fcover, data = canrf)) #0.99 AIC 553
glance(lm(density_P-density07_P ~ edge, data = canrf)) #0.11 AIC 550
glance(lm(density_P-density07_P ~ mean_fcover*edge, data = canrf)) #0.457 AIC 554
mrp <- lm(density_P-density07_P ~ edge, data = canrf)
summary(mrp)
#removing influential
cooksd <- cooks.distance(mrp)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf4 <- canrf[-influential, ]
mrp2<-lm( density_P-density07_P ~ edge,data=canrf4)
summary(mrp2)


#secondary
glance(lm(density_SE-density07_SE ~ mean_fcover*edge, data = canrf)) #0.02 AIC 625
glance(lm(density_SE-density07_SE ~ mean_fcover, data = canrf)) #0.01 #AIC 625
glance(lm(density_SE-density07_SE ~ edge, data = canrf)) #0.1 AIC 629
mrs <- lm(density_SE-density07_SE ~ mean_fcover, data = canrf)
summary(mrs)
#removing influential
cooksd <- cooks.distance(mrs)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf2 <- canrf[-influential, ]
mrs2<-lm( density_SE-density07_SE ~ mean_fcover, data=canrf2)
summary(mrs2)


#climax
glance(lm(density_C-density07_C ~ mean_fcover*edge, data = canrf)) #0.02 AIC 600
glance(lm(density_C-density07_C ~ mean_fcover, data = canrf)) #0.03 AIC 598
glance(lm(density_C-density07_C ~ edge, data = canrf)) #0.15 AIC 600
mrc <- lm(density_C-density07_C ~ mean_fcover, data = canrf)
summary(mrc)
#removing influential
cooksd <- cooks.distance(mrc)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf3 <- canrf[-influential, ]
mrc2<-lm(density_C-density07_C ~ mean_fcover, data = canrf3)
summary(mrc2)

# #anemo
# summary(lm(density_ane-density07_ane ~ poly(mean_fcover,2), data = canrf)) #NS
# summary(lm(density_ane-density07_ane ~ mean_fcover, data = canrf)) #0.03
# 
# #auto
# summary(lm(density_aut-density07_aut ~ poly(mean_fcover,2), data = canrf)) #NS
# summary(lm(density_aut-density07_aut ~ mean_fcover, data = canrf)) #NS
# 
# #zoo
# summary(lm(density_zoo-density07_zoo ~ poly(mean_fcover,2), data = canrf)) #NS
# summary(lm(density_zoo-density07_zoo ~ mean_fcover, data = canrf)) #0.05


gc()
#araucaria forest
#total
glance(lm(density-density07 ~ mean_fcover*edge, data = canaf)) #0.84 aic 729
glance(lm(density-density07 ~ mean_fcover, data = canaf)) #0.49 aic 725
glance(lm(density-density07 ~ edge, data = canaf)) #0.98 aic 726 
mat <- lm(density-density07 ~ mean_fcover, data = canaf)
summary(mat)
#removing influential
cooksd <- cooks.distance(mat)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf1 <- canaf[-influential, ]
mat1<-lm(density-density07 ~ mean_fcover, data = canaf1)
summary(mat1)


#pioneer
glance(lm(density_P-density07_P ~ mean_fcover*edge, data = canaf)) #0.47 aic 607
glance(lm(density_P-density07_P ~ mean_fcover, data = canaf)) #0.12 aic 603
glance(lm(density_P-density07_P ~ edge, data = canaf)) #0.84 aic 605
map <- lm(density_P-density07_P ~ mean_fcover, data = canaf)
summary(map)
#removing influential
cooksd <- cooks.distance(map)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf4 <- canaf[-influential, ]
map1<-lm(density_P-density07_P ~ mean_fcover, data = canaf4)
summary(map1)


#secondary
glance(lm(density_SE-density07_SE ~ mean_fcover*edge, data = canaf)) #0.41 aic 606
glance(lm(density_SE-density07_SE ~ mean_fcover, data = canaf)) #0.16 aic 603
glance(lm(density_SE-density07_SE ~ edge, data = canaf)) #0.96 aic 605
mas <- lm(density_SE-density07_SE ~ mean_fcover, data = canaf)
summary(mas)
#removing influential
cooksd <- cooks.distance(mas)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf2 <- canaf[-influential, ]
mas1<-lm(density_SE-density07_SE ~ mean_fcover, data = canaf2)
summary(mas1)


#climax
glance(lm(density_C-density07_C ~ mean_fcover*edge, data = canaf)) #0.12 aic 475
glance(lm(density_C-density07_C ~ mean_fcover, data = canaf)) #0.02 aic 472
glance(lm(density_C-density07_C ~ edge, data = canaf)) #0.57 aic 477
mac <- lm(density_C-density07_C ~ mean_fcover, data = canaf)
summary(mac)
#removing influential
cooksd <- cooks.distance(mac)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf3 <- canaf[-influential, ]
mac1<-lm(density_C-density07_C ~ mean_fcover, data = canaf3)
summary(mac1)


# #anemo
# summary(lm(density_ane-density07_ane ~ poly(mean_fcover,2), data = canaf)) #NS
# summary(lm(density_ane-density07_ane ~ mean_fcover, data = canaf)) #NS
# 
# #auto
# summary(lm(density_aut-density07_aut ~ poly(mean_fcover,2), data = canaf)) #NS
# summary(lm(density_aut-density07_aut ~ mean_fcover, data = canaf)) #NS
# 
# #zoo
# summary(lm(density_zoo-density07_zoo ~ poly(mean_fcover,2), data = canaf)) #NS
# summary(lm(density_zoo-density07_zoo ~ mean_fcover, data = canaf)) #NS


gc()

#deciduous forest
#total
glance(lm(density-density07 ~ mean_fcover*edge, data = candf)) #0.41 aic 121
glance(lm(density-density07 ~ mean_fcover, data = candf)) #0.85 aic 121
glance(lm(density-density07 ~ edge, data = candf)) #0.34 aic 120
mdt <- lm(density-density07 ~ mean_fcover, data = candf)
summary(mdt)
#removing influential
cooksd <- cooks.distance(mdt)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf1 <- candf[-influential, ]
mdt1<-lm(density-density07 ~ mean_fcover, data = candf1)
summary(mdt1)

#pioneer
glance(lm(density_P-density07_P ~ mean_fcover*edge, data = candf)) #0.001 aic 73,5
glance(lm(density_P-density07_P ~ mean_fcover, data = candf)) #0.5 aic 90,2
glance(lm(density_P-density07_P ~ edge, data = candf)) #0.98 aic 90,8
mdp <- lm(density_P-density07_P ~ mean_fcover*edge, data = candf)
summary(mdp)
#removing influential
cooksd <- cooks.distance(mdp)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf4 <- candf[-influential, ]
mdp1<-lm(density_P-density07_P ~ mean_fcover*edge, data = candf4)
summary(mdp1)


#secondary
glance(lm(density_SE-density07_SE ~ mean_fcover*edge, data = candf)) #0.38 aic 106
glance(lm(density_SE-density07_SE ~ mean_fcover, data = candf)) #0.92 aic 106
glance(lm(density_SE-density07_SE ~ edge, data = candf)) #0.08 aic 102
mds <- lm(density_SE-density07_SE ~ edge, data = candf)
summary(mds)
#removing influential
cooksd <- cooks.distance(mds)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf2 <- candf[-influential, ]
mds1<-lm(density_SE-density07_SE ~ edge, data = candf2)
summary(mds1)


#climax
glance(lm(density_C-density07_C ~ mean_fcover*edge, data = candf)) #0.21 aic 91,3
glance(lm(density_C-density07_C ~ mean_fcover, data = candf)) #0.04 aic 88,2
glance(lm(density_C-density07_C ~ edge, data = candf)) #0.81 aic 93,6
mdc <- lm(density_C-density07_C ~ mean_fcover, data = candf)
summary(mdc)
#removing influential
cooksd <- cooks.distance(mdc)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf3 <- candf[-influential, ]
mdc1<-lm(density_C-density07_C ~ mean_fcover, data = candf3)
summary(mdc1)


#% de pioneiras
#rain forest
glance(lm(density_P/density ~ mean_fcover*edge, data = canrf)) #NS
glance(lm(density_P/density ~ mean_fcover, data = canrf)) #NS
glance(lm(density_P/density ~ edge, data = canrf)) #NS
pr <- lm(density_P/density ~ mean_fcover, data = canrf)
summary(pr)
#removing influential
cooksd <- cooks.distance(pr)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf1 <- canrf[-influential, ]
pr1<-lm(density_P/density ~ mean_fcover, data = canrf1)
summary(pr1)
#rm(canrf1)

#araucaria forest
glance(lm(density_P/density ~ mean_fcover*edge, data = canaf)) #NS
glance(lm(density_P/density ~ mean_fcover, data = canaf)) #NS
glance(lm(density_P/density ~ edge, data = canaf)) #NS
pa <- lm(density_P/density ~ mean_fcover, data = canaf)
summary(pa)
#removing influential
cooksd <- cooks.distance(pa)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf2 <- canaf[-influential, ]
pa1<-lm(density_P/density ~ mean_fcover, data = canaf2)
summary(pa1)
#rm(canaf2)

#deciduous forest
glance(lm(density_P/density ~ mean_fcover*edge, data = candf)) #NS
glance(lm(density_P/density ~ mean_fcover, data = candf)) #NS
glance(lm(density_P/density ~ edge, data = candf)) #NS
pd <- lm(density_P/density ~ mean_fcover*edge, data = candf)
summary(pd)
#removing influential
cooksd <- cooks.distance(pd)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf1 <- candf[-influential, ]
pd1<-lm(density_P/density ~ mean_fcover*edge, data = candf1)
summary(pd1)

#graphs
#edge
er <- ggplot(data = canrf1)+
  geom_point(aes(x = edge, y = density - density07),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  # geom_smooth(aes(x = edge, y = density - density07), method = 'lm',se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))+  
  labs(title = 'Evergreen Rain Forest')
  #annotate(geom='text',x=50,y=50,size=5,label=paste('Total'))#+ facet_wrap(~edgecat)

erp <-ggplot(data = canrf4)+
  #geom_smooth(aes(x = mean_fcover, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_P - density07_P),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
   theme_classic() + labs (title = 'Pioneer', x = '', y = '') 
   
  
ers <-ggplot(data = canrf2)+
  geom_point(aes(x = edge, y = density_SE - density07_SE),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  # geom_smooth(aes(x = mean_fcover, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')

erc <-ggplot(data = canrf3)+
  geom_point(aes(x = edge, y = density_C - density07_C),shape = 21, 
             fill ='#4d5c15',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
  # geom_smooth(aes(x = mean_fcover, y = density_C - density07_C), 
  #             method = 'lm',se = F, color = 'black') +
  theme_classic()+ labs (title = 'Climax', x = '', y = '') 

row_er <- plot_grid (erp,ers,erc, nrow = 1)
edg_rain <- plot_grid (er, row_er, nrow = 2, rel_heights = c(2,1))

ea <- ggplot(data = canaf1)+
  #geom_smooth(aes(x = mean_fcover, y = density - density07), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density - density07),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  xlab('Distance to edge (m)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))  +
  labs(title = 'Araucaria Forest')
  
  
eap <-ggplot(data = canaf4)+
  #geom_smooth(aes(x = mean_fcover, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_P - density07_P),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Pioneer', x = '', y = '')
  

eas <-ggplot(data = canaf2)+
  #geom_smooth(aes(x = mean_fcover, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_SE - density07_SE),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')
  
  
eac <-ggplot(data = canaf3)+
  # geom_smooth(aes(x = edge, y = density_C - density07_C), 
  #             method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_C - density07_C),shape = 21, 
             fill ='#ffa600',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
 theme_classic()+ labs (title = 'Climax', x = '', y = '')
   
  
row_ea <- plot_grid (eap,eas,eac, nrow = 1)
edg_ara <- plot_grid (ea, row_ea, nrow = 2, rel_heights = c(2,1))

ed <- ggplot(data = candf)+
  #geom_smooth(aes(x = mean_fcover, y = density - density07), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density - density07),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  xlab('Distance to edge (m)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))  +
  labs(title = 'Semi-Deciduous Forest')


edp <-ggplot(data = candf4)+
  geom_smooth(aes(x = edge, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_P - density07_P),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Pioneer', x = '', y = '')
  

eds <-ggplot(data = candf2)+
  geom_smooth(aes(x = edge, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_SE - density07_SE),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')
   

edc <-ggplot(data = candf3)+
  # geom_smooth(aes(x = mean_fcover, y = density_C - density07_C), 
  #            method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = edge, y = density_C - density07_C),shape = 21, 
             fill ='#998600',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Climax', x = '', y = '')
 

row_ed <- plot_grid (edp,eds,edc, nrow = 1)
edg_dec <- plot_grid (ed, row_ed, nrow = 2, rel_heights = c(2,1))

row_edge <- plot_grid (edg_rain, edg_ara, edg_dec, nrow = 1, labels = c('D', 'E', 'F'))

#fcover
dr <- ggplot(data = canrf1)+
  geom_point(aes(x = mean_fcover, y = density - density07),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  # geom_smooth(aes(x = mean_fcover, y = density - density07), method = 'lm',se = F, color = 'black') +
  xlab('FCover (%)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))+  
  labs(title = 'Evergreen Rain Forest')
#annotate(geom='text',x=50,y=50,size=5,label=paste('Total'))#+ facet_wrap(~edgecat)

drp <-ggplot(data = canrf4)+
  #geom_smooth(aes(x = mean_fcover, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_P - density07_P),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic() + labs (title = 'Pioneer', x = '', y = '') 


drs <-ggplot(data = canrf2)+
  geom_point(aes(x = mean_fcover, y = density_SE - density07_SE),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  geom_smooth(aes(x = mean_fcover, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')

drc <-ggplot(data = canrf3)+
  geom_point(aes(x = mean_fcover, y = density_C - density07_C),shape = 21, 
             fill ='#4d5c15',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
  geom_smooth(aes(x = mean_fcover, y = density_C - density07_C), 
              method = 'lm',se = F, color = 'black') +
  theme_classic()+ labs (title = 'Climax', x = '', y = '') 

row_dr <- plot_grid (drp,drs,drc, nrow = 1)
den_rain <- plot_grid (dr, row_dr, nrow = 2, rel_heights = c(2,1))

da <- ggplot(data = canaf1)+
  #geom_smooth(aes(x = mean_fcover, y = density - density07), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density - density07),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  xlab('FCover (%)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))  +
  labs(title = 'Araucaria Forest')


dap <-ggplot(data = canaf4)+
  #geom_smooth(aes(x = mean_fcover, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_P - density07_P),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Pioneer', x = '', y = '')


das <-ggplot(data = canaf2)+
  #geom_smooth(aes(x = mean_fcover, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_SE - density07_SE),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')


dac <-ggplot(data = canaf3)+
  geom_smooth(aes(x = mean_fcover, y = density_C - density07_C), 
              method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_C - density07_C),shape = 21, 
             fill ='#ffa600',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Climax', x = '', y = '')


row_da <- plot_grid (dap,das,dac, nrow = 1)
den_ara <- plot_grid (da, row_da, nrow = 2, rel_heights = c(2,1))

dd <- ggplot(data = candf)+
  #geom_smooth(aes(x = mean_fcover, y = density - density07), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density - density07),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.7,stroke = 1.5,size = 3.0)+
  xlab('FCover (%)') + theme_classic()+
  ylab(bquote("Density change"~(n.100m^-2)))  +
  labs(title = 'Semi-Deciduous Forest')


ddp <-ggplot(data = candf4)+
  geom_smooth(aes(x = mean_fcover, y = density_P - density07_P), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_P - density07_P),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Pioneer', x = '', y = '')


dds <-ggplot(data = candf2)+
  #geom_smooth(aes(x = mean_fcover, y = density_SE - density07_SE), method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_SE - density07_SE),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Secondary', x = '', y = '')


ddc <-ggplot(data = candf3)+
  geom_smooth(aes(x = mean_fcover, y = density_C - density07_C), 
              method = 'lm',se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = density_C - density07_C),shape = 21, 
             fill ='#998600',color = 'black', alpha = 0.5,stroke = 1,size = 2)+
  theme_classic()+ labs (title = 'Climax', x = '', y = '')


row_dd <- plot_grid (ddp,dds,ddc, nrow = 1)
den_dec <- plot_grid (dd, row_dd, nrow = 2, rel_heights = c(2,1))

row_den <- plot_grid (den_rain, den_ara, den_dec, nrow = 1, labels = c('A', 'B', 'C'))

plot_grid (row_den, row_edge, nrow = 2)

#Dsquared function for glm ----
#create the Dsquared function
#then you can afterwards run your model Mglm1<-glm...
#and then use Dsquared(Mglm1)
Dsquared <- function(model = NULL,
                     obs = NULL,
                     pred = NULL,
                     family = NULL, # needed only when 'model' not provided
                     adjust = FALSE,
                     npar = NULL) { # needed only when 'model' not provided
  # version 1.4 (31 Aug 2015)
  model.provided <- ifelse(is.null(model), FALSE, TRUE)
  if (model.provided) {
    if (!("glm" %in% class(model))) stop ("'model' must be of class 'glm'.")
    if (!is.null(pred)) message("Argument 'pred' ignored in favour of 'model'.")
    if (!is.null(obs)) message("Argument 'obs' ignored in favour of 'model'.")
    obs <- model$y
    pred <- model$fitted.values
    
  } else { # if model not provided
    if (is.null(obs) | is.null(pred)) stop ("You must provide either 'obs' and 'pred', or a 'model' object of class 'glm'.")
    if (length(obs) != length(pred)) stop ("'obs' and 'pred' must be of the same length (and in the same order).")
    if (is.null(family)) stop ("With 'obs' and 'pred' arguments (rather than a model object), you must also specify one of two model family options: 'binomial' or 'poisson' (in quotes).")
    else if (!is.character(family)) stop ("Argument 'family' must be provided as character (i.e. in quotes: 'binomial' or 'poisson').")
    else if (length(family) != 1 | !(family %in% c("binomial", "poisson"))) stop ("'family' must be either 'binomial' or 'poisson' (in quotes).")
    
    if (family == "binomial") {
      if (any(!(obs %in% c(0, 1)) | pred < 0 | pred > 1)) stop ("'binomial' family implies that 'obs' data should be binary (with values 0 or 1) and 'pred' data should be bounded between 0 and 1.")
      link <- log(pred / (1 - pred))  # logit
    }  # end if binomial
    
    else if (family == "poisson") {
      if (any(obs %%1 != 0)) stop ("'poisson' family implies that 'obs' data should consist of whole numbers.")
      link <- log(pred)
      
    }  # end if poisson
    
    model <- glm(obs ~ link, family = family)
    
  }  # end if model not provided
  
  D2 <- (model$null.deviance - model$deviance) / model$null.deviance
  if (adjust) {
    if (model.provided) {
      n <- length(model$y)
      #p <- length(model$coefficients)
      p <- attributes(logLik(model))$df
    } else {
      if (is.null(npar)) stop ("Adjusted D-squared from 'obs' and 'pred' values (rather than a model object) requires specifying the number of parameters in the underlying model ('npar').")
      n <- length(na.omit(obs))
      p <- npar
    }  # end if model.provided else
    
    D2 <- 1 - ((n - 1) / (n - p)) * (1 - D2)
  }  # end if adjust
  return (D2)
}


#HYPOTHESYS B - areas with higher canopy cover variability will have higher species diversity----

#rain forest
glance(glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = canrf))
glance(glm (r ~ sd_fcover + mean_fcover, family = 'poisson', data = canrf))

sr <- glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = canrf)
#removing influential
cooksd <- cooks.distance(sr)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf10 <- canrf[-influential, ]
sr2<-glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = canrf10)
summary(sr2)
Dsquared(sr2)
plot(sr2)

# srl <- lm(r ~ sd_fcover * mean_fcover, data = canrf) #model to plot interactions
# summary(srl) #0.05
# pr<-ggPredict(srl,se=T,interactive=TRUE) #plot interactions

glance(lm (r_P/r ~ sd_fcover * edge + mean_fcover * edge, data = canrf)) #0.83 aic -243
glance(lm (r_P/r ~ sd_fcover + mean_fcover, data = canrf)) #0.60 aic -246
glance(lm (r_P/r ~ edge, data = canrf)) #0.17 aic -249

srp <- lm (r_P/r ~ edge, data = canrf) #check relative pioneers richness
summary(srp) #NS
#removing influential
cooksd <- cooks.distance(srp)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf11 <- canrf[-influential, ]
srp1<-lm (r_P/r ~ edge, data = canrf11)
summary(srp1)
rm(canrf11)

glance(lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = canrf)) #0.001 aic -186
glance(lm (r_SE/r ~ sd_fcover + mean_fcover, data = canrf)) #0.007 aic -181
glance(lm (r_SE/r ~ edge, data = canrf)) #0.002 aic -183
srs <- lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = canrf)#check relative secondary richness
summary(srs) #0.02
#removing influential
cooksd <- cooks.distance(srs)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf12 <- canrf[-influential, ]
srs1<-lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = canrf12)
summary(srs1)

glance(lm (r_C/r ~ sd_fcover * edge + mean_fcover * edge, data = canrf)) #0.003 aic -176
glance(lm (r_C/r ~ sd_fcover + mean_fcover, data = canrf)) #0.001 aic -176
glance(lm (r_C/r ~ edge, data = canrf)) #0.05 aic -169
src <- lm (r_C/r ~ sd_fcover + mean_fcover, data = canrf) #check relative climax richness
summary(src) #0.003 
#removing influential
cooksd <- cooks.distance(src)
sample_size <- nrow(canrf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canrf13 <- canrf[-influential, ]
src1<-lm (r_C/r ~ sd_fcover + mean_fcover, data = canrf13)
summary(src1)
rm(canrf13)


#araucaria forest
glance(glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = canaf))
glance(glm (r ~ sd_fcover + mean_fcover, family = 'poisson', data = canaf))
sa <- glm (r ~ sd_fcover * edge + mean_fcover * edge, family = 'poisson', data = canaf)
summary(sa)
plot(sa)
Dsquared(sa)

cooksd <- cooks.distance(sa)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(canaf)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf10 <- canaf[-influential, ]
sa2<-glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = canaf10)
summary(sa2)
Dsquared(sa2)
plot(sa2)


sal <- lm(r ~ sd_fcover * edge + mean_fcover * edge, data = canaf)
summary(sal) #0.008
ar<-ggPredict(sal,se=T,interactive=TRUE)

glance(lm (r_P/r ~ sd_fcover * edge + mean_fcover * edge, data = canaf)) #0.0003 aic -92,8
glance(lm (r_P/r ~ sd_fcover + mean_fcover, data = canaf)) #0.000006 aic -94,1
glance(lm (r_P/r ~ edge, data = canaf)) #0.11 aic -78,5
sap <- lm (r_P/r ~ sd_fcover + mean_fcover, data = canaf) #check relative pioneers richness
summary(sap) #0.0002
#removing influential
cooksd <- cooks.distance(sap)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf11 <- canaf[-influential, ]
sap1<-lm (r_P/r ~ sd_fcover + mean_fcover, data = canaf11)
summary(sap1)


glance(lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = canaf)) #0.03 aic -92,8
glance(lm (r_SE/r ~ sd_fcover + mean_fcover, data = canaf)) #0.02 aic -93,6
glance(lm (r_SE/r ~ edge, data = canaf)) #0.09 aic -90,2
sas <- lm (r_SE/r ~ sd_fcover + mean_fcover, data = canaf)#check relative secondary richness
summary(sas) #0.02
#removing influential
cooksd <- cooks.distance(sas)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf12 <- canaf[-influential, ]
sas1<-lm (r_SE/r ~ sd_fcover + mean_fcover, data = canaf12)
summary(sas1)

glance(lm (r_C/r ~ sd_fcover * edge + mean_fcover * edge, data = canaf)) #0.05 aic -147
glance(lm (r_C/r ~ sd_fcover + mean_fcover, data = canaf)) #0.007 aic -152
glance(lm (r_C/r ~ edge, data = canaf)) #0.009 aic -143
sac <- lm (r_C/r ~ sd_fcover + mean_fcover, data = canaf) #check relative climax richness
summary(sac) #0.02 
#removing influential
cooksd <- cooks.distance(sac)
sample_size <- nrow(canaf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
canaf13 <- canaf[-influential, ]
sac1<-lm (r_C/r ~ sd_fcover * edge + mean_fcover * edge, data = canaf13)
summary(sac1)


#deciduous forest
glance(glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = candf))
glance(glm (r ~ sd_fcover + mean_fcover, family = 'poisson', data = candf))
sd <- glm (r ~ sd_fcover * edge + mean_fcover * edge, family = 'poisson', data = candf)
summary(sd)
Dsquared(sd)

cooksd <- cooks.distance(sd)
# Plot the Cook's Distance using the traditional 4/n criterion
sample_size <- nrow(candf)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 4/sample_size, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4/sample_size, names(cooksd),""), col="red")  # add labels
# Removing Outliers
# influential row numbers
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf10 <- candf[-influential, ]
sd2<-glm (r ~ sd_fcover * edge + mean_fcover * edge , family = 'poisson', data = candf10)
summary(sd2)
Dsquared(sd2)
plot(sa2)

glance(lm (r_P/r ~ sd_fcover * edge + mean_fcover * edge, data = candf)) #0.05 aic -23,7
glance(lm (r_P/r ~ sd_fcover + mean_fcover, data = candf)) #0.61 aic -12,8
glance(lm (r_P/r ~ edge, data = candf)) #0.24 aic -15,3
sdp <- lm (r_P/r ~ sd_fcover * edge + mean_fcover * edge, data = candf) #check relative pioneers richness
summary(sdp) #NS
#removing influential
cooksd <- cooks.distance(sdp)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf11 <- candf[-influential, ]
sdp1<-lm (r_P/r ~ sd_fcover * edge + mean_fcover * edge, data = candf11)
summary(sdp1)

glance(lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = candf)) #0.19 aic -13
glance(lm (r_SE/r ~ sd_fcover + mean_fcover, data = candf)) #0.81 aic-7,35
glance(lm (r_SE/r ~ edge, data = candf)) #0.52 aic -9,3
sds <- lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = candf)#check relative secondary richness
summary(sds) #NS
#removing influential
cooksd <- cooks.distance(sds)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf12 <- candf[-influential, ]
sds1<-lm (r_SE/r ~ sd_fcover * edge + mean_fcover * edge, data = candf12)
summary(sds1)

glance(lm (r_C/r ~ sd_fcover * edge + mean_fcover * edge, data = candf)) #0.39 aic -15,8
glance(lm (r_C/r ~ sd_fcover + mean_fcover, data = candf)) #0.34 aic -21,3
glance(lm (r_C/r ~ edge, data = candf)) #0.66 aic -20,7
sdc <- lm (r_C/r ~ sd_fcover + mean_fcover, data = candf) #check relative climax richness
summary(sdc) #NS
#removing influential
cooksd <- cooks.distance(sdc)
sample_size <- nrow(candf)
influential <- as.numeric(names(cooksd)[(cooksd > (4/sample_size))])
candf13 <- candf[-influential, ]
sdc1<-lm (r_C/r ~ sd_fcover + mean_fcover, data = candf13)
summary(sdc1)


sdan <- lm (r_ane/r ~ sd_fcover * mean_fcover, data = candf) #check relative anemochoric richness
summary(sdan) #NS
sdau <- lm (r_aut/r ~ sd_fcover * mean_fcover, data = candf) #check relative autochoric richness
summary(sdau) #NS
sdzo <- lm (r_zoo/r ~ sd_fcover * mean_fcover, data = candf) #check relative zoochoric richness
summary(sdzo) #0.05

#GRAPHS H2 ####
#mean_fcover RF
ra <- ggplot(data = canrf)+
   geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
               se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

rp <- ggplot(data = canrf)+
  #geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
  #             se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r_P/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

rs <- ggplot(data = canrf)+
  geom_point(aes(x = mean_fcover, y = r_SE/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = mean_fcover, y = r_SE/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

rc <- ggplot(data = canrf)+
   geom_point(aes(x = mean_fcover, y = r_C/r*100),shape = 21, fill ='#4d5c15',color = 'black',
              alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = mean_fcover, y = r_C/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

rf1 <- plot_grid (ra,rp,rs,rc, nrow = 2)

title1 <- ggdraw() + 
  draw_label("Evergreen Rain Forest", fontface = 'bold', x = 0, hjust = 0) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 105)
  )

title2 <- ggdraw() + 
  draw_label("FCover mean (%)", fontface = 'bold', x = 0, hjust = 0, angle = 90) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, -100, 7)
  )

rain_fcover <- plot_grid (NULL,title1, title2,rf1, ncol = 2, rel_heights = c(0.1,1),rel_widths = c(0.1,1))

#mean_fcover AF
aa <- ggplot(data = canaf)+
  geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

ap <- ggplot(data = canaf)+
  geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r_P/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

as <- ggplot(data = canaf)+
  geom_point(aes(x = mean_fcover, y = r_SE/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = mean_fcover, y = r_SE/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

ac <- ggplot(data = canaf)+
  geom_point(aes(x = mean_fcover, y = r_C/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = mean_fcover, y = r_C/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

af1 <- plot_grid (aa,ap,as,ac, nrow = 2)

title1 <- ggdraw() + 
  draw_label("Araucaria Forest", fontface = 'bold', x = 0, hjust = 0) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 105)
  )

ara_fcover <- plot_grid (title1,af1, ncol = 1, rel_heights = c(0.1,1))

#mean_fcover SF
da <- ggplot(data = candf)+
  geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

dp <- ggplot(data = candf)+
  geom_smooth(aes(x = mean_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = mean_fcover, y = r_P/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

ds <- ggplot(data = candf)+
  geom_point(aes(x = mean_fcover, y = r_SE/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = mean_fcover, y = r_SE/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

dc <- ggplot(data = candf)+
  geom_point(aes(x = mean_fcover, y = r_C/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = mean_fcover, y = r_C/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('FCover mean (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

df1 <- plot_grid (da,dp,ds,dc, nrow = 2)

title1 <- ggdraw() + 
  draw_label("Semi-Deciduous Forest", fontface = 'bold', x = 0, hjust = 0) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, 0, 105)
  )

dec_fcover <- plot_grid (title1,df1, ncol = 1, rel_heights = c(0.1,1))

fcover_line <- plot_grid(rain_fcover,ara_fcover,dec_fcover, nrow = 1)


#SD_fcover RF
ra <- ggplot(data = canrf)+
  geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

rp <- ggplot(data = canrf)+
  #geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
  #             se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r_P/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

rs <- ggplot(data = canrf)+
  geom_point(aes(x = sd_fcover, y = r_SE/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = sd_fcover, y = r_SE/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

rc <- ggplot(data = canrf)+
  geom_point(aes(x = sd_fcover, y = r_C/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = sd_fcover, y = r_C/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

rf2 <- plot_grid (ra,rp,rs,rc, nrow = 2)

title2 <- ggdraw() + 
  draw_label("FCover SD (%)", fontface = 'bold', x = 0, hjust = 0, angle = 90) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, -100, 7)
  )

rain_sdfcover <- plot_grid (title2,rf2, ncol = 2, rel_heights = c(0.1,1),rel_widths = c(0.1,1))

#SD_fcover AF
aa <- ggplot(data = canaf)+
  geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

ap <- ggplot(data = canaf)+
  geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r_P/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

as <- ggplot(data = canaf)+
  geom_point(aes(x = sd_fcover, y = r_SE/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = sd_fcover, y = r_SE/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

ac <- ggplot(data = canaf)+
  geom_point(aes(x = sd_fcover, y = r_C/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = sd_fcover, y = r_C/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

af2 <- plot_grid (aa,ap,as,ac, nrow = 2)

ara_sdfcover <- plot_grid (af2, ncol = 1)

#SD_fcover SF
da <- ggplot(data = candf)+
  geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

dp <- ggplot(data = candf)+
  geom_smooth(aes(x = sd_fcover, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = sd_fcover, y = r_P/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

ds <- ggplot(data = candf)+
  geom_point(aes(x = sd_fcover, y = r_SE/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = sd_fcover, y = r_SE/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

dc <- ggplot(data = candf)+
  geom_point(aes(x = sd_fcover, y = r_C/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = sd_fcover, y = r_C/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('FCover SD (%)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

df2 <- plot_grid (da,dp,ds,dc, nrow = 2)

dec_sdfcover <- plot_grid (df2, ncol = 1)

sdfcover_line <- plot_grid(rain_sdfcover,ara_sdfcover,dec_sdfcover, nrow = 1)


#DISTANCE TO EDGE RF
ra <- ggplot(data = canrf)+
  geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = edge, y = r),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

rp <- ggplot(data = canrf)+
  #geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
  #             se = F, color = 'black') +
  geom_point(aes(x = edge, y = r_P/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

rs <- ggplot(data = canrf)+
  geom_point(aes(x = edge, y = r_SE/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  geom_smooth(aes(x = edge, y = r_SE/r*100), method = 'lm',
              se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

rc <- ggplot(data = canrf)+
  geom_point(aes(x = edge, y = r_C/r*100),shape = 21, fill ='#4d5c15',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = edge, y = r_C/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

rf3 <- plot_grid (ra,rp,rs,rc, nrow = 2)

title2 <- ggdraw() + 
  draw_label("Distance to edge (m)", fontface = 'bold', x = 0, hjust = 0, angle = 90) +
  theme(
    # add margin on the left of the drawing canvas,
    # so title is aligned with left edge of first plot
    plot.margin = margin(0, 0, -100, 7)
  )

rain_edge <- plot_grid (title2,rf3, ncol = 2, rel_widths = c(0.1,1))

#DISTANCE TO EDGE AF
aa <- ggplot(data = canaf)+
  geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = edge, y = r),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('Distance to edge (m)') + theme_classic()+theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

ap <- ggplot(data = canaf)+
  # geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
  #             se = F, color = 'black') +
  geom_point(aes(x = edge, y = r_P/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

as <- ggplot(data = canaf)+
  geom_point(aes(x = edge, y = r_SE/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = edge, y = r_SE/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

ac <- ggplot(data = canaf)+
  geom_point(aes(x = edge, y = r_C/r*100),shape = 21, fill ='#ffa600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = edge, y = r_C/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

af3 <- plot_grid (aa,ap,as,ac, nrow = 2)

ara_edge <- plot_grid (af3, ncol = 1)

#DISTANCE TO EDGE SF
da <- ggplot(data = candf)+
  geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = edge, y = r),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2)+
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Species Richness (n)')

dp <- ggplot(data = candf)+
  geom_smooth(aes(x = edge, y = r), method = 'glm',method.args = list(family = "poisson"),
              se = F, color = 'black') +
  geom_point(aes(x = edge, y = r_P/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Pioneer (%)')

ds <- ggplot(data = candf)+
  geom_point(aes(x = edge, y = r_SE/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = edge, y = r_SE/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Secondary (%)') 

dc <- ggplot(data = candf)+
  geom_point(aes(x = edge, y = r_C/r*100),shape = 21, fill ='#998600',color = 'black',
             alpha = 0.5,stroke = 1,size = 2.0)+
  # geom_smooth(aes(x = edge, y = r_C/r*100), method = 'lm',
  #             se = F, color = 'black') +
  xlab('Distance to edge (m)') + theme_classic()+#theme (axis.text.x = element_blank(),axis.text.y = element_blank())+
  ylab('Climax (%)')  

df3 <- plot_grid (da,dp,ds,dc, nrow = 2)

dec_edge <- plot_grid (df3, ncol = 1)

edge_line <- plot_grid(rain_edge,ara_edge,dec_edge, nrow = 1)

all_lines <- plot_grid(fcover_line, sdfcover_line, edge_line, ncol = 1)

#espécies
canopy0714$ft <- revalue(canopy0714$ft, c('AF' = 'Araucaria Forest', 'DF' = 'Deciduous Forest', 'RF' = 'Rain Forest')) #rename variables
sp <- ggplot(canopy0714)+
  geom_boxplot(aes(x = ft, y = r - r07, fill = ft),show.legend = F)+
  scale_fill_manual(values = c('#ffa600','#998600','#4d5c15'))+
  geom_jitter(position=position_jitter(0.2), alpha = 0.4, size = 4, aes(x = ft, y = r - r07))+
  geom_hline(yintercept = 0)+theme_classic()+
  labs(x = '', y = 'RIchness Change')+coord_flip()
  

#HYPOTHESYS C - areas with higher densities of conspecific saplings in the first ----
#measurement t0 will have a decrease in density in the second measurement t1 (~5 years).

#creating a list of species with average densities, ge, ds and lf
#rf
sp_list_rf <- species_rf %>% 
  group_by (sp) %>% 
  summarise (d07 = mean (density07),
             d14 = mean (density14),
             ge = first (ge),
             ds = first (ds),
             lf = first (lf)) %>% 
  mutate (mean_change = d14-d07) 

#af
sp_list_af <- species_af %>% 
  group_by (sp) %>% 
  summarise (d07 = mean (density07),
             d14 = mean (density14),
             ge = first (ge),
             ds = first (ds),
             lf = first (lf)) %>% 
  mutate (mean_change = d14-d07) 

#df
sp_list_df <- species_df %>% 
  group_by (sp) %>% 
  summarise (d07 = mean (density07),
             d14 = mean (density14),
             ge = first (ge),
             ds = first (ds),
             lf = first (lf)) %>% 
  mutate (mean_change = d14-d07) 


#H3 RAIN FOREST----
#model results rain forest
species_rf %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('rf_results_lm.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates rain forest
species_rf %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('rf_estimates_lm.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_rf <- read.csv("rf_results_lm.csv")
results_rf <- select(results_rf, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_rf <- read.csv('rf_estimates_lm.csv')
estimates_rf <- estimates_rf %>% select(sp, estimate, std.error, p.value) %>% 
                           rename(est.p = p.value)

cons_result_rf <- full_join(sp_list_rf,results_rf) #joins the results to the species list
cons_result_rf <- full_join(cons_result_rf, estimates_rf) #joins the lm results with the estimates values
cons_result_rf$significance <- ifelse(cons_result_rf$p.value <= 0.05,'p < 0.05','p > 0.05') #creates a column with 1 for the lm significance p<0.05
cons_result_rf$significance[is.na(cons_result_rf$significance)] <- 'p > 0.05' #replaces NA with 0
cons_result_rf$significance<-as.factor(cons_result_rf$significance) #makes the values factors 
cons_result_rf$population <- ifelse(cons_result_rf$mean_change <= 0, 'Decrease', 'Increase') #create column with population increase
cons_result_rf$population <- as.factor(cons_result_rf$population) #makes the values factors
cons_result_rf$ds<-as.factor(cons_result_rf$ds)
#filter only taxa identified at species level
cons_result_rfsp<- cons_result_rf %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

cons_result_rf10 <- cons_result_rfsp %>% filter(nobs >= 10) #filter only taxas with 10+ occurances
write_csv(cons_result_rf10,'fnal_H3_rf.csv') #write the final dataframe

ggplot(cons_result_rf10, aes(x = estimate, y = sp))+ #makes the estimates graph
  geom_point(alpha = 0.7, aes(size = d07, colour = significance, shape = population))+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  geom_vline(xintercept = 0)+
  labs(color = 'Significance', size = bquote("Conspecific \n density at"~T[0]), shape = 'Population')+
  labs(title = 'Evergreen Rain Forest') +ylab('Estimate')+ xlab('Species')+ theme_classic()

#organizing the ecological group column
cons_result_rf10$ge <- revalue(cons_result_rf10$ge, c('P' = 'Pioneer', 'SE' = 'Secondary', 'C' = 'Climax', 'NC' = 'Not Classified'))
GE <- factor(cons_result_rf10$ge, levels = c('Pioneer','Secondary', 'Climax', 'Not Classified'),ordered = T)


#BOXPLOT FOR ECOLOGICAL GROUP
sr <- ggplot(aes(x = GE, y = estimate), data = cons_result_rf10)+ 
  #geom_violin(show.legend = F,color = 'black', fill = 'grey') + 
  #scale_fill_manual(values = c('#ffa600','#b99200','#7d7909','#4d5c15'))+
  ylab('Estimate')+ xlab('Ecological Group')+ labs(title = 'Evergreen Rain Forest')+
  geom_jitter(position=position_jitter(0.3),alpha = 0.5,
              aes(size = d07, color = significance, shape = population)) + 
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  labs(color = 'Significance', size = bquote("Conspecific \n density at"~T[0]), shape = 'Population')+
  theme_classic()+ coord_flip() #+ 
  # annotate('text',label = 'Myrcia neotomentosa', x=2.97, y=0.9,size = 2.2)+
  # annotate('text',label = 'Protium kleinii', x=2.03, y=1.5,size = 2.2)


# ANOVA pra ver o efeito dividido por grupo ecologico
cons_result_rf10 %>% group_by(ge) %>% 
  summarise(estimate = mean(estimate))
# ge    estimate
# 1 C       -0.629
# 2 NC      -0.837
# 3 P       -0.880
# 4 SE      NA 

aov_ge <- aov(estimate ~ ge, data = cons_result_rf10)
summary(aov_ge)
TukeyHSD(aov_ge)

# ANOVA pra ver o efeito dividido por sindrome de dispersão
cons_result_rf10 %>% group_by(ds) %>% 
  summarise(estimate = mean(estimate))
# ds                       estimate
# 2 anemochoric                -0.847
# 3 anemochoric | autochoric   -0.681
# 4 autochoric                 -0.363
# 5 zoochoric                  -0.715

aov_ds <- aov(estimate ~ ds, data = cons_result_rf10)
summary(aov_ds)
TukeyHSD(aov_ds)
  
#H3 ARAUCARIA FOREST----
#model results rain forest
species_af %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('af_results_lm.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates rain forest
species_af %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('af_estimates_lm.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_af <- read.csv("af_results_lm.csv",sep=";")
results_af <- select(results_af, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_af <- read.csv('af_estimates_lm.csv',sep=",")
estimates_af <- estimates_af %>% select(sp, estimate, std.error, p.value) %>% 
  rename(est.p = p.value)

cons_result_af <- full_join(sp_list_af,results_af) #joins the results to the species list
cons_result_af <- full_join(cons_result_af, estimates_af) #joins the lm results with the estimates values
cons_result_af$significance <- ifelse(cons_result_af$p.value <= 0.05,'p < 0.05','p > 0.05') #creates a column with 1 for the lm significance p<0.05
cons_result_af$significance[is.na(cons_result_af$significance)] <- 0 #replaces NA with 0
cons_result_af$significance<-as.factor(cons_result_af$significance) #makes the values factors 
cons_result_af$population <- ifelse(cons_result_af$mean_change <= 0, 'Decrease', 'Increase') #create column with population increase
cons_result_af$population <- as.factor(cons_result_af$population) #makes the values factors

#filter only taxa identified at species level
cons_result_afsp<- cons_result_af %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

cons_result_af10 <- cons_result_afsp %>% filter(nobs >= 10) #filter only taxas with 30+ occurances
write_csv(cons_result_af10,'fnal_H3_af.csv') #write the final dataframe

sa <- ggplot(cons_result_af10, aes(x = estimate, y = sp))+ #makes the estimates graph
  geom_point(alpha = 0.5, aes(size = d07, colour = significance, shape = population))+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  geom_vline(xintercept = 0)+
  labs(color = 'Significance', size = bquote("Conspecific \n density at"~T[0]), shape = 'Population')+
  ylab('Species')+ xlab('Estimate')+ labs(title = 'Araucaria Forest')+ theme_classic()

plot_grid(sr, sa, ncol = 2, nrow = 1, labels = c('A', 'B'))

#organizing the ecological group column
GE <- factor(cons_result_af10$ge, levels = c('Pioneer','Secondary', 'Climax', 'Not Classified'),ordered = T)


# ANOVA pra ver o efeito dividido por grupo ecologico
cons_result_af10 %>% group_by(ge) %>% 
  summarise(estimate = mean(estimate))
# ge        estimate
# 1 Climax      -0.651
# 2 Pioneer     -0.718
# 3 Secondary   -0.725

aov_ge <- aov(estimate ~ ge, data = cons_result_af10)
summary(aov_ge)
TukeyHSD(aov_ge)

# ANOVA pra ver o efeito dividido por sindrome de dispersão
cons_result_af10 %>% group_by(ds) %>% 
  summarise(estimate = mean(estimate))
# ds          estimate
# 2 anemochoric   -0.544
# 3 autochoric    -0.461
# 4 zoochoric     -0.739

aov_ds <- aov(estimate ~ ds, data = cons_result_af10)
summary(aov_ds)
TukeyHSD(aov_ds)

  
#H3 DECIDUOUS FOREST----
#model results rain forest
species_df %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('df_results_lm.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates rain forest
species_df %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ density07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('df_estimates_lm.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_df <- read.csv("df_results_lm.csv")
results_df <- select(results_df, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_df <- read.csv('df_estimates_lm.csv')
estimates_df <- estimates_df %>% select(sp, estimate, std.error, p.value) %>% 
  rename(est.p = p.value)

cons_result_df <- full_join(sp_list_df,results_df) #joins the results to the species list
cons_result_df <- full_join(cons_result_df, estimates_df) #joins the lm results with the estimates values
cons_result_df$significance <- ifelse(cons_result_df$p.value <= 0.05,1,0) #creates a column with 1 for the lm significance p<0.05
cons_result_df$significance[is.na(cons_result_df$significance)] <- 0 #replaces NA with 0
cons_result_df$significance<-as.factor(cons_result_df$significance) #makes the values factors 

#filter only taxa identified at species level
cons_result_dfsp<- cons_result_df %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

cons_result_df10 <- cons_result_dfsp %>% filter(nobs >= 10) #filter only taxas with 30+ occurances
write_csv(cons_result_df10,'fnal_H3_df.csv') #write the final dataframe

ggplot(cons_result_df10, aes(x = estimate, y = sp))+ #makes the estimates graph
  geom_point(alpha = 0.7, aes(size = d07, colour = significance))+
  geom_vline(xintercept = 0)  
  
#H3 RAIN FOREST - TOTAL DENSITY ----
#model results rain forest
species_rf %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('rf_results_lm_dt.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates rain forest
species_rf %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('rf_estimates_lm_dt.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_rf_dt <- read.csv("rf_results_lm_dt.csv")
results_rf_dt <- select(results_rf_dt, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_rf_dt <- read.csv('rf_estimates_lm_dt.csv')
estimates_rf_dt <- estimates_rf_dt %>% select(sp, estimate, std.error, p.value) %>% 
  rename(est.p = p.value)

final_rf_dt <- full_join(sp_list_rf,results_rf_dt) #joins the results to the species list
final_rf_dt <- full_join(final_rf_dt, estimates_rf_dt) #joins the lm results with the estimates values
final_rf_dt$significance <- ifelse(final_rf_dt$p.value <= 0.05,'p < 0.05','p > 0.05') #creates a column with 1 for the lm significance p<0.05
final_rf_dt$significance[is.na(final_rf_dt$significance)] <- 'p > 0.05' #replaces NA with 0
final_rf_dt$significance<-as.factor(final_rf_dt$significance) #makes the values factors 
final_rf_dt$population <- ifelse(final_rf_dt$mean_change <= 0, 'Decrease', 'Increase') #create column with population increase
final_rf_dt$population <- as.factor(final_rf_dt$population) #makes the values factors
final_rf_dt$ds<-as.factor(final_rf_dt$ds)
#filter only taxa identified at species level
final_rf_dt<- final_rf_dt %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

final_rf_dt <- final_rf_dt %>% filter(nobs >= 10) #filter only taxas with 10+ occurances
write_csv(final_rf_dt,'fnal_H3_rf_dt.csv') #write the final dataframe

ggplot(final_rf_dt, aes(x = estimate, y = sp))+ #makes the estimates graph
  geom_point(alpha = 0.7, aes(size = d07, colour = significance, shape = population))+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  geom_vline(xintercept = 0)+
  labs(color = 'Significance', size = bquote("Total\n density at"~T[0]), shape = 'Population')+
  ylab('Species')+ xlab('Estimate')+ labs(title = 'Evergreen Rain Forest')+ theme_classic()

#organizing the ecological group column
final_rf_dt$ge <- revalue(final_rf_dt$ge, c('P' = 'Pioneer', 'SE' = 'Secondary', 'C' = 'Climax', 'NC' = 'Not Classified'))
GE <- factor(final_rf_dt$ge, levels = c('Pioneer','Secondary', 'Climax', 'Not Classified'),ordered = T)


#JITTER FOR ECOLOGICAL GROUP
srt <- ggplot(aes(x = GE, y = estimate), data = final_rf_dt)+ 
  ylab('Estimate')+ xlab('Ecological Group')+
  geom_jitter(position=position_jitter(0.3),alpha = 0.5,
              aes(size = d07,color = significance, shape = population)) + 
  geom_hline(yintercept = 0)+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  labs(color = 'Significance', size = bquote("Total\n density at"~T[0]), shape = 'Population')+
  theme_classic()+ labs(title = 'Evergreen Rain Forest')+ coord_flip() #+ 

#NÃO HÁ DIFERENÇA ENTRE GRUPOS ECOLÓGICOS OU SÍNDROMES DE DISPERSÃO


#H3 ARAUCARIA FOREST - TOTAL DENSITY ----
species_af %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('af_results_lm_dt.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates rain forest
species_af %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('af_estimates_lm_dt.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_af_dt <- read.csv("af_results_lm_dt.csv")
results_af_dt <- select(results_af_dt, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_af_dt <- read.csv('af_estimates_lm_dt.csv')
estimates_af_dt <- estimates_af_dt %>% select(sp, estimate, std.error, p.value) %>% 
  rename(est.p = p.value)

final_af_dt <- full_join(sp_list_af,results_af_dt) #joins the results to the species list
final_af_dt <- full_join(final_af_dt, estimates_af_dt) #joins the lm results with the estimates values
final_af_dt$significance <- ifelse(final_af_dt$p.value <= 0.05,'p < 0.05','p > 0.05') #creates a column with 1 for the lm significance p<0.05
final_af_dt$significance[is.na(final_af_dt$significance)] <- 0 #replaces NA with 0
final_af_dt$significance<-as.factor(final_af_dt$significance) #makes the values factors 
final_af_dt$population <- ifelse(final_af_dt$mean_change <= 0, 'Decrease', 'Increase') #create column with population increase
final_af_dt$population <- as.factor(final_af_dt$population) #makes the values factors

#filter only taxa identified at species level
final_af_dt<- final_af_dt %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

final_af_dt <- final_af_dt %>% filter(nobs >= 10) #filter only taxas with 30+ occurances
write_csv(final_af_dt,'fnal_H3_af_dt.csv') #write the final dataframe

sat <- ggplot(final_af_dt, aes(x = estimate, y = sp))+ #makes the estimates graph
  geom_point(alpha = 0.5, aes(size = d07, colour = significance, shape = population))+
  scale_color_manual(values = c('#4d5c15','#ffa600'))+
  geom_vline(xintercept = 0)+
  labs(color = 'Significance', size = bquote("Total \n density at"~T[0]), shape = 'Population')+
  ylab('Species')+ xlab('Estimate')+ labs(title = 'Araucaria Forest')+ theme_classic()

plot_grid(srt, sat, ncol = 2, nrow = 1, labels = c('A', 'B'))

#organizing the ecological group column
GE <- factor(cons_result_af10$ge, levels = c('Pioneer','Secondary', 'Climax', 'Not Classified'),ordered = T)


# ANOVA pra ver o efeito dividido por grupo ecologico
cons_result_af10 %>% group_by(ge) %>% 
  summarise(estimate = mean(estimate))
# ge        estimate
# 1 Climax      -0.651
# 2 Pioneer     -0.718
# 3 Secondary   -0.725

aov_ge <- aov(estimate ~ ge, data = cons_result_af10)
summary(aov_ge)
TukeyHSD(aov_ge)

# ANOVA pra ver o efeito dividido por sindrome de dispersão
cons_result_af10 %>% group_by(ds) %>% 
  summarise(estimate = mean(estimate))
# ds          estimate
# 2 anemochoric   -0.544
# 3 autochoric    -0.461
# 4 zoochoric     -0.739

aov_ds <- aov(estimate ~ ds, data = cons_result_af10)
summary(aov_ds)
TukeyHSD(aov_ds)
#H3 DECIDUOUS FOREST - TOTAL DENSITY ----
species_df %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         results = map(fit, glance)) %>% 
  unnest(results) %>% 
  select(-c(data,fit)) %>%
  write_csv('df_results_lm_dt.csv') #iterates a linear model for all species and tabulate the regression results

#predictor estimates deciduous forest
species_df %>% 
  nest(-sp)  %>% 
  mutate(fit = map(data, ~ lm(density14-density07 ~ dt07, data =.)),
         cof = map(fit, tidy)) %>% 
  unnest(cof) %>% 
  select(-c(data, fit)) %>%
  filter(term !='(Intercept)') %>% 
  write_csv('df_estimates_lm_dt.csv') #iterates a linear model for all species and tabulate the estimates results
#because I don´t know how to do these two at the same time

#read and join the files
results_df_dt <- read.csv("df_results_lm_dt.csv")
results_df_dt <- select(results_df_dt, sp, r.squared, adj.r.squared, p.value, nobs)

estimates_df_dt <- read.csv('df_estimates_lm_dt.csv')
estimates_df_dt <- estimates_df_dt %>% select(sp, estimate, std.error, p.value) %>% 
  rename(est.p = p.value)

final_df_dt <- full_join(sp_list_df,results_df_dt) #joins the results to the species list
final_df_dt <- full_join(final_df_dt, estimates_df_dt) #joins the lm results with the estimates values
final_df_dt$significance <- ifelse(final_df_dt$p.value <= 0.05,'p < 0.05','p > 0.05') #creates a column with 1 for the lm significance p<0.05
final_df_dt$significance[is.na(final_df_dt$significance)] <- 0 #replaces NA with 0
final_df_dt$significance<-as.factor(final_df_dt$significance) #makes the values factors 
final_df_dt$population <- ifelse(final_df_dt$mean_change <= 0, 'Decrease', 'Increase') #create column with population increase
final_df_dt$population <- as.factor(final_df_dt$population) #makes the values factors

#filter only taxa identified at species level
final_df_dt<- final_df_dt %>% filter(str_detect(sp,"[:alpha:]+[:blank:]+[:alpha:]"))

final_df_dt <- final_df_dt %>% filter(nobs >= 10) #filter only taxas with 30+ occurances
write_csv(final_df_dt,'fnal_H3_df_dt.csv') #write the final dataframe



  
