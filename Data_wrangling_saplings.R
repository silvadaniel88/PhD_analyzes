## LIBRARIES
library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(vegan)

setwd("folder path")
wdname<-"folder path"

df<-read.csv("bases/dados14.csv",sep=";", fileEncoding="UTF-8-BOM") #dados 2014
head(df)
names(df)

df07<-read.csv('bases/dados07.csv',sep=";", fileEncoding="UTF-8-BOM") #dados 2007
head(df07)
names(df07)



# CREATING DATA FOD ----
#filtrar UAs na FOD e com fotos hemisféricas
df14 <- (filter(df,manter == 'Sim', ft =='RF', area == 400))
#df14 <- inner_join(df14, sp_tree, by = 'sp') # filtra apenas espécies arbóreas


#dados gerais para a parcela
df_ua<- df14 %>% 
  group_by(ua) %>% 
  summarize(r = length(unique(sp)),
            density = length(sp),
            ht = mean(ht),
            co = mean(co),
            mean_fcover = mean(mean_fcover),
            sd_fcover = mean(sd_fcover),
            ft = first(ft),
            area = first(area))

#dados por grupo ecológico para parcela
df_ua_ge<-df14 %>% 
  group_by(ua,ge) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
            ) %>% 
  pivot_wider(names_from = ge, values_from = c(density, r, ht))

#dados por sindrome de dispersão para parcela
df_ua_ds<-df14 %>% 
  group_by(ua,ds) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ds, values_from = c(density, r, ht))

ua14<-full_join(df_ua, df_ua_ge, by = 'ua') #junta tudo numa tabela só pt.1
ua14<-full_join(ua14, df_ua_ds, by = 'ua') #junta tudo numa tabela só pt.2
#cria csv
write.csv(ua14,"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/ua14_rf_400.csv",row.names = FALSE)

#PRIMEIRO CICLO
df07 <- (filter(df07, area == 400)) #filtra somente parcelas com 400m² de reg

#dados por parcela p/ grupos ecologicos
df07_ua_ge <- df07 %>% 
  group_by(ua,ge) %>% 
  summarize(ht07 = mean(ht),
            area = first(area),
            density07 = length(sp),
            r07 = length(unique(sp)),
            ) %>% 
  pivot_wider(names_from = ge, values_from = c(density07, r07, ht07))

#dados por parcela p/ sindrome de dispersão
df07_ua_ds <- df07 %>% 
  group_by(ua,ds) %>% 
  summarize(ht07 = mean(ht),
            density07 = length(sp),
            r07 = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ds, values_from = c(density07, r07, ht07))

#dados gerais por parcela
df07_ua <- df07 %>% 
  group_by(ua) %>% 
  summarize(ht07 = mean(ht),
            density07 = length(sp),
            r07 = length(unique(sp)))
ua07 <- full_join(df07_ua, df07_ua_ge, by = 'ua') #junta tudo pt1    
ua07 <- full_join(ua07, df07_ua_ds, by = 'ua') #junta tudo pt2 

lista_ua <- select(ua14,ua) #lista final de parcelas com fotos e 400 m² na FOD em 2014
ua07 <- inner_join(ua07,lista_ua, by ='ua') #planilha final do primeiro ciclo
lista_ua07 <- select(ua07,ua)# lista final de parcelas com fotos e 400 m² na FOD conjunta 2007 e 2014
ua14c <- inner_join(ua14,lista_ua07, by = 'ua')

#cria csv
write.csv(ua14c,"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/ua14_rf_0714.csv",row.names = FALSE)
write.csv(ua07,"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/ua07_rf_07014.csv",row.names = FALSE)

# tabelas por espécie apenas das parcelas que aparecem em 07 e 14

sp14 <- df14 %>%
  filter(ua %in% lista_ua07$ua) %>%
  group_by(ua,sp) %>% 
  summarize(ht14 = mean(ht),
            density14 = n(),
            ft = first(ft)
            )


sp07 <- df07 %>% 
  filter(ua %in% lista_ua07$ua) %>%
  group_by(ua,sp) %>% 
  summarize(ht07 = mean(ht),
            density07 = n())

#junta tabelas
sp_final <- full_join(sp07, sp14)
sp_final <- arrange(sp_final, ua, sp)

#cria csv
write.csv(sp_final,"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/sp_rf_0714.csv",row.names = FALSE)

# EXPLORATION  FOD----

#lê tabelas criadas no passo anterior
setwd("D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados")
wdname<-"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados"
ua14 <- read.csv("ua14_rf.csv") 
ua07 <- read.csv("ua07_rf.csv")
sp <- read.csv("sp_rf.csv")

ua14_ <- ua14 %>% 
  filter(mean_fcover > 15) %>% 
  filter(ft != 'RES') %>% 
  filter(ft != 'AF/DF') %>% 
  filter(ft != 'RF/AF')


corre<-cor(ua14)
write.csv(corre,'D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/correlation14.csv')

#insere o GE na lista de espécies
ge14 <- df %>% 
  select(sp,ge) %>% 
  distinct()

ge07 <- df07 %>% 
  select(sp,ge) %>% 
  distinct()

ge <- full_join(ge14, ge07)
write.csv(ge,'D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/ge.csv')
#adiciona o GE nos dados das parcelas por espécie
sp1 <- inner_join(ge,sp, by = 'sp')
# sp1 <- sp1 %>% 
#   filter(ft != 'RES') %>% 
#   filter(ft != 'AF/DF') %>% 
#   filter(ft != 'RF/AF')

#calcula valores médios por espécie
# sp_mean <-sp %>% 
#   group_by(sp) %>% 
#   summarise(ge = first(ge),
#             ht07 = mean(ht07,na.rm = T),
#             ht14 = mean(ht14,na.rm = T),
#             density07 = mean(density07,na.rm = T),
#             density14 = mean(density14,na.rm = T))



# fcover médio vs. altura
ggplot(ua14)+
  geom_smooth(method='loess',se=F,aes(x=mean_fcover, y=ht))+
  geom_point(colour='blue', alpha = 0.1, aes(x=mean_fcover, y=ht))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=ht_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=ht_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=ht_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=ht_C))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=mean_fcover, y=ht_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=ht_SE))+
  facet_wrap(~ft)


# fcover médio vs. densidade
ggplot(ua14)+
  geom_smooth(method='loess',se=F,aes(x=mean_fcover, y=density))+
  geom_point(colour='blue', alpha = 0.1, aes(x=mean_fcover, y=density))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=density_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=density_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=density_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=density_C))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=mean_fcover, y=density_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=density_SE))+
  facet_wrap(~ft)

# fcover médio vs. riqueza
ggplot(ua14)+
  #geom_smooth(method='loess',se=F,aes(x=mean_fcover, y=r))+
  #geom_point(colour='blue', alpha = 0.3, aes(x=mean_fcover, y=r))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=mean_fcover, y=r_P/r))+
  geom_point(colour='red', alpha = 0.1, aes(x=mean_fcover, y=r_P/r))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=mean_fcover, y=r_C/r))+
  geom_point(colour='green', alpha = 0.1, aes(x=mean_fcover, y=r_C/r))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=mean_fcover, y=r_SE/r))+
  geom_point(colour='black', alpha = 0.1, aes(x=mean_fcover, y=r_SE/r))+
  facet_wrap(~ft)

# fcover vaiação vs. altura
ggplot(ua14)+
  geom_smooth(method='loess',se=F,aes(x=sd_fcover, y=ht))+
  geom_point(colour='blue', alpha = 0.1, aes(x=sd_fcover, y=ht))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=sd_fcover, y=ht_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=sd_fcover, y=ht_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=sd_fcover, y=ht_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=sd_fcover, y=ht_C))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=sd_fcover, y=ht_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=sd_fcover, y=ht_SE))+
  facet_wrap(~ft)

# fcover vaiação vs. densidade
ggplot(ua14)+
  geom_smooth(method='loess',se=F,aes(x=sd_fcover, y=density))+
  geom_point(colour='blue', alpha = 0.1, aes(x=sd_fcover, y=density))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=sd_fcover, y=density_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=sd_fcover, y=density_P))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=sd_fcover, y=density_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=sd_fcover, y=density_C))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=sd_fcover, y=density_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=sd_fcover, y=density_SE))+
  facet_wrap(~ft)

# fcover vaiação vs. riqueza
ggplot(ua14)+
  #geom_smooth(method='loess',se=F,aes(x=sd_fcover, y=r))+
  #geom_point(colour='blue', alpha = 0.3, aes(x=sd_fcover, y=r))+
  geom_smooth(method='loess',se=F, colour='red', aes(x=sd_fcover, y=r_P/r))+
  geom_point(colour='red', alpha = 0.3, aes(x=sd_fcover, y=r_P/r))+
  geom_smooth(method='loess',se=F, colour='green', aes(x=sd_fcover, y=r_C/r))+
  geom_point(colour='green', alpha = 0.3, aes(x=sd_fcover, y=r_C/r))+
  geom_smooth(method='loess',se=F, colour='black', aes(x=sd_fcover, y=r_SE/r))+
  geom_point(colour='black', alpha = 0.3, aes(x=sd_fcover, y=r_SE/r))+
  facet_wrap(~ft)


#densidade 07 vs. densidade 14 plot level
ua_total <- inner_join(ua07, ua14, by = 'ua')

ggplot(ua_total)+
geom_smooth(method='loess', se=F,  colour='green', aes(x=density07_C, y=density_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=density07_C, y=density_C))+
  geom_smooth(method='loess', se=F,  colour='red', aes(x=density07_P, y=density_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=density07_P, y=density_P))+
  geom_smooth(method='loess', se=F, colour='black', aes(x=density07_SE, y=density_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=density07_SE, y=density_SE))+
  geom_abline(intercept = 0)+xlim(1,400)+ylim(1,400)+
  geom_smooth(method='loess', se=F,  colour='blue', aes(x=density07, y=density))
  #+facet_wrap(~ft)

#densidade 07 vs. densidade 14 sp/plot level
ggplot(sp1)+
  geom_point(alpha = 0.1, aes(x = density07, y = density14, color = ge))+
  geom_smooth(method = 'loess', se = F, aes(x = density07, y = density14, color = ge))+
  xlim(1,100)+ylim(1,100)+geom_abline(intercept = 0) + scale_color_brewer(palette="Set1")
  #+facet_wrap(~ft)

#dinamica da altura e densidade sp/plot level
ggplot(sp1)+
  geom_point(alpha = 0.1, aes(x = density14-density07, y = ht14-ht07, color = ge))+
  geom_smooth(method = 'lm', se = F, aes(x = density14-density07, y = ht14-ht07, color = ge))+
  #xlim(1,100)+ylim(1,15)+geom_abline(intercept = 0) + scale_color_brewer(palette="Set1")+
   scale_color_brewer(palette="Set1")

#riqueza 07 vs riqueza 14
ggplot(ua_total)+
  geom_smooth(method='loess', se=F,  colour='green', aes(x=r07_C, y=r_C))+
  geom_point(colour='green', alpha = 0.1, aes(x=r07_C, y=r_C))+
  geom_smooth(method='loess', se=F,  colour='red', aes(x=r07_P, y=r_P))+
  geom_point(colour='red', alpha = 0.1, aes(x=r07_P, y=r_P))+
  geom_smooth(method='loess', se=F, colour='black', aes(x=r07_SE, y=r_SE))+
  geom_point(colour='black', alpha = 0.1, aes(x=r07_SE, y=r_SE))+
  geom_abline(intercept = 0)+xlim(1,100)+ylim(1,100)+
  geom_smooth(method='loess', se=F,  colour='blue', aes(x=r07, y=r))+
  geom_point(colour='blue', alpha = 0.1, aes(x=r07, y=r))
  #+facet_wrap(~ft)



# CREATING DATA FED/FOM ----

#filtrar UAs na FED/FOM e com fotos hemisféricas e 400m²
df14 <- (filter(df,manter == 'Sim', area == 400,
               ft != 'RES',ft != 'RF',ft != 'AF/DF',ft != 'RF/AF'))

#dados gerais para a parcela
df_ua<- df14 %>% 
  group_by(ua) %>% 
  summarize(r = length(unique(sp)),
            density = length(sp),
            ht = mean(ht),
            co = mean(co),
            mean_fcover = mean(mean_fcover),
            sd_fcover = mean(sd_fcover),
            ft = first(ft),
            area = first(area))

#dados por grupo ecológico para parcela
df_ua_ge<-df14 %>% 
  group_by(ua,ge) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ge, values_from = c(density, r, ht))

#dados por sindrome de duspersão 
df_ua_ds<-df14 %>% 
  group_by(ua,ds) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ds, values_from = c(density, r, ht))

ua14<-full_join(df_ua, df_ua_ge, by = 'ua') #junta tudo numa tabela só pt1
ua14<-full_join(ua14, df_ua_ds, by = 'ua') #junta tudo numa tabela só pt1

#cria arquivo csv
write.csv(ua14,"folder path/ua14_afdf_400.csv",row.names = FALSE)

# arquivos para comparar 07 com 14
# permanece somente a SR 3 de cada subparcela 14
#segundo ciclo
df14c <- (filter(df,manter == 'Sim', SR == 3,
                ft != 'RES',ft != 'RF',ft != 'AF/DF',ft != 'RF/AF'))

#dados gerais para a parcela
df_uac<- df14c %>% 
  group_by(ua) %>% 
  summarize(r = length(unique(sp)),
            density = length(sp),
            ht = mean(ht),
            ft = first(ft),
            area = first(area))

#dados por grupo ecológico para parcela
df_ua_gec<-df14c %>% 
  group_by(ua,ge) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ge, values_from = c(density, r, ht))

#dados por sindrome de dispersao
df_ua_dsc<-df14c %>% 
  group_by(ua,ds) %>% 
  summarize(ht=mean(ht),
            density = length(sp),
            r = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ds, values_from = c(density, r, ht))

ua14c<-full_join(df_uac, df_ua_gec, by = 'ua') #junta tudo numa tabela só pt1
ua14c<-full_join(ua14c, df_ua_dsc, by = 'ua') #junta tudo numa tabela só pt2


#primeiro ciclo
df07c <- (filter(df07, SR == 3,
                 ft != 'RES',ft != 'RF',ft != 'AF/DF',ft != 'RF/AF'))

#dados por parcela p/ grupos ecologicos
df07_ua_gec <- df07c %>% 
  group_by(ua,ge) %>% 
  summarize(ht07 = mean(ht),
            area = first(area),
            density07 = length(sp),
            r07 = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ge, values_from = c(density07, r07, ht07))

#dados por parcela p/ sindrome de dispersão
df07_ua_dsc <- df07c %>% 
  group_by(ua,ds) %>% 
  summarize(ht07 = mean(ht),
            area = first(area),
            density07 = length(sp),
            r07 = length(unique(sp)),
  ) %>% 
  pivot_wider(names_from = ds, values_from = c(density07, r07, ht07))

#dados gerais por parcela
df07_uac <- df07c %>% 
  group_by(ua) %>% 
  summarize(ht07 = mean(ht),
            density07 = length(sp),
            r07 = length(unique(sp)))

ua07c <- full_join(df07_uac, df07_ua_gec, by = 'ua') #junta tudo pt1   
ua07c <- full_join(ua07c, df07_ua_dsc, by = 'ua') #junta tudo pt2 

lista_uac <- select(ua14c,ua) #lista final de parcelas com fotos SR3 na FOM/FED em 2014
ua07c <- inner_join(ua07c,lista_uac, by ='ua') #planilha final do primeiro ciclo

listauac7 <- select(ua07c,ua)
ua14c <- inner_join(ua14c,listauac7, by = 'ua')

#cria arquivo csv
write.csv(ua07c,"folder path/ua07_afdf_sr3.csv",row.names = FALSE)
write.csv(ua14c,"folder path/ua14_afdf_sr3.csv",row.names = FALSE)

# tabelas por espécie 
sp14c <- df14c %>% 
  filter(ua %in% listauac7$ua) %>% 
  group_by(ua,sp) %>% 
  summarize(ht14 = mean(ht),
            density14 = n(),
            ft = first(ft)
  )

sp07c <- df07c %>% 
  filter(ua %in% listauac7$ua) %>%
  group_by(ua,sp) %>% 
  summarize(ht07 = mean(ht),
            density07 = n())
#sp07c <- inner_join(sp07c,lista_uac, by = 'ua') #apenas parcela que temons na lista final

sp_finalc <- full_join(sp14c, sp07c)
sp_finalc <- arrange(sp_finalc, ua, sp)
write.csv(sp_finalc,"D:/OneDrive - FURB/Doutorado/Estudos/Restauração/dados/sp_afdf_sr3.csv",row.names = FALSE)




