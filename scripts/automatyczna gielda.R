library(XLConnect)
library(tidyverse)
library(readxl)
library(rvest)
library(stringr)
library(zoo)
library(treemapify)
library(plotly)
library(here)
library(httr)
library(scales)
library(shadowtext)
library(ggrepel)


source(here("./scripts/url.R"))
load(here("./data/ticker.RData"))

theurl10<-read_html("http://notowania.pb.pl/stocktable/GPWAKCJE")
tables0<-as_tibble(html_table(theurl10)[[1]])
table<-tibble(spolka=tables0$`Nazwa AD`,
              zmiana=gsub(",",".",tables0$`Zmiana % AD`),
              obroty=gsub(",",".",tables0$`Obrót AD`)
) %>% 
  mutate_at(c("zmiana"),~as.numeric(substr(.,1,nchar(.)-1))) %>% 
  mutate_at(c("obroty"),~as.numeric(gsub("\\s","",.)))

table$color<-unlist(lapply(table$zmiana,function(x) sign(x)*min(abs(x),3)))
table$label<-paste0(table$spolka,"\n",round(table$zmiana,1),"%")



theurl20<-read_html("http://notowania.pb.pl/stocktable/UTPXNCO")
tables20<-as_tibble(html_table(theurl20)[[1]])
table2<-tibble(spolka=tables20$`Nazwa AD`,
              zmiana=gsub(",",".",tables20$`Zmiana % AD`),
              obroty=gsub(",",".",tables20$`Obrót AD`)
) %>% 
  mutate_at(c("zmiana"),~as.numeric(substr(.,1,nchar(.)-1))) %>% 
  mutate_at(c("obroty"),~as.numeric(gsub("\\s","",.)))

table2$color<-unlist(lapply(table2$zmiana,function(x) sign(x)*min(abs(x),3)))
table2$label<-paste0(table2$spolka,"\n",round(table2$zmiana,1),"%")


godzina<-theurl10 %>% html_nodes("time") %>% html_text() %>% substr(.,26,30)
dzien<-theurl10 %>% html_nodes("time") %>% html_text() %>% substr(.,15,24) %>% as.Date()

bp<-ggplot(table, aes(area = obroty, fill = color, label=label)) +
  geom_treemap()+
  scale_fill_gradient2(low="red",high="forestgreen",mid="gray10",midpoint = 0)+
  geom_treemap_text(fontface = "italic", 
                    colour = "white", place = "topleft", 
                    padding.x = grid::unit(0.2, "cm"),padding.y = grid::unit(0.2, "cm"),min.size = 1)+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  labs(title=paste0("Wyniki sesji na rynku głównym z dnia ",format(Sys.Date(),"%d %B"),", z godziny ", godzina),
       subtitle="wielkość pola jest proporcjonalna do obrotów generowanych na spółce",
       caption="źródło: Puls Biznesu")

png("/Users/ignacymorawski/Downloads/gielda1.png",width=350,height=180,units="mm",bg = "white",res=300)
print(bp)
dev.off()



bp2<-ggplot(table2, aes(area = obroty, fill = color, label=label)) +
  geom_treemap()+
  scale_fill_gradient2(low="red",high="forestgreen",mid="gray10",midpoint = 0)+
  geom_treemap_text(fontface = "italic", 
                    colour = "white", place = "topleft", 
                    padding.x = grid::unit(0.2, "cm"),padding.y = grid::unit(0.2, "cm"),min.size = 1)+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  labs(title=paste0("Wyniki sesji na New Connect z dnia ",format(Sys.Date(),"%d %B"),", z godziny ", godzina),
       subtitle="wielkość pola jest proporcjonalna do obrotów generowanych na spółce",
       caption="źródło: Puls Biznesu")

png("/Users/ignacymorawski/Downloads/gielda1b.png",width=350,height=180,units="mm",bg = "white",res=300)
print(bp2)
dev.off()

#### Spółki o największych obrotach ogółem ####

turn<-table %>% arrange(desc(obroty))

 turn.plus<-turn %>% filter(zmiana>0) %>% head(.,10) %>% arrange(zmiana) %>% 
   mutate(theme="10 spółek rosnących o największych obrotach") %>% 
   mutate(spread=max(zmiana)-min(zmiana)) %>% 
   mutate(point=max(zmiana))
   
 turn.minus<-turn %>% filter(zmiana<0) %>% head(.,10) %>% arrange(zmiana) %>% 
   mutate(theme="10 spółek spadających o największych obrotach") %>% 
   mutate(spread=min(zmiana)-max(zmiana)) %>% 
   mutate(point=min(zmiana))
 
 turn0<-bind_rows(turn.plus,turn.minus) %>% select(c(1,2,3,6,7,8)) %>% 
   mutate_at(c("spolka"),~factor(.,levels=unique(.))) %>%
   mutate_at(c("theme"),~factor(.,levels=unique(.))) %>% 
   mutate(color=sign(zmiana))
# 
# 
# 
# 
# bp<-ggplot(turn0,aes(x = spolka, fill = color, group=theme)) +
#   geom_bar(aes(y=zmiana),stat="identity")+
#   geom_text(aes(y=zmiana+spread*0.05,label=paste0(round(zmiana,1),"%"),color=color),size=4)+
#   coord_flip()+
#   scale_fill_gradient2(low="red",high="forestgreen",midpoint = 0)+
#   scale_color_gradient2(low="red",high="forestgreen",midpoint = 0)+
#   facet_wrap(~theme,ncol=2,scales="free")+
#   theme(strip.background = element_rect(fill="transparent",color="transparent"))+
#   theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
#   theme(legend.position = "none")+
#   theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
#   theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
#   theme(panel.spacing = unit(3,"cm"))+
#   theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
#   theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
#   xlab("")+
#   ylab("")+
#   labs(title="Wyniki sesji giełdowej z dnia 12. października",
#        subtitle="spółki o największych obrotach wśród zyskujących i tracących",
#        caption="źródło: Puls Biznesu, Bankier.pl")
# 
# png("/Users/ignacymorawski/Downloads/gielda2.png",width=350,height=180,units="mm",bg = "white",res=300)
# print(bp)
# dev.off()


### Spółki o największym zainteresowaniu na rynku głównym ####

load(here("./data/wse.RData"))

m1<-tibble(
  spolka=colnames(ws2)[-c(1)],
  mean=ws2 %>% select(-c(1)) %>% tail(50) %>% colMeans(na.rm=TRUE),
  sd=apply(ws2 %>% select(-c(1)) %>% tail(50),2,sd,na.rm=TRUE),
  coeff=mean+1.9*sd
)


h.mult<-17-as.numeric(substr(godzina,1,2))+as.numeric(substr(godzina,4,5))/60

turn<-table %>% arrange(desc(obroty))
m2<-m1 %>% 
  left_join(turn %>% select(c(1,2,3)),by=c("spolka")) %>% 
  mutate(ratio1=((h.mult*obroty)-mean)/sd) %>%
  mutate(ratio2=100*h.mult*obroty/mean-100) %>% 
  drop_na %>% 
  filter(is.finite(ratio1) & is.finite(ratio2) & obroty>100000)


m21<-m2 %>% 
  arrange(desc(ratio1)) %>% 
  head(10)

m22<-m2 %>% 
  filter(ratio1>1) %>% 
  arrange(desc(obroty)) %>% 
  head(20)


m23<-bind_rows(m21,m22) %>% 
  select(ratio1,zmiana,spolka) %>% 
  distinct()

cols<-c("4"="gray23","1"="darkred","3"="forestgreen","2"="forestgreen")

ar1<-max(m2$zmiana)
ar2<-min(m2$zmiana)
ar3<-max(m2$ratio1)

bp3<-ggplot(m2,aes(x=ratio1,y=zmiana,size=(1/2)*obroty^(1/2),color=sign(zmiana)*abs(zmiana)^(1/4)))+
  geom_hline(yintercept = 0, color="gray",size=0.4)+
  geom_vline(xintercept = 0, color="gray",size=0.4)+
  geom_point(alpha=0.7)+
  scale_color_gradient2(low=cols[2],mid=cols[1],high=cols[3],midpoint=0)+
  geom_text(mapping=aes(x=ar3/1.5,y=0.1,label="spółki z obrotami\npowyżej ich średniej 50-dniowej"),color="orange",size=3,hjust=1)+
  geom_text(mapping=aes(x=-2,y=0.1,label="spółki z obrotami\nponiżej ich średniej 50-dniowej"),color="orange",size=3,hjust=0)+  geom_text(mapping=aes(x=0.1,y=ar1+2,label="spółki zyskujące"),color=cols[3],size=4,hjust=0)+
  geom_text(mapping=aes(x=0.1,y=ar2-2,label="spółki tracące"),color=cols[2],size=4,hjust=0)+
  geom_text_repel(data=m23,mapping=aes(x=ratio1+0.05,y=zmiana,label=spolka,color=sign(zmiana)*abs(zmiana)^(1/4)),size=2.5)+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.grid = element_blank())+
  xlim(-2,ar3+0.2)+
  ylab("zmiana kursu w proc.")+
  xlab("o ile oborty akcjami spółki różnią się od średniej dla tej spółki (o ile odchyleń standardowych)")+
  theme(axis.title.x=element_text(size=10,margin=margin(20,0,0,0,unit="pt")))+
  theme(axis.title.y=element_text(size=10,margin=margin(0,20,0,0,unit="pt")))
  #scale_x_continuous(trans='log2') 
  
png("/Users/ignacymorawski/Downloads/gielda3scatter.png",width=300,height=150,units="mm",bg = "white",res=300)
print(bp3)
dev.off()


m2z<-m2 %>%
   filter(obroty>500000 & is.finite(ratio1)) %>% 
   arrange(desc(ratio1)) %>% 
   head(10)
  

m3<-m2z %>% 
  left_join(table,by=c("spolka")) %>% 
  select(c(1,2,5,6,7)) %>%
  mutate(ratio=round(obroty.x/mean,1)) %>% 
  mutate(spread.a=max(ratio1)-min(ratio1)) %>% 
  mutate(spreab.a=max(zmiana.x)-min(zmiana.x)) %>% 
  arrange(ratio1) %>% 
  select(c(1,3,5,6,7,8))

m4<-bind_rows(
 m3 %>% select(c(1,3,5)) %>% `colnames<-`(c("spolka","ratio","spread")) %>% mutate(theme="Ile razy obroty akcjami spółki\nprzekraczały jej średnią z miesiąca*") %>% mutate(label=paste0(round(ratio,1),"x")) %>% mutate(color=4),
 m3 %>% select(c(1,2,6)) %>% `colnames<-`(c("spolka","ratio","spread")) %>% mutate(theme="Zmiana kursu akcji") %>% mutate(label=paste0(round(ratio,1),"%")) %>% mutate(color=sign(ratio)+2)
)

m4$spolka<-factor(m4$spolka,levels=unique(m4$spolka))
m4$theme<-factor(m4$theme,levels=unique(m4$theme))
m4$color<-factor(m4$color,levels=unique(m4$color))

cols<-c("4"="gray23","1"="darkred","3"="forestgreen","2"="forestgreen")

bp3<-ggplot(data=m4,aes(group=theme,fill=color))+
  geom_bar(data=m4,aes(spolka,ratio),stat="Identity")+
  scale_fill_manual(values = cols)+
  coord_flip()+
  geom_text(data=m4,aes(spolka,ratio+sign(ratio)*spread/18,label=label,color=color))+
  scale_color_manual(values = cols)+
  facet_wrap(~theme,scales="free")+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(3,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  xlab("")+
  ylab("")+
  labs(title="Spółki budzące wyjątkowe zainteresowanie",
       subtitle="spółki o największych obrotach wobec średniej historycznej",
       caption="źródło: Puls Biznesu; *wielkość obrotów mierzona jako liczba odchyleń standardowych")
  

png("/Users/ignacymorawski/Downloads/gielda3.png",width=350,height=180,units="mm",bg = "white",res=300)
print(bp3)
dev.off()




### Spółki o największym zainteresowaniu na new connect ####

load(here("./data/nc.RData"))

mc1<-tibble(
  spolka=colnames(nc2)[-c(1)],
  mean=nc2 %>% select(-c(1)) %>% tail(50) %>% colMeans(na.rm=TRUE),
  sd=apply(nc2 %>% select(-c(1)) %>% tail(50),2,sd,na.rm=TRUE),
  coeff=mean+1.9*sd
)


turn2<-table2 %>% arrange(desc(obroty))
mc2<-mc1 %>% 
  left_join(turn2 %>% select(c(1,2,3)),by=c("spolka")) %>% 
  mutate(ratio1=((h.mult*obroty)-mean)/sd) %>%
  mutate(ratio2=100*h.mult*obroty/mean-100) %>% 
  filter(obroty>100000 & is.finite(ratio1)) %>% 
  arrange(desc(ratio1)) %>% 
  head(10)


mc3<-mc2 %>% 
  left_join(table2,by=c("spolka")) %>% 
  select(c(1,2,5,6,7)) %>%
  mutate(ratio=round(obroty.x/mean,1)) %>% 
  mutate(spread.a=max(ratio1)-min(ratio1)) %>% 
  mutate(spreab.a=max(zmiana.x)-min(zmiana.x)) %>% 
  arrange(ratio1) %>% 
  select(c(1,3,5,6,7,8))

mc4<-bind_rows(
  mc3 %>% select(c(1,3,5)) %>% `colnames<-`(c("spolka","ratio","spread")) %>% mutate(theme="Ile razy obroty akcjami spółki\nprzekraczały jej średnią z roku") %>% mutate(label=paste0(round(ratio,1),"x")) %>% mutate(color=4),
  mc3 %>% select(c(1,2,6)) %>% `colnames<-`(c("spolka","ratio","spread")) %>% mutate(theme="Zmiana kursu akcji") %>% mutate(label=paste0(round(ratio,1),"%")) %>% mutate(color=sign(ratio)+2)
)

mc4$spolka<-factor(mc4$spolka,levels=unique(mc4$spolka))
mc4$theme<-factor(mc4$theme,levels=unique(mc4$theme))
mc4$color<-factor(mc4$color,levels=unique(mc4$color))

cols<-c("4"="gray23","1"="darkred","3"="forestgreen","2"="forestgreen")


bp3b<-ggplot(data=mc4,aes(group=theme,fill=color))+
  geom_bar(data=mc4,aes(spolka,ratio),stat="Identity")+
  scale_fill_manual(values = cols)+
  coord_flip()+
  geom_text(data=mc4,aes(spolka,ratio+sign(ratio)*spread/18,label=label,color=color))+
  scale_color_manual(values = cols)+
  facet_wrap(~theme,scales="free")+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(3,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  xlab("")+
  ylab("")+
  labs(title="Spółki budzące wyjątkowe zainteresowanie na New Connect",
       subtitle="spółki o największych obrotach wobec średniej historycznej",
       caption="źródło: Puls Biznesu")


png("/Users/ignacymorawski/Downloads/gielda3b.png",width=350,height=180,units="mm",bg = "white",res=300)
print(bp3b)
dev.off()






#### Spółki najbliższe rocznych maksimów i rocznych minimów NA RYNKU GŁÓWNYM ####

n1<-tibble(
  spolka=colnames(ws1)[-c(1)],
  max=apply(tail(ws1 %>% select(-c(1)),250),2,max,na.rm=TRUE),
  min=apply(tail(ws1 %>% select(-c(1)),250),2,min,na.rm=TRUE),
  turn=apply(tail(ws2 %>% select(-c(1)),250),2,mean,na.rm=TRUE),
  last=tail(ws1 %>% select(c(1)),1) %>% unlist %>% as.Date(.),
  sd=apply(tail(ws1 %>% select(-c(1)),250),2,sd,na.rm=TRUE)
)

# n1<-Reduce(bind_rows,lapply(lista,function(x) {
#   z1<-tail(x[,6],250)
#   z2<-tail(x[,8],250)
#   z3<-max(z1,na.rm=TRUE)
#   z4<-min(z1,na.rm=TRUE)
#   z5<-mean(z2,na.rm=TRUE)
#   z6<-tail(x[,1],1)
#   z7<-sd(tail(z1,30))
#   z8<-tibble(spolka="z",max=z3,min=z4,turn=z5,last=z6,sd=z7)
#   return(z8)
# }
# ))
# n1$spolka<-names(lista)

tab2<-tibble(spolka=tables0$`Nazwa AD`,
              kurs=gsub(",",".",tables0$`Kurs AD`),
              obroty=gsub(",",".",tables0$`Obrót AD`)
) %>% 
  mutate_at(c("kurs"),~as.numeric(.)) %>% 
  mutate_at(c("obroty"),~as.numeric(gsub("\\s","",.)))

n2a<-n1 %>% 
  left_join(tab2,by=c("spolka")) %>% 
  mutate(ratio=kurs/max) %>%
  arrange(desc(ratio,obroty)) %>% 
  filter(turn>500000) %>% 
  head(30) %>% 
  filter(sd>0 & last>as.Date("2020-11-01")) %>% 
  head(10)

n2b<-n1 %>% 
  left_join(tab2,by=c("spolka")) %>% 
  mutate(ratio=kurs/min) %>%
  arrange(ratio) %>% 
  filter(turn>500000) %>% 
  head(30) %>% 
  filter(sd>0 & last>as.Date("2020-10-01")) %>% 
  head(10)

# 
# names<-NULL
# for (i in c(2,1,3:length(lista))){
#   
#   if(i==2){
#     close<-as_tibble(lista[i][[1]][,c(1,6)])
#   } else {
#     close<-close %>% left_join(as_tibble(lista[i][[1]][,c(1,6)]),by=c("Data sesji"))
#   }
#   names<-c(names,names(lista[i]))
# }
# colnames(close)<-c("Date",names)

close<-ws1


n3a<-close %>% 
  select(c(1,all_of(n2a$spolka))) %>% 
  tail(250) %>% 
  pivot_longer(
    cols=c(2:11),
    names_to="spolka",
    values_to="value"
               ) %>% 
  mutate_at(c("date"),~as.Date(.))

n3a$spolka<-factor(n3a$spolka,levels=unique(n3a$spolka))



n3b<-close %>% 
  select(c(1,all_of(n2b$spolka))) %>% 
  tail(250) %>% 
  pivot_longer(
    cols=c(2:11),
    names_to="spolka",
    values_to="value"
  ) %>% 
  mutate_at(c("date"),~as.Date(.))
n3b$spolka<-factor(n3b$spolka,levels=unique(n3b$spolka))



bp4<-ggplot(data=n3a,aes(x=date,y=value,group=spolka))+
  geom_line(size=0.6,color="forestgreen")+
  #scale_fill_manual(values = cols)+
  facet_wrap(~spolka,scales="free",ncol=5)+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(2,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  theme(plot.margin=margin(t=10,r=10,b=10,l=10,unit="pt"))+
  xlab("")+
  ylab("")+
  labs(title="Spółki najbliżej rocznego maksimum",
       subtitle="kurs za ostatnie 250 dni",
       caption="źródło: Puls Biznesu, Bankier.pl; dane dotyczą spółek o średnich obrotach powyżej 500 tys. zł")
png("/Users/ignacymorawski/Downloads/gielda4.png",width=350,height=200,units="mm",bg = "white",res=300)
print(bp4)
dev.off()



bp5<-ggplot(data=n3b,aes(x=date,y=value,group=spolka))+
  geom_line(size=0.6,color="darkred")+
  #scale_fill_manual(values = cols)+
  facet_wrap(~spolka,scales="free",ncol=5)+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(2,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  theme(plot.margin=margin(t=10,r=10,b=10,l=10,unit="pt"))+
  xlab("")+
  ylab("")+
  labs(title="Spółki najbliżej rocznego minimum",
       subtitle="kurs za ostatnie 250 dni",
       caption="źródło: Puls Biznesu, Bankier.pl; dane dotyczą spółek o średnich obrotach powyżej 500 tys. zł")


png("/Users/ignacymorawski/Downloads/gielda5.png",width=350,height=200,units="mm",bg = "white",res=300)
print(bp5)
dev.off()






#### Spółki najbliższe rocznych maksimów i rocznych minimów NA NEW CONNECT ####

nx1<-tibble(
  spolka=colnames(nc1)[-c(1)],
  max=apply(tail(nc1 %>% select(-c(1)),250),2,max,na.rm=TRUE),
  min=apply(tail(nc1 %>% select(-c(1)),250),2,min,na.rm=TRUE),
  turn=apply(tail(nc2 %>% select(-c(1)),250),2,mean,na.rm=TRUE),
  last=tail(nc1 %>% select(c(1)),1) %>% unlist %>% as.Date(.),
  sd=apply(tail(nc1 %>% select(-c(1)),250),2,sd,na.rm=TRUE)
)

# n1<-Reduce(bind_rows,lapply(lista,function(x) {
#   z1<-tail(x[,6],250)
#   z2<-tail(x[,8],250)
#   z3<-max(z1,na.rm=TRUE)
#   z4<-min(z1,na.rm=TRUE)
#   z5<-mean(z2,na.rm=TRUE)
#   z6<-tail(x[,1],1)
#   z7<-sd(tail(z1,30))
#   z8<-tibble(spolka="z",max=z3,min=z4,turn=z5,last=z6,sd=z7)
#   return(z8)
# }
# ))
# n1$spolka<-names(lista)

tabx2<-tibble(spolka=tables20$`Nazwa AD`,
             kurs=gsub(",",".",tables20$`Kurs AD`),
             obroty=gsub(",",".",tables20$`Obrót AD`)
) %>% 
  mutate_at(c("kurs"),~as.numeric(.)) %>% 
  mutate_at(c("obroty"),~as.numeric(gsub("\\s","",.)))

nx2a<-nx1 %>% 
  left_join(tabx2,by=c("spolka")) %>% 
  mutate(ratio=kurs/max) %>%
  filter(turn>50000) %>% 
  arrange(desc(ratio)) %>% 
  head(30) %>% 
  filter(sd>0 & last>as.Date("2020-11-01")) %>% 
  head(10)

nx2b<-nx1 %>% 
  left_join(tabx2,by=c("spolka")) %>% 
  mutate(ratio=kurs/min) %>%
  filter(turn>50000) %>% 
  arrange(ratio) %>% 
  head(30) %>% 
  filter(sd>0 & last>as.Date("2020-10-01")) %>% 
  head(10)

# 
# names<-NULL
# for (i in c(2,1,3:length(lista))){
#   
#   if(i==2){
#     close<-as_tibble(lista[i][[1]][,c(1,6)])
#   } else {
#     close<-close %>% left_join(as_tibble(lista[i][[1]][,c(1,6)]),by=c("Data sesji"))
#   }
#   names<-c(names,names(lista[i]))
# }
# colnames(close)<-c("Date",names)

closex<-nc1


nx3a<-closex %>% 
  select(c(1,all_of(nx2a$spolka))) %>% 
  tail(250) %>% 
  pivot_longer(
    cols=c(2:11),
    names_to="spolka",
    values_to="value"
  ) %>% 
  mutate_at(c("date"),~as.Date(.))

nx3a$spolka<-factor(nx3a$spolka,levels=unique(nx3a$spolka))



nx3b<-closex %>% 
  select(c(1,all_of(nx2b$spolka))) %>% 
  tail(250) %>% 
  pivot_longer(
    cols=c(2:11),
    names_to="spolka",
    values_to="value"
  ) %>% 
  mutate_at(c("date"),~as.Date(.))
nx3b$spolka<-factor(nx3b$spolka,levels=unique(nx3b$spolka))



bp4<-ggplot(data=nx3a,aes(x=date,y=value,group=spolka))+
  geom_line(size=0.6,color="forestgreen")+
  #scale_fill_manual(values = cols)+
  facet_wrap(~spolka,scales="free",ncol=5)+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(2,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  theme(plot.margin=margin(t=10,r=10,b=10,l=10,unit="pt"))+
  xlab("")+
  ylab("")+
  labs(title="Spółki z New Connect najbliżej rocznego maksimum",
       subtitle="kurs za ostatnie 250 dni",
       caption="źródło: Puls Biznesu; dane dotyczą spółek o średnich obrotach powyżej 50 tys. zł")
png("/Users/ignacymorawski/Downloads/gielda4b.png",width=350,height=200,units="mm",bg = "white",res=300)
print(bp4)
dev.off()



bp5<-ggplot(data=nx3b,aes(x=date,y=value,group=spolka))+
  geom_line(size=0.6,color="darkred")+
  #scale_fill_manual(values = cols)+
  facet_wrap(~spolka,scales="free",ncol=5)+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(2,"cm"))+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  theme(plot.margin=margin(t=10,r=10,b=10,l=10,unit="pt"))+
  xlab("")+
  ylab("")+
  labs(title="Spółki z New Connect najbliżej rocznego minimum",
       subtitle="kurs za ostatnie 250 dni",
       caption="źródło: Puls Biznesu; dane dotyczą spółek o średnich obrotach powyżej 50 tys. zł")


png("/Users/ignacymorawski/Downloads/gielda5b.png",width=350,height=200,units="mm",bg = "white",res=300)
print(bp5)
dev.off()








#### Zaciąganie informacji o spółkach

spolki<-sort(unique(c(as.character(turn0$spolka),
                      as.character(m4$spolka),
                      as.character(mc4$spolka)
                      )
                    )
             )

linkiG<-theurl10 %>%  html_nodes("a") %>% html_attr("href")
linkiN<-theurl20 %>%  html_nodes("a") %>% html_attr("href")

for (i in seq(length(spolki))){
  tryCatch({

linki.n<-grep(tolower(spolki[i]),linkiG)
if (length(linki.n)==0){
  linki.n<-grep(tolower(spolki[i]),linkiN)
  spx<-paste0("https://notowania.pb.pl",linkiN[linki.n])
} else {
  spx<-paste0("https://notowania.pb.pl",linkiG[linki.n])
}

    
#54-61
#spx<-paste0("https://notowania.pb.pl",linki[linki.n])

sph<-read_html(spx)
spl<-sph %>% html_nodes("a") %>% html_attr("href")
spt<-sph %>% html_nodes("a") %>% html_text()
spl2<-spl[54:61]
spt2<-spt[54:61]
# spl3<-spl2[substr(spl2,1,1)=="/"]
# spt3<-spt2[substr(spl2,1,1)=="/"]
# spl4<-spl3[-grep("Dywidendy",spl3)]
# spt4<-spt3[-grep("Dywidendy",spl3)]

linkA<-read_html(spl2[1])
linkB<-linkA %>% html_nodes("[class='npb-a-h-d-pub-date']") %>% html_text()
dateX<-trimws(linkB)[1]
dateA<-paste(substr(dateX,nchar(dateX)-10,nchar(dateX)-7),
             substr(dateX,nchar(dateX)-13,nchar(dateX)-12),
             substr(dateX,nchar(dateX)-16,nchar(dateX)-15),
             sep="-"
)

if (as.Date(dateA)>dzien-2){
cat("####", spolki[i], "\n")

for (j in seq(length(spl2))){
  tryCatch({
link<-read_html(spl2[j])
#link2<-link %>% html_nodes("h1") %>% html_text()
link3<-link %>% html_nodes("p") %>% html_text()
link4<-link %>% html_nodes("span") %>% html_text()

datex<-trimws(link4[grep("opublikowano",link4)[1]])
date<-paste(substr(datex,nchar(datex)-10,nchar(datex)-7),
             substr(datex,nchar(datex)-13,nchar(datex)-12),
             substr(datex,nchar(datex)-16,nchar(datex)-15),
             sep="-"
)

if (as.Date(date)>dzien-2){
cat("######", link3[2], "\n")
cat("**", date, "**", "\n")
cat(link3[3], "\n")
cat("<br></br>","\n")
}
}, error= function(e) {})
}
}

  }, error= function(e) {})
}


#### Zaciągnięcie danych giełdowych z innych krajow

now1<-read_html("http://notowania.pb.pl/stocktable/GPWAKCJE")
now2<-now1 %>% html_nodes("time") %>% html_text() %>% substr(.,15,24)
today<-as.Date(now2)

download.file(url=paste0("https://stooq.pl/db/d/?d=",gsub("-","",today),"&t=5"),destfile = "/Users/ignacymorawski/Downloads/stock.txt", method="libcurl")

checkwig<-NA
ca<-1
while(is.na(checkwig)){
download.file(url=paste0("https://stooq.pl/db/d/?d=",gsub("-","",today-ca),"&t=5"),destfile = "/Users/ignacymorawski/Downloads/stockref.txt", method="libcurl")
stock.ref<-read_delim("/Users/ignacymorawski/Downloads/stockref.txt",delim=",",col_names = TRUE)
checkwig<-which(stock.ref$`<TICKER>`=="WIG")[1]
ca<-ca+1
}


ind<-c("WIG","WIG20","SWIG80","MWIG40","NCINDEX","^SPX","^CAC","^DAX","^FTM")
name<-c("WIG","WIG20","SWIG80","MWIG40","NCINDEX","S&P500","CAC40","DAX","FTSE250")
indx<-tibble(
  `<TICKER>`=ind,
  name=name
)

stock.ref2<-stock.ref %>% 
  select(c(1,4,8)) %>% 
  filter(`<TICKER>` %in% ind) %>% 
  group_by(`<TICKER>`) %>% 
  summarise(close=tail(`<CLOSE>`,1))

time<-tibble(time=unique(stock.ref$`<TIME>`)) %>% arrange(time)

stock<-read_delim("/Users/ignacymorawski/Downloads/stock.txt",delim=",",col_names = TRUE)
stock2<- time %>% 
  left_join(stock,by=c("time"="<TIME>")) %>% 
  mutate_at(c("time"),~paste0(substr(.,1,2),":",substr(.,3,4))) %>% 
  select(c(1,2,8)) %>% 
  filter(`<TICKER>` %in% ind) %>% 
  left_join(indx,by=c("<TICKER>")) %>% 
  left_join(stock.ref2,by=c("<TICKER>")) %>% 
  mutate(change=100*`<CLOSE>`/close-100)

timex<-paste0(substr(time$time,1,2),":",substr(time$time,3,4))
time2<-which((timex %in% unique(stock2$time))==FALSE & as.numeric(substr(timex,1,2))>=9)
timex2<-timex[time2]

n1<-unique(stock2$`<TICKER>`)
n2<-length(n1)

timex3<-tibble(
  time=rep(timex2,n2),
  `<TICKER>`=rep(n1,each=length(timex2)),
  `<CLOSE>`=NA,
  name=rep(indx %>% filter(`<TICKER>` %in% n1) %>% select(name) %>% unlist,each=length(timex2)),
  close=rep(stock.ref2 %>%  filter(`<TICKER>` %in% n1) %>% select(close) %>% unlist,each=length(timex2)),
  change=NA,
)

stock3<-bind_rows(stock2,timex3)
stock3$name<-factor(stock3$name,levels=name)

q<-as.numeric(substr(tail(stock2$time,1),1,2))
if (q<16){
  stock3<-stock3 %>% filter(`<TICKER>`!="^SPX")
}

stock3$time<-as.POSIXct(paste(Sys.Date(),stock3$time,sep=" "))
stock3

bp5<-ggplot(data=stock3,aes(x=time,y=change,group=name))+
  geom_ribbon(aes(x=time,ymin=0,ymax=change),fill="gray",alpha=0.3)+
  geom_line(aes(color=change),size=0.6)+
  scale_color_gradient2(min="red",mid="gray23",high="darkolivegreen4",midpoint=0)+
  geom_hline(aes(yintercept = 0),color="red",size=0.5,linetype="dashed")+
  facet_wrap(~name,scales="free_x",ncol=5)+
  theme(strip.background = element_rect(fill="transparent",color="transparent"))+
  theme(strip.text = element_text(size=11, face="bold",color="gray23"))+
  theme(axis.ticks=element_blank())+
  theme(axis.text.y=element_text(margin = margin(t=0,r=20,b=0,l=0,unit="pt")))+
  theme(axis.text.x=element_text(size=7))+
  theme(legend.position = "none")+
  theme(plot.background = element_rect(fill="cornsilk",color="cornsilk"))+
  theme(panel.background = element_rect(fill="cornsilk",color="cornsilk"))+ 
  theme(panel.spacing = unit(2,"cm"))+
  #theme(panel.grid=element_blank())+
  theme(plot.title = element_text(color = "#EC0108",size=16,face="bold"))+
  theme(plot.subtitle = element_text(color = "gray23",size=11,margin=margin(t=0,r=0,b=25,l=0,unit="pt")))+
  theme(plot.margin=margin(t=10,r=10,b=10,l=10,unit="pt"))+
  xlab("")+
  ylab("zmiana w proc.")+
  scale_x_datetime(labels = date_format("%H:%M", tz="Europe/Berlin"), date_breaks = "3 hours")+
  labs(title="",
       subtitle="indeksy giełdowe w dniu dzisiejszym, zmiana procentowa od zamknięcia dnia poprzedniego (notowania 5min)",
       caption="źródło: Puls Biznesu, stooq.pl")


png("/Users/ignacymorawski/Downloads/gielda6.png",width=350,height=150,units="mm",bg = "white",res=300)
print(bp5)
dev.off()


### TICKERY I NAZWY ###


t<-read_html("http://infostrefa.com/infostrefa/pl/spolki")
t2<-as_tibble(html_table(t)[[2]]) %>% slice(-c(1))
colnames(t2)<-as_tibble(html_table(t)[[2]]) %>% slice(c(1))
ticker<-t2
save(ticker,file=here("./data/ticker.RData"))
 


### WSE DATA ###

wsl<-list.files(here("./data/data/stooq_daily/pl/wse stocks/"),full.names = TRUE)


for (i in c(68,1:67,69:length(wsl))){
  tryCatch({
x<-read_delim(wsl[i],delim=",")
x1<-x %>% 
  select(c(3,8)) %>% 
  `colnames<-`(c("date",toupper(substr(wsl[i],nchar(wsl[i])-6,nchar(wsl[i])-4))))
x2<-x %>%
  mutate(turn=`<CLOSE>`*`<VOL>`) %>% 
  select(c(3,11)) %>% 
  `colnames<-`(c("date",toupper(substr(wsl[i],nchar(wsl[i])-6,nchar(wsl[i])-4))))

if (i==68){
  ws1<-x1
  ws2<-x2
} else {
  ws1<-ws1 %>% left_join(x1,by=c("date"))
  ws2<-ws2 %>% left_join(x2,by=c("date"))
}

},error=function(e){})
  
  print(i)
  flush.console()
  
}

load(here("./data/ticker.RData"))

m3<-match(colnames(ws1),ticker$Ticker)
m4<-ifelse(is.na(m3),colnames(ws1),ticker$`Nazwa giełdowa`[m3])
colnames(ws1)<-m4
colnames(ws2)<-m4

ws1$date<-as.Date(paste(substr(ws1$date,1,4),substr(ws1$date,5,6),substr(ws1$date,7,8),sep="-"))
ws2$date<-as.Date(paste(substr(ws2$date,1,4),substr(ws2$date,5,6),substr(ws2$date,7,8),sep="-"))

save(ws1,ws2,file=here("/data/wse.RData"))


### WSE CONNECT DATA ###



ncl<-list.files(here("./data/data/stooq_daily/pl/nc stocks/"),full.names = TRUE)


for (i in seq(length(ncl))){
  tryCatch({
    x<-read_delim(ncl[i],delim=",")
    x1<-x %>% 
      select(c(3,8)) %>% 
      `colnames<-`(c("date",toupper(substr(ncl[i],nchar(ncl[i])-6,nchar(ncl[i])-4))))
    x2<-x %>%
      mutate(turn=`<CLOSE>`*`<VOL>`) %>% 
      select(c(3,11)) %>% 
      `colnames<-`(c("date",toupper(substr(ncl[i],nchar(ncl[i])-6,nchar(ncl[i])-4))))
    
    if (i==1){
      nc1<-x1
      nc2<-x2
    } else {
      nc1<-nc1 %>% left_join(x1,by=c("date"))
      nc2<-nc2 %>% left_join(x2,by=c("date"))
    }
    
  },error=function(e){})
  
  print(i)
  flush.console()
  
}

load(here("./data/ticker.RData"))

m<-match(colnames(nc1),ticker$Ticker)
m2<-ifelse(is.na(m),colnames(nc1),ticker$`Nazwa giełdowa`[m])
colnames(nc1)<-m2
colnames(nc2)<-m2

nc1$date<-as.Date(paste(substr(nc1$date,1,4),substr(nc1$date,5,6),substr(nc1$date,7,8),sep="-"))
nc2$date<-as.Date(paste(substr(nc2$date,1,4),substr(nc2$date,5,6),substr(nc2$date,7,8),sep="-"))

save(nc1,nc2,file=here("/data/nc.RData"))



### UZUPEŁNIANIE DANYCH NC I WSE ###

load(here("./data/wse.RData"))
load(here("./data/nc.RData"))


lwse<-tail(ws1$date,1)
lwse2<-format(seq(as.Date(lwse)+1,Sys.Date(),by="day"))

for (i in seq(length(lwse2))){
u<-paste0("https://www.gpw.pl/price-archive?fetch=0&type=10&instrument=&date=",format(as.Date(lwse2[i]),"%d-%m-%Y"),"&show_x=Show+results")
u2<-read_html(u)
u3<-html_table(u2)

if(length(u3)>1){
  u4<-as_tibble(u3[[2]])
  u5<-t(u4$`Closing price`)
  u6<-as_tibble(cbind(lwse2[i],u5))
  colnames(u6)<-c("date",u4$Name %>% unlist)
  u7<-u6 %>% mutate_at(c(1),~as.Date(.)) %>% mutate_at(c(-1),~as.numeric(gsub(",","",.)))
  ws1<-bind_rows(ws1,u7)
  
  
  u5b<-t(u4$`Turnover value (thou.)`)
  u6b<-as_tibble(cbind(lwse2[i],u5b))
  colnames(u6b)<-c("date",u4$Name %>% unlist)
  u7b<-u6b %>% mutate_at(c(1),~as.Date(.)) %>% mutate_at(c(-1),~1000*as.numeric(gsub(",","",.)))
  ws2<-bind_rows(ws2,u7b)
}
}

save(ws1,ws2,file=here("/data/wse.RData"))


lnce<-tail(nc1$date,1)
lnce2<-format(seq(as.Date(lnce)+1,Sys.Date(),by="day"))
lnce3<-gsub("-","",lnce2)

urlFileExist <- function(url){
  HTTP_STATUS_OK <- 200
  hd <- httr::HEAD(url)
  status <- hd$all_headers[[1]]$status
  list(exists = status == HTTP_STATUS_OK, status = status)
}

for (i in seq(length(lnce2))){
  
  exists<-urlFileExist(paste0("https://newconnect.pl/pub/NEWCONNECT/statystyki/statystyki_dzienne/NC_",lnce3[i],".xls"))
  
  if (exists$exists==TRUE){
  
  download.file(paste0("https://newconnect.pl/pub/NEWCONNECT/statystyki/statystyki_dzienne/NC_",lnce3[i],".xls"),
                destfile=here(paste0("./data/ncq/NC_",lnce3[i],".xls")))
  
  u0<-read_excel(here(paste0("./data/ncq/NC_",lnce3[i],".xls")),3,skip=5,col_names = TRUE)

  u2<-u0 %>% 
    select(Security,Closing,Trading...24) %>% 
    mutate_at(c(2,3),~as.numeric(.)) %>% 
    drop_na %>% 
    `colnames<-`(c("Security","Closing","Turnover"))
  
  if(length(u2)>1){
    u4<-t(u2$Closing)
    u5<-as_tibble(cbind(lnce2[i],u4))
    colnames(u5)<-c("date",u2$Security)
    u6<-u5 %>% mutate_at(c(1),~as.Date(.)) %>% mutate_at(c(-1),~as.numeric(gsub(",","",.)))
    nc1<-bind_rows(nc1,u6)
    
    u4b<-t(u2$Turnover)
    u5b<-as_tibble(cbind(lnce2[i],u4b))
    colnames(u5b)<-c("date",u2$Security)
    u6b<-u5b %>% mutate_at(c(1),~as.Date(.)) %>% mutate_at(c(-1),~as.numeric(gsub(",","",.)))
    nc2<-bind_rows(nc2,u6b)
  }
  
  }
}


save(nc1,nc2,file=here("/data/nc.RData"))



### Ładowanie bieżących danych GPW i New Connect




