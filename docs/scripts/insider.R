library(XML)
library(RCurl)
library(rlist)
library(XLConnect)
library(tidyverse)
library(readxl)
library(rvest)
library(stringr)
library(zoo)
library(treemapify)
library(plotly)
library(htmlwidgets)
library(here)



### SZUKANIE INFORMACJI O TRANSAKCJACH INSIDERÓW - MAR ###

z<-NULL

for (t in seq(80)){
list1<-read_html(paste0("https://www.bankier.pl/gielda/wiadomosci/komunikaty-spolek/",t))
check<-list1 %>% html_nodes("[class='entry-title']") %>% html_nodes("a") %>% html_attr("href")

list4<-check


if(length(list4)>0){
for (i in seq(length(list4))){
list5<-read_html(paste0("https://www.bankier.pl",list4[i]))
list6txt<-list5 %>% html_nodes("[class='o-article-content']") %>% html_text()
ver<-gsub(" ","",gsub("\\.","",tolower(list6txt)))

if(gregexpr("art19",ver)[[1]][1]>0){

list6tim<-list5 %>% html_nodes("[class='a-span']") %>% html_text()
list6ttl<-list5 %>% html_nodes("h1") %>% html_text()
list6tab<-list5 %>% html_table()
list6lnk<-list5 %>% html_nodes("a") %>% html_attr("href")
file<-list6lnk[grep("pdf",list6lnk)]


if (length(file)<4){
  txt<-"Brak pdfa"
  txt2<-"Brak pdfa"
} else {
  file0<-file[1]
  download.file(paste0("https://www.bankier.pl",file0),destfile = paste0("/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/pdf/",tail(str_split(file0,"/")[[1]],1)))
  b1<-readtext(paste0("/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/pdf/",tail(str_split(file,"/")[[1]],1)))
  cb1<-corpus(b1)
  
  m<-gregexpr("Rodzaj transakcji",cb1[1])[[1]][1]
  if (m>0){
  txt<-as.character(gsub(" ","",substr(cb1[1],m,m+200)))
  } else{
  txt<-"Brak możliwości odczytu rodzaju transakcji"
  }
  
  m2<-gregexpr("Łączny wolumen",cb1[1])[[1]][1]
  if (m2>0){
    txt2<-as.character(gsub(" ","",substr(cb1[1],m2,m2+200)))
  } else{
    txt2<-"Brak możliwości odczytu wolumenu"
  }
  
  m3<-gregexpr("Opis instrumentu",cb1[1])[[1]][1]
  if (m3>0){
    txt3<-as.character(gsub(" ","",substr(cb1[1],m3,m3+200)))
  } else{
    txt3<-"Brak możliwości odczytu rodzaju instrumentu"
  }
  
  
  m4<-gregexpr("Miejsce transakcji",cb1[1])[[1]][1]
  if (m4>0){
    txt4<-as.character(gsub(" ","",substr(cb1[1],m4,m4+300)))
  } else{
    txt4<-"Brak możliwości odczytu miejsca transakcji"
  }
  
}


z0<-tibble(
  time=list6tim,
  title=list6ttl,
  link=paste0("https://www.bankier.pl",list4[i]),
  transaction=txt,
  volume=txt2,
  instrument=txt3,
  place=txt4
)

z<-bind_rows(z,z0)

} ### Tu się kończy warunek, czy w informacji o transakcjach jest artykuł 19
} ### Tu się kończy pętla przez linki ze strony z komunikatami 
} ### Tu się kończy warunek, czy na danej stronie Bankiera są informacje o transakcjach

print(t)
flush.console()
}



z.short<-z

wb<-loadWorkbook("/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/excel/insider_short.xlsx")
writeWorksheet(wb,z.short,sheet=1)
saveWorkbook(wb)



### SZUKANIE INFORMACJI O TRANSAKCJACH INSIDERÓW POPRZEZ PROFILE SPÓŁEK ###

#LISTA SPÓŁEK Z RYNKU GŁÓWNEGO
theurl10<-read_html("http://notowania.pb.pl/stocktable/GPWAKCJE")
tables0<-as_tibble(html_table(theurl10)[[1]])
spolki<-tables0$`Nazwa AD` %>% unlist
spolki<-spolki[nchar(spolki)>0]


s<-spolki[3]
i=30


z<-NULL
for (s in spolki){
  
  tryCatch({
    
  check<-1
  t=1
  
  while(length(check>0)){
  
  html<-paste0("https://www.bankier.pl/gielda/notowania/akcje/",s,"/komunikaty/",t,"?start_dt=2017-01-01&end_dt=2020-11-15")
  list1<-read_html(html)
  list2<-list1 %>% html_nodes("a") %>% html_attr("href")
  list3<-list2[grep("wiadomosc",list2)]
  
  t<-t+1
  check<-list1 %>% html_nodes("[class='entry-title']") %>% html_nodes("a") %>% html_attr("href")
  
  list4<-check

  if(length(list4)>0){
    for (i in seq(length(list4))){
      list5<-read_html(paste0("https://www.bankier.pl",list4[i]))
      list6txt<-list5 %>% html_nodes("[class='o-article-content']") %>% html_text()
      ver<-gsub(" ","",gsub("\\.","",tolower(list6txt)))
      
      if(gregexpr("art19",ver)[[1]][1]>0){
        
        list6tim<-list5 %>% html_nodes("[class='a-span']") %>% html_text()
        list6ttl<-list5 %>% html_nodes("h1") %>% html_text()
        list6tab<-list5 %>% html_table()
        list6lnk<-list5 %>% html_nodes("a") %>% html_attr("href")
        file<-list6lnk[grep("pdf",list6lnk)]
        
        
        if (length(file)<4){
          txt<-"Brak pdfa"
          txt2<-"Brak pdfa"
        } else {
          file0<-file[1]
          download.file(paste0("https://www.bankier.pl",file0),destfile = paste0("/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/pdf/",tail(str_split(file0,"/")[[1]],1)))
          b1<-readtext(paste0("/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/pdf/",tail(str_split(file,"/")[[1]],1)))
          cb1<-corpus(b1)
          
          m<-gregexpr("Rodzaj transakcji",cb1[1])[[1]][1]
          if (m>0){
            txt<-as.character(gsub(" ","",substr(cb1[1],m,m+200)))
          } else{
            txt<-"Brak możliwości odczytu rodzaju transakcji"
          }
          
          m2<-gregexpr("Łączny wolumen",cb1[1])[[1]][1]
          if (m2>0){
            txt2<-as.character(gsub(" ","",substr(cb1[1],m2,m2+200)))
          } else{
            txt2<-"Brak możliwości odczytu wolumenu"
          }
          
          m3<-gregexpr("Opis instrumentu",cb1[1])[[1]][1]
          if (m3>0){
            txt3<-as.character(gsub(" ","",substr(cb1[1],m3,m3+200)))
          } else{
            txt3<-"Brak możliwości odczytu rodzaju instrumentu"
          }
          
          
          m4<-gregexpr("Miejsce transakcji",cb1[1])[[1]][1]
          if (m4>0){
            txt4<-as.character(gsub(" ","",substr(cb1[1],m4,m4+300)))
          } else{
            txt4<-"Brak możliwości odczytu miejsca transakcji"
          }
          
        }
        
        
        z0<-tibble(
          time=list6tim,
          title=list6ttl,
          link=paste0("https://www.bankier.pl",list4[i]),
          transaction=txt,
          volume=txt2,
          instrument=txt3,
          place=txt4
        )
        
        z<-bind_rows(z,z0)
        
      } ### Tu się kończy warunek, czy w informacji o transakcjach jest artykuł 19
    } ### Tu się kończy pętla przez linki ze strony z komunikatami 
  } ### Tu się kończy warunek, czy na danej stronie Bankiera są informacje o transakcjach
  
  print(s)
  print(t)
  flush.console()
  
  }
  
  }, error= function(e) {})
}


save(z.long,file="/Users/ignacymorawski/Documents/SPOTDATA/PB FORECAST/AUTOMATIC/data/insider_long.RData")


