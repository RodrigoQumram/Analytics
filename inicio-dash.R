ifelse(Sys.info()[1]=="Darwin",setwd("~/Dropbox (Qumram)/Qumram/"), setwd("C:/Users/coop/Dropbox (Qumram)/Qumram/"))

library(RCurl)
library(RGoogleAnalytics)
library(rworldmap)
library(data.table)
library(ggplot2)
library(reshape2)
library(scales)
library(ggthemes)
library(plyr)
library(RColorBrewer)
library(knitr)
library(DT)
library(lubridate)
library(RMySQL)
library(htmlTable)
library(mailR)
library(rJava)
library(stringr)
load("QData2")
Sys.setlocale("LC_TIME", "en_US.UTF-8") #for mac
Sys.setlocale("LC_TIME", "English") #for windows
options(stringsAsFactors = F)
tema <- theme_minimal()
tema$legend.position <- ""
tema$text$size=12
tema$axis.title.y=element_blank()
tema$axis.title.x=element_blank()
tema$axis.line.x=element_line(size = 0.75,color="#777777")
tema$panel.grid.major.y=element_line(linetype = 5,size=0.5,color="#bbbbbb")


#r2excel_ourfunction
#This function will insert data frames in excel files with a nice format
#df: data frame you want insert
#filename: file name where you'll insert the table
#nameforsheet: sheet name where you'll insert the table
# library("r2excel")
# excel2<-function(df,filename,nameforsheet="Sheet1")
# {
#   wb <- createWorkbook(type="xlsx")
#   sheet<- createSheet(wb, sheetName = nameforsheet)
# 
#   # Add table : add a data frame
#   n<-ncol(df)
#   xlsx.addTable(wb, sheet, df[1:1,1:n], startCol=1,col.names=TRUE,startRow=1,row.names=FALSE,fontColor="#FFFFFF",fontSize=12,rowFill=c("#000000"))
#   ######################################################
#   #Dates autodetection:
#   a<-sapply(df,class)
#   df[names(a[a=='Date'])]<-lapply(df[names(a[a=='Date'])],as.character)
#   ######################################################
#   xlsx.addLineBreak(sheet, 1)
#   m<-nrow(df)
#   xlsx.addTable(wb, sheet, df
#                 , startCol=1,col.names=FALSE,startRow=2,row.names=FALSE,
#                 fontColor="#000000", fontSize=11,
#                 rowFill=c("#FFFFFF", "#EEEEFF")
#   )
#   dataux<-data.frame(a='')
#   xlsx.addTable(wb,sheet,data=dataux,startCol=n+1,col.names=FALSE,startRow=1,row.names=FALSE,
#                 rowFill=c("#FFFFFF") )
#   xlsx.addLineBreak(sheet, 14)
#   saveWorkbook(wb, filename)
# }

# library(XLConnect)
# excel <- function(x,s,clear=T){
#   aux <- loadWorkbook(s, create=T)
#   createSheet(aux,"Data")
#   if(clear) clearSheet(aux,"Data")
#   writeWorksheet(aux,x,"Data")
#   saveWorkbook(aux)
# }
n <- function(x)  {return(colnames(x)) }
dd <- function(x) {return(data.table(x))}
monthly <- function(x) {return(as.Date(format(as.Date(x),"%Y-%m-01"),"%Y-%m-%d"))}
lu <- function(x) {return(length(unique(x)))}

monthdiff <- function(date1,date2){
  aux2 <- 1
  if(date1<date2) aux2 <- -1
  if(date1<date2) {
    date3 <- date2
    date2 <- date1
    date1 <- date3}
  aux <- as.numeric(format(as.Date(date1),"%m"))-as.numeric(format(as.Date(date2),"%m"))
  aux <- aux+12*(as.numeric(format(as.Date(date1),"%Y"))-as.numeric(format(as.Date(date2),"%Y")))
  aux <- aux-sum(as.numeric(format(as.Date(date1),"%d"))<as.numeric(format(as.Date(date2),"%d")))
  return(aux*aux2)
}
tna <- function(x){table(is.na(x))}
