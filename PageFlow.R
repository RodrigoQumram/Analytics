wv2 <- strsplit(webs$url,";") #split jsession
webs$clean_url <- sapply(wv2,"[",1)
webs$domain <- sapply(strsplit(webs$clean_url,".ch",fixed=T),"[",1) 
webs$path <- sapply(strsplit(webs$clean_url,".ch",fixed=T),"[",2) 
table(sapply(strsplit(webs$captureTime,".",fixed=T),"[",2) )
webs$time <- as.numeric(as.POSIXct(webs$captureTime,format="%Y-%m-%dT%H:%M:%S.000Z"))
kk <- webs
setnames(kk,"sessionId","session")
kk <- kk[,.(time=min(time)),by=c("session","path")]
kk$path <- gsub("/proposal/pages/","",kk$path,fixed=T)
kk$path <- gsub(".mobi","",kk$path,fixed=T)
kk2 <- kk[!is.na(path)&path!="exception/maintenance",.(count=.N),by=c("session","path","time")]
kk2[,.(count=sum(count)),path]
kk2 <- kk2[,order:=rank(time),session]

kk3 <- kk2[,.(count=.N),by=c("order","path")] 
kk3[order==1,percent(count/sum(kk3[order==1,]$count)),by=path]
kk3[,max:=max(count),by=c("order")] 
kk3 <- kk3[count==max,]
setnames(kk3,"order","flow")
# add common path to sessions
kk4 <- merge(kk2,kk3[,c("path","flow"),with=F],by="path")
# check how many session comply
kk4 <- kk4[,.(ordered=sum(flow==order),total=.N),by=session]
kk4[,.(sum(ordered==total)*100/.N),]