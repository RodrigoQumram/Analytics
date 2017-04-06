# microbenchmark
sp <- function(){
  kk <- form[!path %in% c("","/proposal/test.mobi","/proposal/pages/carProposal.mobi"),]
  setorder(kk,"session","time")
  kk$time <- as.integer(kk$time/1000) %% 100000000
  kk <- kk[,.(time=min(time)),by=c("session","path")]
  # kk <- kk[!duplicated(kk[,c("session","path"),with=F]),]
  kk$path <- gsub("/proposal/pages/","",kk$path,fixed=T)
  kk$path <- gsub(".mobi","",kk$path,fixed=T)
  kk2 <- kk[!is.na(path),.(count=.N),by=c("session","path","time")]
  kk2[,.(count=sum(count)),path]
  kk2 <- kk2[,order:=rank(time),session]
  
  #automatic
  #determine path
  kk3 <- kk2[,.(count=.N),by=c("order","path")] 
  kk3[order==1,percent(count/sum(kk3[order==1,]$count)),by=path]
  kk3[,max:=max(count),by=c("order")] 
  kk3 <- kk3[count==max,]
  setnames(kk3,"order","flow")
  form_flow <- kk3
  # add common path to sessions
  kk4 <- merge(kk2,kk3[,c("path","flow"),with=F],by="path")
  # check how many session comply
  kk4 <- kk4[,.(ordered=sum(flow==order),total=.N),by=session]
  print(kk4[,.(sum(ordered==total)*100/.N),])
}