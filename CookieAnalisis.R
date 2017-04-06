kk <- co[1:1000000,]
kk$co1 <- strsplit(kk$cookie,";")
kk$co2 <- strsplit(sapply(kk$co1,"[",1),"=")
kk$co2Var <-sapply(kk$co2,"[",1)
kk$co2Val <-sapply(kk$co2,"[",2)
kk$co3 <- strsplit(sapply(kk$co1,"[",2),"=")
kk$co3Var <-sapply(kk$co3,"[",1)
kk$co3Val <-sapply(kk$co3,"[",2)
kk$value <- ifelse(kk$co2Var=="_ga",kk$co2Val,kk$co3Val)
kk2 <- kk[,.(count=lu(value)),by=sessionId]
kk3 <- kk[,.(count=lu(sessionId)),by=value]
kk4 <- form[session %in% kk2[count>2,]$sessionId,]
qplot(data=form[session %in% kk2[count>7,]$sessionId,],date,fill=session)
qplot(form[session %in% co$sessionId,]$date)
# Dates by session
kk4 <- form[!duplicated(form[,c("session","date"),with=F]),]
kk4 <- kk4[,list(visits=.N,mind=min(date),maxd=max(date)),by=session]
