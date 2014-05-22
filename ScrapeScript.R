getData<-function(f,l){
    library(RCurl)
    library(XML)
    data<-data.frame(GameId=numeric(l-f+1),
                     Date=character(l-f+1),
                     HomeTeam=character(l-f+1),
                     AwayTeam=character(l-f+1),
                     HomeGoals=numeric(l-f+1),
                     HomeShots=numeric(l-f+1),
                     AwayGoals=numeric(l-f+1),
                     AwayShots=numeric(l-f+1),
                     HomePDO=numeric(l-f+1),
                     AwayPDO=numeric(l-f+1),
                     HomeTSR=numeric(l-f+1),
                     AwayTSR=numeric(l-f+1),
                     stringsAsFactors=FALSE)
    for(i in f:l){
        url<-paste("http://en.khl.ru/report/244/?idgame=",i,sep="")
        source<-getURL(url)
        parsed<-htmlParse(source)
        data$GameId[i-f+1]<-i
        data$HomeTeam[i-f+1]<-xpathSApply(parsed, "//h2",xmlValue)[1]
        data$AwayTeam[i-f+1]<-xpathSApply(parsed, "//h2",xmlValue)[2]
        tds<-xpathSApply(parsed, "//td",xmlValue)
        x<-which(tds=="summary")
        data$HomeGoals[i-f+1]<-tds[x+1]
        data$HomeShots[i-f+1]<-tds[x+2]
        data$AwayGoals[i-f+1]<-tds[x+5]
        data$AwayShots[i-f+1]<-tds[x+6]
        data$Date[i-f+1]<-date<-xpathSApply(parsed, "//p",xmlValue)[1]
        print(i)
    }
    data$HomeTeam<-factor(data$HomeTeam)
    data$AwayTeam<-factor(data$AwayTeam)
    data$HomeGoals<-as.numeric(data$HomeGoals)
    data$HomeShots<-as.numeric(data$HomeShots)
    data$AwayGoals<-as.numeric(data$AwayGoals)
    data$AwayShots<-as.numeric(data$AwayShots)
    data$HomePDO<-round((data$HomeGoals/data$HomeShots+(data$AwayShots-data$AwayGoals)/data$AwayShots)*1000)
    data$AwayPDO<-2000-data$HomePDO
    data$HomeTSR<-data$HomeShots/(data$HomeShots+data$AwayShots)
    data$AwayTSR<-data$AwayShots/(data$HomeShots+data$AwayShots)
    return(data)
}