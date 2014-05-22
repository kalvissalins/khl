getData<-function(f,l){
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
                     HomeTSR=numeric(1-f+1),
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
    }
    data$HomeTeam<-factor(data$HomeTeam)
    data$AwayTeam<-factor(data$AwayTeam)
    data$HomeGoals<-as.numeric(data$HomeGoals)
    data$HomeShots<-as.numeric(data$HomeShots)
    data$AwayGoals<-as.numeric(data$AwayGoals)
    data$AwayShots<-as.numeric(data$AwayShots)
    data$HomeTSR<-data$HomeShots/(data$HomeShots+data$AwayShots)
    data$AwayTSR<-data$AwayShots/(data$HomeShots+data$AwayShots)
    return(data)
}

HomeWins<-function(home,away){
    wins<-factor(levels=c("win","draw","loss"))
    for(i in seq(home)){
        if(home[i]>away[i]){
            wins[i]<-"win"
        }
        else if(home[i]<away[i]){
            wins[i]<-"loss"
        }
        else{
            wins[i]<-"draw"
        }
    }
    return(wins)
}


url<-"http://en.khl.ru/report/244/?idgame=22316"
source<-getURL(url)
parsed<-htmlParse(source)
HomeTeam<-xpathSApply(parsed, "//h2",xmlValue)[1]
AwayTeam<-xpathSApply(parsed, "//h2",xmlValue)[2]
tds<-xpathSApply(parsed, "//td",xmlValue)
x<-which(tds=="summary")
HGoals<-tds[x+1]
HShots<-tds[x+2]
AGoals<-tds[x+5]
AShots<-tds[x+6]
Date<-date<-xpathSApply(parsed, "//p",xmlValue)[1]


data$HomePDO<-round((data$HomeGoals/data$HomeShots+(data$AwayShots-data$AwayGoals)/data$AwayShots)*1000)
data$AwayPDO<-2000-data$HomePDO


data2013$HomeTeam<-factor(data2013$HomeTeam)
data2013$AwayTeam<-factor(data2013$AwayTeam)
data2013$HomeGoals<-as.numeric(data2013$HomeGoals)
data2013$HomeShots<-as.numeric(data2013$HomeShots)
data2013$AwayGoals<-as.numeric(data2013$AwayGoals)
data2013$AwayShots<-as.numeric(data2013$AwayShots)

HomePDO<-round((HomeGoals/HomeShots+(AwayShots-AwayGoals)/AwayShots)*1000),
AwayPDO<-2000-HomePDO,
HomeTSR<-HomeShots/(HomeShots+AwayShots),
AwayTSR<-AwayShots/(HomeShots+AwayShots))



Winslosses<-function(home,away){
         wins<-factor(levels=c("win","draw","loss"))
         for(i in seq(home)){
             if(home[i]>away[i]){
                 wins[i]<-"win"
             }
             else if(home[i]<away[i]){
                 wins[i]<-"loss"
             }
             else{
                 wins[i]<-"draw"
             }
         }
         return(wins)
     }
with(data2013,
HomeWins<-Winslosses(HomeGoals,AwayGoals),
AwayWins<-Winslosses(AwayGoals,HomeGoals))




