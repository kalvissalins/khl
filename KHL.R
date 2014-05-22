AddWins<-function(home,away){
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


data2013$HomePDO<-round((data2013$HomeGoals/data2013$HomeShots+(data2013$AwayShots-data2013$AwayGoals)/data2013$AwayShots)*1000)
data2013$AwayPDO<-2000-data2013$HomePDO
data2013$HomeTSR<-data2013$HomeShots/(data2013$HomeShots+data2013$AwayShots)
data2013$AwayTSR<-data2013$AwayShots/(data2013$HomeShots+data2013$AwayShots)

