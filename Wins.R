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