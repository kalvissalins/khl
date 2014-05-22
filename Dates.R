corDates<-function(x){
    dates<-seq(as.Date("1971-07-01 19:30:00"),by=1,len=length(x))
    for(i in seq(x)){
      if(i<10){
          x[i]<-substr(x[i],11,nchar(x[i]))
          dates[i]<-strptime(x[i],format="%e %B %Y, %A,")
      }
      else if(i<100){
          x[i]<-substr(x[i],12,nchar(x[i]))
          dates[i]<-strptime(x[i],format="%e %B %Y, %A,")
      }
      else{
          x[i]<-substr(x[i],13,nchar(x[i]))
          dates[i]<-strptime(x[i],format="%e %B %Y, %A,")
      }
    }
    return(dates)
}
