mteam<-function(x){
    y<-character(length(x))
    for(i in seq(x)){
        if(x[i]==""){
            y[i]<-"Khimik"
        }
        else{
            y[i]<-x[i]
            print(y[i])
        }
    }
    return(y)
}