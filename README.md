# R
aaaaaaaaaaaa
bbbbbbbbbbbbbbb

bubblesort<-function(v){
  l<-length(v)
  n<-1
  p<-1
  swaps=TRUE
  if(l==1){
    return(v)
    }else{
      while (swaps){
        swaps=FALSE
        for (i in 1:(l-1)) {
          if (v[i]>v[i+1]){
            swaps=TRUE
            p<-v[i]
            v[i]<-v[i+1]
            v[i+1]<-p
          }
        }
        
      }
      return(v)
    }
}
