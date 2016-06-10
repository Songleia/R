
selsort<-function(v){
  l=length(v)
  t<-integer(l)
  if(l==1){
    return(v)
  }
  i=1;
  while(l>=1){
    t[i]<-min(v)
    v<-v[-which.min(v)]
    l<-l-1
    i<-i+1
  }
  return(t)
}