a<-1
bbbbb<-2
DF<-data.frame(Teams=character(),wins=numeric(),losses=numeric())
nrow_year<-integer()
nrow_year[1]<-0
for (l in 1961:2010){
Y<-paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf",l,"gms.txt",sep="")
  
  
file<-read.fwf(Y,widths = c(11,28,2,2,28,2))
file<-file[,-4]

Row<-nrow(file)

sumteam<-character()
for(i in 1:Row){
  sumteam[i]<-substr(file[[2]][i],1,28)
  sumteam[Row+i]<-substr(file[[4]][i],1,28)
}

sumteam<-sort(sumteam)                    #find teams that fewer than 6 games
drop_6<-character()
j=1
n=1
for(i in 1:(2*Row-1)){
  if(sumteam[i]==sumteam[i+1]){
    n=n+1
    if(i==(2*Row-1)){
      if(n<6){
        drop_6[j]=sumteam[i]
        j=j+1
      }
    }
  }else{
    if(i==(2*Row-1)){
      drop_6[j]=sumteam[i+1]
      j=j+1
    }
    if(n<6){
      drop_6[j]=sumteam[i]
      j=j+1
      n=1
    }else{
      n=1
    }
  }
}

n<-1                                                  #find the rownumber of teams need to be drop,and drop them
delete<-integer()
ndrop<-length(drop_6)
for(i in 1:ndrop){
  for(j in 1:Row){
    if(substr(file[[2]][j],1,28)==drop_6[i]){
      delete[n]<-j
      n=n+1
    }
    if(substr(file[[4]][j],1,28)==drop_6[i]){
      delete[n]<-j
      n=n+1
    }
  }
}
file<-file[-delete,]

Row=nrow(file)                                     #find the teams except those fewer than 6
Teams<-character()
T<-1
for (i in 1:Row) {
     if(substr(file[[2]][i],1,28)%in%Teams){
       }else{
        Teams[T]<-substr(file[[2]][i],1,28)
        T=T+1
     }
    if(substr(file[[4]][i],1,28)%in%Teams){
        }else{
         Teams[T]<-substr(file[[4]][i],1,28)
         T=T+1
     }
 }

nteams<-length(Teams)                                         #fill wins,losses
df<-data.frame(Teams,wins=numeric(nteams),losses=numeric(nteams))      
t<-1:nteams
names(t)<-c(Teams)

for(i in 1:Row){                                    
  if(file[[3]][i]>file[[5]][i]){                     
    tem<-t[[substr(file[[2]][i],1,28)]]
    df[[2]][tem]=df[[2]][tem]+1
    tem<-t[[substr(file[[4]][i],1,28)]]
    df[[3]][tem]=df[[3]][tem]+1
    
  }
  if(file[[3]][i]<file[[5]][i]){
    tem<-t[[substr(file[[4]][i],1,28)]]
    df[[2]][tem]=df[[2]][tem]+1
    tem<-t[[substr(file[[2]][i],1,28)]]
    df[[3]][tem]=df[[3]][tem]+1
  }
}


sumoppo<-list()                                               #fill opponents. try to O(2n),but do not know the brief function
for(i in 1:nteams){
  noppo<-1
  oppo<-integer()
  for(j in 1:Row){
    if(file[[3]][j]!=file[[5]][j]){
      if(Teams[[i]]==substr(file[[2]][j],1,28)){
        oppo[noppo]<-t[[substr(file[[4]][j],1,28)]]
        noppo<-noppo+1
      }
      if(Teams[[i]]==substr(file[[4]][j],1,28)){
        oppo[noppo]<-t[[substr(file[[2]][j],1,28)]]
        noppo<-noppo+1
      }
    }
  }
  sumoppo[[i]]<-oppo
}

sumoppo<-as.character(sumoppo)
opponent<-data.frame(opponent=sumoppo)
df<-cbind(df,opponent)


DF<-rbind(DF,df)
nrow_year[l-1959]<-nrow(DF)                                    
save(DF,file="rbindwholeyear.rdata")
}


colley=function(x){
df=DF[(nrow_year[[1961-1960]]+1):nrow_year[[1961-1959]],]
nteams<-nrow(df)  
b<-matrix(c(1+(df[,2]-df[,3])/2),nrow=nteams)
a<-diag(df[,2]+df[,3]+2)
for(i in 1:nteams){
  for(j in 1:nteams){
    if(i!=j){
      tem<-as.character(df[[4]][i])                 #"c(2,4,5)"  transfer to c(2,4,5)
      tem<-gsub("c","",tem)
      tem<-gsub(",","",tem)
      tem<-substr(tem,2,(nchar(tem)-1))
      tem<-strsplit(tem," ",fixed=TRUE)
      tem<-as.numeric(tem[[1]])
      a[i,j]=-1*(sum(tem==j))
    }
  }
}


solution<-data.frame(Teamname=df[,1],Score=solve(a,b))
FINAL<-solution[order(-solution[,2]),]
View(FINAL)
}
