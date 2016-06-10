shelling<-function(N,f,t,s){
  initial_index<-numeric()
  equilibrium_index<-numeric()
  S=0 
    one<-as.integer(f*N)
    two<-N-one
    x1<-as.numeric(runif(one))
    y1<-as.numeric(runif(one))
    x2<-as.numeric(runif(two))
    y2<-as.numeric(runif(two))
    
    n1 = length(x1)
    n2 = length(x2)
    q1e=sum(as.integer((x1<0.5)&(y1<0.5)))
    q2e=sum(as.integer((x1>=0.5)&(y1<0.5)))
    q3e=sum(as.integer((x1<0.5)&(y1>=0.5)))
    q4e=sum(as.integer((x1>=0.5)&(y1>=0.5)))
    q1o=sum(as.integer((x2<0.5)&(y2<0.5)))
    q2o=sum(as.integer((x2>=0.5)&(y2<0.5)))
    q3o=sum(as.integer((x2<0.5)&(y2>=0.5)))
    q4o=sum(as.integer((x2>=0.5)&(y2>=0.5)))
    r = 0.5*(abs(q1e/n1-q1o/n2)+abs(q2e/n1-q2o/n2)+abs(q3e/n1-q3o/n2)+abs(q4e/n1-q4o/n2))
    initial_index<-r                 
    
    r_last=1
    while(r_last!=r){                     #judge the recircle
      for(i in 1:one){                    #the distance from type 1 to everyone
        dis<-numeric()
        for(j in 1:one){
          dis[j]<-(x1[[j]]-x1[[i]])*(x1[[j]]-x1[[i]])+(y1[[j]]-y1[[i]])*(y1[[j]]-y1[[i]])
        }
        for(j in 1:two){
          dis[one+j]<-(x2[[j]]-x1[[i]])*(x2[[j]]-x1[[i]])+(y2[[j]]-y1[[i]])*(y2[[j]]-y1[[i]])
        }
        near<-integer()
        dis[which.min(dis)]<-Inf             #collect the number of closest 10
        for(j in 1:10){
          near[j]<-which.min(dis)
          dis[which.min(dis)]<-Inf
        }
        if((sum(near<=one)/10)>=t){
        }else{
          swip=TRUE                         #check if individual moves or not and circle
          while(swip){
            swip=FALSE
            x1[i]<-runif(1)
            y1[i]<-runif(1)
            for(j in 1:one){
              dis[j]<-(x1[[j]]-x1[[i]])*(x1[[j]]-x1[[i]])+(y1[[j]]-y1[[i]])*(y1[[j]]-y1[[i]])
            }
            for(j in 1:two){
              dis[one+j]<-(x2[[j]]-x1[[i]])*(x2[[j]]-x1[[i]])+(y2[[j]]-y1[[i]])*(y2[[j]]-y1[[i]])
            }
            near<-integer()
            dis[which.min(dis)]<-Inf
            for(j in 1:10){
              near[j]<-which.min(dis)
              dis[which.min(dis)]<-Inf
            }
            if((sum(near<=one)/10)>=t){
              S=S+1
            }else{
              swip=TRUE
            }
          }
        }
      }
      
      
      
      
      for(i in 1:two){                       #the distance from type 2 to everyone
        dis<-numeric()
        for(j in 1:one){
          dis[j]<-(x1[[j]]-x2[[i]])*(x1[[j]]-x2[[i]])+(y1[[j]]-y2[[i]])*(y1[[j]]-y2[[i]])
        }
        for(j in 1:two){
          dis[one+j]<-(x2[[j]]-x2[[i]])*(x2[[j]]-x2[[i]])+(y2[[j]]-y2[[i]])*(y2[[j]]-y2[[i]])
        }
        near<-integer()
        dis[which.min(dis)]<-Inf
        for(j in 1:10){
          near[j]<-which.min(dis)
          dis[which.min(dis)]<-Inf
        }
        if((sum(near>one)/10)>=t){
        }else{
          swip=TRUE
          while(swip){
            swip=FALSE
            x2[i]<-runif(1)
            y2[i]<-runif(1)
            for(j in 1:one){
              dis[j]<-(x1[[j]]-x2[[i]])*(x1[[j]]-x2[[i]])+(y1[[j]]-y2[[i]])*(y1[[j]]-y2[[i]])
            }
            for(j in 1:two){
              dis[one+j]<-(x2[[j]]-x2[[i]])*(x2[[j]]-x2[[i]])+(y2[[j]]-y2[[i]])*(y2[[j]]-y2[[i]])
            }
            near<-integer()        
            dis[which.min(dis)]<-Inf
            for(j in 1:10){
              near[j]<-which.min(dis)
              dis[which.min(dis)]<-Inf
            }
            if((sum(near>one)/10)>=t){
              S=S+1
            }else{
              swip=TRUE
            }
          }
        }
      }

      
      
      
      q1e=sum(as.integer((x1<0.5)&(y1<0.5)))
      q2e=sum(as.integer((x1>=0.5)&(y1<0.5)))
      q3e=sum(as.integer((x1<0.5)&(y1>=0.5)))
      q4e=sum(as.integer((x1>=0.5)&(y1>=0.5)))
      
      q1o=sum(as.integer((x2<0.5)&(y2<0.5)))
      q2o=sum(as.integer((x2>=0.5)&(y2<0.5)))
      q3o=sum(as.integer((x2<0.5)&(y2>=0.5)))
      q4o=sum(as.integer((x2>=0.5)&(y2>=0.5)))
      
      r_last<-r
      r = 0.5*(abs(q1e/n1-q1o/n2)+abs(q2e/n1-q2o/n2)+abs(q3e/n1-q3o/n2)+abs(q4e/n1-q4o/n2))
      if(S>s){
        r<-r_last
      }
    }
    equilibrium_index<-r                  #collect equilibrium index
  final<-data.frame(initial=initial_index,equilibrium=equilibrium_index)
  print(final)
}


