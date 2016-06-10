df<-as.matrix(pddata)
sum <- matrix(0, ncol=2, nrow = 2000)

#function ll_rl
ll_rl<-function(h){
for (i in 0:19){
  for(j in 1:99){
    last<-100*i+j
    if (df[last,1] == 0) {
      sum[last+1,1] = sum[last,1] + df[last,4]
      sum[last+1,2] = sum[last,2]
    }
    else {
      sum[last+1,1] = sum[last,1]
      sum[last+1,2] = sum[last,2]+ df[last,4]
    }
  }
}
ll = 0
for (i in 0:19){
  for (j in 1:99){
    if (df[j+1,1] == 0) {
      ll=h*sum[j,1]-log(exp(h * sum[j,1] ) +exp(h * sum[j,2])) + ll
    }
    else {
      ll=h*sum[j,2]-log(exp(h * sum[j,1] ) +exp(h * sum[j,2])) + ll
    }
  }
}
return(-ll)
}

llresult<-nlminb(start = 0, objective = ll_rl, lower = 0, upper = Inf)


ll_rl_MSE<-function(){
  for (i in 0:19){
    for(j in 1:99){
      last<-100*i+j
      if (df[last,1] == 0) {
        sum[last+1,1] = sum[last,1] + df[last,4]
        sum[last+1,2] = sum[last,2]
      }
      else {
        sum[last+1,1] = sum[last,1]
        sum[last+1,2] = sum[last,2]+ df[last,4]
      }
    }
  }
  h = 0.199
  for (i in 0:19){
    for (j in 1:99){
      if (df[j+1,1] == 0) {
        MSE_temp=(exp(h*sum[j,1])/(exp(h * sum[j,1] ) +exp(h * sum[j,2])) -1)^2
      }
      else {
        MSE_temp=(exp(h*sum[j,1])/(exp(h * sum[j,1] ) +exp(h * sum[j,2])))^2
      }
    }
  }
  MSE = MSE_temp / 2000
  return(MSE)
}
serror = ll_rl_MSE()



#function ll_rl2
ll_rl2<-function(par){
  for (i in 0:19){
    for(j in 1:99){
      last<-100*i+j
      if (df[last,1] == 0) {
        sum[last+1,1] = par[1]*sum[last,1] + df[last,4]
        sum[last+1,2] = par[1]*sum[last,2]
      }
      else {
        sum[last+1,1] = par[1]*sum[last,1]
        sum[last+1,2] = par[1]*sum[last,2]+ df[last,4]
      }
    }
  }
  ll=0
  for (i in 0:19){
    for (j in 1:99){
      if (df[j+1,1] == 0) {
        ll=par[2]*sum[j,1]-log(exp(par[2] * sum[j,1] ) +exp(par[2] * sum[j,2])) + ll
      }
      else {
        ll=par[2]*sum[j,2]-log(exp(par[2] * sum[j,1] ) +exp(par[2] * sum[j,2])) + ll
      }
    }
  }
  return(-ll)
}
  llresult2<-nlminb(c(0,0), ll_rl2, lower = c(0,0), upper = c(1,Inf))
  
ll_rl2_MSE<-function(){
  
  par = c(0.944,0.77)
  for (i in 0:19){
    for(j in 1:99){
      last<-100*i+j
      if (df[last,1] == 0) {
        sum[last+1,1] = par[1]* sum[last,1] + df[last,4]
        sum[last+1,2] = par[1]* sum[last,2]
      }
      else {
        sum[last+1,1] = par[1]* sum[last,1]
        sum[last+1,2] = par[1]* sum[last,2]+ df[last,4]
      }
    }
  }
  
  for (i in 0:19){
    for (j in 1:99){
      if (df[j+1,1] == 0) {
        MSE_temp=(exp(par[2]*sum[j,1])/(exp(par[2] * sum[j,1] ) +exp(par[2] * sum[j,2])) -1)^2
      }
      else {
        MSE_temp=(exp(par[2]*sum[j,1])/(exp(par[2] * sum[j,1] ) +exp(par[2]* sum[j,2])))^2
      }
    }
  }
  MSE = MSE_temp / 2000
  return(MSE)
}
serror2 = ll_rl2_MSE()