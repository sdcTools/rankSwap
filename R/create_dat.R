###########################################
# help functions
#

# create dummy data
create.dat <- function(N=10000){
  nuts1 <- sample(1:5,N,replace=TRUE)
  nuts2 <- sample(1:10,N,replace=TRUE)
  nuts3 <- sample(1:15,N,replace=TRUE)
  nuts4 <- sample(1:25,N,replace=TRUE)
  hsize <- sample(1:6,N,replace=TRUE)
  
  # replicate
  hid <- rep(1:length(hsize),times=hsize)
  nuts1 <- rep(nuts1,times=hsize)
  nuts2 <- rep(nuts2,times=hsize)
  nuts3 <- rep(nuts3,times=hsize)
  nuts4 <- rep(nuts4,times=hsize)
  hsize <- rep(hsize,times=hsize)
  geschl <- sample(c(1,2),length(hsize),replace=TRUE)
  ageGroup <- sample(1:7,length(hsize),replace=TRUE)
  national <- sample(1:5,length(hsize),replace=TRUE)
  
  dat <- data.table(nuts1,nuts2,nuts3,nuts4,hid,hsize,ageGroup,geschl,national)
  return(dat)
}

# resample integer
resample <- function(x, ...){
  x[sample.int(length(x), ...)]
}

# random rounding
randomRound <- function(x){
  if(length(x)==1){
    if(x<1){
      return(ceiling(x))
    }else{
      if(sample(c(0,1),1)==1){
        return(ceiling(x))
      }else{
        return(floor(x))
      }
    }
  }else{
    
    x_diff <- sum(x)-floor(x)
    up_down <- rep(FALSE,length(x))
    up_down[1:x_diff] <- TRUE
    
    up_down <- sample(up_down)
    x[up_down] <- ceiling(x[up_down])
    x[!up_down] <- floor(x[!up_down])
    return(x)
  }
  
}


## loop over hierarchies
# permute all risky households in each hierarchy level
# in last level permute remaining number of needed households (if still > 0)
# function to compare with c++ version
recordSwapR <- function(dat,hierarchy){
  
  dat[,SELECTED:=0]
  dat[,ID:=.I]
  nhier <- length(hierarchy)
  for( i in 1:length(hierarchy)){
    hier.i <- unique(dat[,c(hierarchy[1:i]),with=FALSE])
    
    if(i<nhier){
      # upper stages
      for(h in 1:nrow(hier.i)){
        select.ih <- dat[hier.i[h],on=colnames(hier.i)][level==i&SELECTED==0,ID]
        swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][SELECTED==0,.(ID,antirisk)]
        antirisk <- swap.with[[2]]
        swap.with <- swap.with[[1]]
        
        n_select <- length(select.ih)
        n_swap.with <- length(swap.with)
        
        if(n_select>0&n_swap.with>0){
          swap.with <- resample(swap.with,min(n_select,n_swap.with),prob=antirisk)
          select.ih <- select.ih[1:length(swap.with)]
          select.geo <- dat[select.ih,mget(hierarchy)]
          swap.with.geo <- dat[swap.with,mget(hierarchy)]
          dat[select.ih,c(hierarchy):=swap.with.geo]
          dat[swap.with,c(hierarchy):=select.geo]
          dat[c(select.ih,swap.with),SELECTED:=1]
          dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"hsize")]
        }
      }
    }else{
      # finale stage
      for(h in 1:nrow(hier.i)){
        select.ih <- dat[hier.i[h],on=colnames(hier.i)][SELECTED==0,if(.N>0){resample(ID,max(drawN[1],sum(level==nhier)),prob=risk)}else{NULL}]
        if(length(select.ih)>0){
          swap.with <- dat[!hier.i[h],on=colnames(hier.i)[1:i]][SELECTED==0,.(ID,antirisk)]
          antirisk <- swap.with[[2]]
          swap.with <- swap.with[[1]]
          
          n_select <- length(select.ih)
          n_swap.with <- length(swap.with)
          
          if(n_select>0&n_swap.with>0){
            swap.with <- resample(swap.with,min(n_select,n_swap.with),prob=antirisk)
            select.ih <- select.ih[1:length(swap.with)]
            select.geo <- dat[select.ih,mget(hierarchy)]
            swap.with.geo <- dat[swap.with,mget(hierarchy)]
            dat[select.ih,c(hierarchy):=swap.with.geo]
            dat[swap.with,c(hierarchy):=select.geo]
            dat[c(select.ih,swap.with),SELECTED:=1]
            dat[,drawN:=pmax(0,drawN-sum(SELECTED)),by=c(hierarchy,"hsize")]
          }
        }
      }
    }
  }
  return(dat)
}