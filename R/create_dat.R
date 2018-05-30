###########################################
# help functions
#

# create dummy data
create.dat <- function(N=10000){
  nuts1 <- sample(1:5,N,replace=TRUE)
  nuts2 <- sample(1:7,N,replace=TRUE)
  nuts3 <- sample(1:15,N,replace=TRUE)
  hsize <- sample(1:6,N,replace=TRUE)
  
  # replicate
  hid <- rep(1:length(hsize),times=hsize)
  nuts1 <- rep(nuts1,times=hsize)
  nuts2 <- rep(nuts2,times=hsize)
  nuts3 <- rep(nuts3,times=hsize)
  hsize <- rep(hsize,times=hsize)
  geschl <- sample(c(1,2),length(hsize),replace=TRUE)
  ageGroup <- sample(1:7,length(hsize),replace=TRUE)
  national <- sample(1:5,length(hsize),replace=TRUE)
  
  dat <- data.table(nuts1,nuts2,nuts3,hsize,hid,ageGroup,geschl,national)
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
