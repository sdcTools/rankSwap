## loop over hierarchies
# permute all risky households in each hierarchy level
# in last level permute remaining number of needed households (if still > 0)
# function to compare with c++ version

#' Title
#'
#' @param dat 
#' @param hierarchy 
#'
#' @return
#' @export
#'
#' @examples
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