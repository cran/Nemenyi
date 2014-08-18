pairwise.nemenyi.test <-
function(matrix,digits){
  
  #Get number of domains and classifiers to be compared
  nDomains<-nrow(matrix)
  kClassifiers<-ncol(matrix)
  #Calculate degrees of freedom
  dof <- (nDomains-1)*(kClassifiers-1)
  
  #Calculate Friedman-type ranks for all classifiers (columns) and one domain
  ranks <- t(apply(matrix, 1, getRanks, ndigits=digits))
  
  #Calc sum of ranks
  sumRanks <- apply(ranks,2,sum)
  
  #Calculate the divisor which makes the nemenyi values compatible with the studentized range (q)-distribution
  divisor<-sqrt((kClassifiers*(kClassifiers+1))/(6*nDomains))
  
  #Perform pairwise comparisons of entities x,y (columns) and return p-value
  compare.p <- function(i,j){
    qStatistic <- abs(sumRanks[i]-sumRanks[j])/divisor
    #Multiply by sqrt(2) to make this statistics compatible with studentized Range statistic
    qStatistic <- qStatistic * sqrt(2) 
    p<-(1-ptukey(qStatistic,kClassifiers,dof))
    return(p)
  }
  
  #Perform pairwise comparisons of entities x,y (columns) and return nemenyi value
  compare.stats <- function(i,j){
    qStatistic <- abs(sumRanks[i]-sumRanks[j])/divisor
    qStatistic <- qStatistic * sqrt(2)    
    return(qStatistic)
  }
  
  METHOD <- "Nemenyi post-hoc test"
  DNAME <- paste("Performance measures of entities 1..",kClassifiers," over domains 1..",nDomains,sep="")
  PVAL <- pairwise.table(compare.p,c(1:kClassifiers), "none")
  PSTAT <- pairwise.table(compare.stats,c(1:kClassifiers), "none")
  PARAMS <- dof
  names(PARAMS)="df"
  PADJ <- "none"
  ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, statistic = PSTAT, parameter = PARAMS, p.adjust.method=PADJ)
  class(ans) <- "pairwise.htest"
  ans
  
}