pairwise.nemenyi.test <-
function(matrix){
  
  #Get number of domains and classifiers to be compared
  nDomains<-nrow(matrix)
  kClassifiers<-ncol(matrix)
  #Calculate degrees of freedom
  dof <- (nDomains-1)*(kClassifiers-1)
  
  #Calculate Friedman-type ranks for all classifiers (columns) and one domain
  m <- t(apply(matrix, 1, getRanks, ndigits=3))
  
  #Calc sum of ranks
  sumClassifier <- apply(m,2,sum)
  
  #Calculate the divisor which makes the nemenyi values compatible with the studentized range (q)-distribution
  d<-sqrt((kClassifiers*(kClassifiers+1))/(6*nDomains))
  
  #Perform pairwise comparisons of entities x,y (columns) and return p-value
  compare.p <- function(i,j){
    nemenyiValue <- abs(sumClassifier[i]-sumClassifier[j])/d 
    p<-ptukey(nemenyiValue,kClassifiers,dof)    
    return(p)
  }
  
  #Perform pairwise comparisons of entities x,y (columns) and return nemenyi value
  compare.stats <- function(i,j){
    nemenyiValue <- abs(sumClassifier[i]-sumClassifier[j])/d 
    return(nemenyiValue)
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