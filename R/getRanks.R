getRanks <-
function(rawVector,ndigits){
  l <- length(rawVector)
  #Round values to allows matching of floats
  rawVector <- round(rawVector,digits=ndigits)
  #Sort and preserve indices. The sorted vector is used to assign ranks.
  sorted <- sort(rawVector,decreasing=TRUE,index.return=TRUE)
  values <- sorted$x
  indices <- sorted$ix
  
  #i fixes the current starting index, j indicates the final position in the current iteration. 
  #The difference represents the count of occurences of the same number.
  i<-1
  while(i<=l){
    j<-i
    #Count number of successive occurences of the same number
    while(values[j+1]==values[j] & j<l){
      j<-j+1
    }
    occurences <- j-i+1    
    
    #Replace raw numbers by ranks
    if(occurences>1){
      #If there is more than one occurence of the same number, assign the mean rank
      occurenceIndices <- indices[(i-1):i+(occurences-1)]
      rawVector[occurenceIndices] <- ((occurences/2)*(i+j))/occurences
    }else{
      rawVector[indices[i]]<-i
    }
    i<-j+1
  }
  return(rawVector)
}
