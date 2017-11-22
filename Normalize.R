Normalize <- function(X){
  # normalizes the data with X a data table ,
  ###### OUT :
  # normalized table
  # column means by which standardized
  # column sd by which standardized
  
  # compute mean column by column
  means <- apply(X, 2, mean)
  # compute standard deviation column by column
  sd <- apply(X, 2, sd)
  
  # normalize the data 
  # (x-mu)/sd
  # substract the mean from each column
  Xnew <- sweep(X, 2, means)
  # divide by sd
  Xnew <- sweep(Xnew, 2, sd, '/')
  
  # returning values
  sortie <- NULL
  sortie$matrice <- Xnew
  sortie$means <- means
  sortie$sd <- sd
  return(sortie)
}