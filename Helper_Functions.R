#Helper Functions

#differential computes trigonometric distances between nodes. From a matrix notation standpoint, other distance metrics dont make much sense and are weird to interpret. Since matrix positions are fixed in their naming convention, it is easy to compute distances between matrices. 
differential <- function(x1,x2,y1,y2){
  dd <- round(sqrt((x2-x1)^2+(y2-y1)^2), digits =2)
  return(dd)
}

#is.wholenumber checks whether an input is a whole number and returns true/false
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5){
  abs(x - round(x)) < tol
}
#this function finds x/y coordinates of any given node in a matrix, given the matrix mat and the point's length(which(mat[]=="target"))
m_colrow <- function(mat,pos){
  y <- ceiling(pos/ncol(mat))  
  starter <- 1
  x <- c()
  if(pos<=ncol(mat)){
    x <- pos
  }else{
    for(i in 2:ncol(mat)){
      if(pos<=ncol(mat)*i){
        x <- pos-ncol(mat)*(i-1)
        break
      }
    }}
  return(list("x"=c(x), "y"=c(y)))
}
#node_init fetches nodes from a semplot object and extracts all node names, then creating a square starting matrix from node vector length by iterating square roots of node length.
node_init <- function(object){
  #get total nodes from the model
  nodes <- c(getlist)
  nodeno <- length(nodes)  
  #set initial matrix as the smallest possible square for the number of nodes, defaulting
  #to the row and col numbers to the square root of nodes.
  if(is.wholenumber(sqrt(nodeno))==TRUE){
    cr <- sqrt(nodeno)
  }else{
    while(is.wholenumber(sqrt(nodeno)==FALSE)){
      nodeno <- nodeno+1
      if(is.wholenumber(sqrt(nodeno))==TRUE){
        break
        return(nodeno)
      }else{next}
    }
    cr <- sqrt(nodeno)
  }
}
