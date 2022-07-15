#Code for a semgraphics function that optimizes pre-loading position matrices to feed into Semplot functions.
#Test Matrix
a <- c("Ere","bro","nah")
b <- c(NA,"this", "argh")
d <- c("yeah", "no", "that")
mat <- rbind(a,b,d)
mat

#MAIN
#-------------------------------------------------------------------------------
#optimize is the main function. algorithm works with trigonometric distance within a matrix, which is by mathematical means easy to evaluate and 
optimize <- function(object){
  #Analyze object
  #Get latent/indicator relations from the model
  #fetch nodes
  cr <- node_init(object)
  #create starting matrix from fitted square root value.
  mat <- matrix(ncol = cr, nrow = cr)
  #for non-parent indicators, set them aside
  #get edges from the model
  
  #Group them according to their status
  
  #Insert all node names into a vector to create a matrix from object nodes
  nodes <- c(getlist)
  nodeno <- length(nodes)
  
  #get coordinates of every node and put them in a "map"
  nodemap <- matrix(ncol=3) 
  for(i in nodes){
    pos <- which(mat[]==i)
    cord <- c(i, m_colrow(mat, pos)$x, m_colrow(mat, pos)$y)
    nodemap <- rbind(nodemap, cord)
    
  }
  rownames(nodemap) <- NULL
  nodemap <- nodemap[-1,]
  
  #get distances for every node with each other node
  #create list of all possible combinations and filter it for duplicates
  comblist <- expand.grid(nodes, nodes) #create list of all possible combinations.
  names(comblist) <- NULL #kills headers and enables proper function of duplicate filtering
  comblist <- as.matrix(comblist) #convert to matrix for easier handling
  
  #filter remove bidirectional duplicates 
  comblist <- comblist[!duplicated(lapply(1:nrow(comblist), 
                                          function(y){
                                            A <- comblist[y, ]
                                            A[order(A)]
                                          })),]
  
  #remove edges pointing to the node they came from.
  removal <- c()
  for(i in 1:nrow(comblist)){
    if(comblist[i,1]==comblist[i,2]){
      removal <- c(removal,i)
    }
  }
  comblist <- comblist[-removal,]
  
  #compute distance list from comblist and nodemap
  distancelist <- matrix(ncol=3)
  for (i in 1:nrow(comblist)){
    a <- which(nodemap[]==comblist[i,1])
    #retrieves the list-position of the first node from the node map
    b <- which(nodemap[]==comblist[i,2]) 
    #retrieves the list-position of the second node from the node map
    dist <- differential(as.numeric(nodemap[a,2]),as.numeric(nodemap[b,2]),as.numeric(nodemap[a,3]),as.numeric(nodemap[b,3])) #computes euclidean distances from the nodes according to the nodemap
    #Finally put together a list that entails distances and nodes. 
    distancelist <- rbind(distancelist, c(comblist[i,1], comblist[i,2], dist))}
  
  #remove empty header
  distancelist <- distancelist[-1,]
  
  #introduce a measure of interpretation to see whether distance is ok or too far away
  distcl <- matrix()
  
  #Kill all node connections that are too far away.
  med <- median(as.numeric(distancelist[,3]))
  distancelist <- distancelist[order(distancelist[,3],decreasing = T),]
  
  #take the input model of nodes and edges from a semplot file.
  #compute distances from every node to another. add horizontal and vertical properties to the node distances. Distances > sqrt(2) (horizontally or diagonally) indicate better distance. Vertical distances of 1 need to be multiplied along scale:::
  #-------------
  #get edges between nodes in the following order
  #latent to latent
  #latent to indicator
  #indicator to indicator
  #manifest to latent
  #manifest to indicator
  #get list of node names
  #test name positions with which(mat[=="name"]), get x or y coordinates and test their distance depending on their relationship. parent nodes get other parameters than child nodes. 
  #compute distances between interesting points with differential(). 
  #determine indicator and node edges
  #specify least distance between latent vars
  #specify least distance between parent and child nodes
  #
  #if there is a zero distance, input columns in between until distance X (tbd) is achieved
  #if there is a zero distance between rows, input rows until distance x is achieved.
}