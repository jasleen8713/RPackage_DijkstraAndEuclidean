#'Function to implement dijkstra algorithm in R
#'@param graph as data.frame
#'@param init_node as numeric
#'@return vector
#'@export
#'@examples dijkstra(graph = data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6), v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9)), init_node = 1)
#'@references <https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm>

dijkstra <- function(graph, init_node){

  #check if arguments are as expected
  if(is.data.frame(graph) && "v1" %in% names(graph) && "v2" %in% names(graph) && "w" %in% names(graph)
     && is.numeric(init_node) && length(init_node) == 1 && (init_node %in% graph$v1 || init_node %in% graph$v2)){
    #set of unvisited nodes
    unvisitedset<-data.frame(node=c(0),dist=c(0),prev=c(0))

    #initialised the return vector
    newdist=c(length(unique(graph$v1)))

    #updated the unvisited nodes data frame with node,distance as infinity and previous node as NA
    for(i in 1:length(unique(graph$v1)))
    {
      unvisitedset[i,]<-c(i,Inf,NA)
    }

    #updated distance of initial (source) node from initial node
    unvisitedset[unvisitedset$node==init_node,] <- c(init_node,0,init_node)

    #till unvisited nodes are empty
    while(nrow(unvisitedset)>0){

      minNode <- which(unvisitedset$dist==min(unvisitedset$dist))[1]
      minNodePrev <-unvisitedset[minNode,]$prev #get row index for minimum distance
      minNodeDist <-unvisitedset[minNode,]$dist

      #get neighbour nodes
      neighbournodes <- graph[which(graph$v1==unvisitedset[minNode,]$node),]
      #update the return vector
      newdist[unvisitedset[minNode,]$node]<-minNodeDist
      #delete the visited node from the unvisited node data frame
      unvisitedset<-unvisitedset[-c(minNode),] #remove

      #loop through all the neighbour nodes
      for(j in 1:nrow(neighbournodes)){
        #skip the neighbour node which has already been visited
        if(nrow(unvisitedset[unvisitedset$node==neighbournodes$v2[j],])>0)
        {
          #calculate new length/weight
          alt<- minNodeDist + neighbournodes$w[j]

          #if it is less than existing then update it and the path as well
          if(alt<unvisitedset[unvisitedset$node==neighbournodes$v2[j],]$dist){
            unvisitedset[unvisitedset$node==neighbournodes$v2[j],]<-c(neighbournodes$v2[j],alt,minNodePrev)

          }
        }

      }
    }
    return(newdist)
  }
  else
  {
    stop("First argument should be a data frame with variables v1, v2 and w. Second argument should be a numeric scalar existing in either v1 or v2.")
  }
}
