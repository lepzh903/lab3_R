#' @docType data
#' @name wiki_graph
#' @title wiki_graph
#' @param v1,v2 vertices
#' @param w weight of the edge between two vertices
#' @description
#' (v1=1,v2=2,w=7) means that the length of the edge linking vertex 1 and vertex 2 is 7.
#' (v1=6,v2=5,w=9) means that the length of the edge linking vertex 6 and vertex 5 is 9.
#' @references /url:https://en.wikipedia.org/wiki/Graph
NULL
wiki_graph <- 
  data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
             v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
             w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))