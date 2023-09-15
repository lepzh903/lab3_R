#' Find the shortest paths between nodes in a weighted graph
#' 
#' @description
#' The function takes a graph and an initial node and calculates the shortest path from the initial node to every other node in the graph. 
#' 
#' @param graph  A data.frame with three variables (v1, v2 and w) that contains the edges of the graph (from v1 to v2) with the weight of the edge (w).
#' @param init_node A numeric scalar that exists in the graph.
#'
#' @returns A numeric vector that consists of the shortest path to every other node from the starting node.
#' 
#' @references [link url:](https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm)
#' 
#' @examples
#' wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),
#'                          v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),
#'                          w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
#' dijkstra(wiki_graph, 1)
#' dijkstra(wiki_graph, 8)
#' 
#' @export
dijkstra <-
function(graph, init_node){
    if (!(is.data.frame(graph))){
        stop('graph should be a dataframe')
    }
    if (!(names(graph)[1]=='v1'&&names(graph)[2]=='v2'&&names(graph)[3]=='w' )){
        stop('graph should only contain three variables v1, v2, and w')
    }
    if (!is.numeric(init_node)){
        stop('the init_node should be a numeric scalar')
    }
    if (!init_node %in% c(graph$v1,graph$v2)){
        stop('the init_node should exist in the graph')
    }
    
    vts <- unique(c(graph$v1,graph$v2))
    num_vts <- length(vts)
    m_dis <- matrix(Inf, nrow = num_vts, ncol = num_vts)
    for (i in 1:nrow(graph)){
        x <- graph$v1[i]
        y <- graph$v2[i]
        m_dis[x,y] <- graph$w[i]
        m_dis[y,x] <- graph$w[i]
    }
    
    dis <- rep(Inf, num_vts)
    dis[init_node] <- 0
    unvisited <- rep(TRUE, num_vts)
    
    while (TRUE %in% unvisited){
        u <- which(unvisited & dis == min(dis[unvisited]))
        unvisited[u] <- FALSE
        
        for (v in 1:num_vts){
            if (unvisited[v] && m_dis[u,v] != Inf){
                if (dis[u] + m_dis[u,v] < dis[v]){
                    dis[v] <- dis[u] + m_dis[u,v]
                } 
            }
        }
    }
    return(dis)
}
