dijkstra <-
function(graph, init_node){
    if (!(is.data.frame(graph))){
        stop('the graph argument should be a dataframe')
    }
    if (sum(names(graph) == c('v1','v2','w')) != 3){
        stop('the graph argument should contain and only contain columns v1, v2, and w')
    }
    if (!is.numeric(init_node)){
        stop('the init_node argument should be a numeric scalar')
    }
    if (!init_node %in% c(graph$v1,graph$v2)){
        stop('the init_node argument should exist in the graph')
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
