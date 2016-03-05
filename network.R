# Network analysis

## Network manipulation

addDegreeToVertices <- function (g) {
  require(igraph)
  try(if(!(is.igraph(g))) stop("Not an igraph graph"))
  if (is_directed(g)) {
    # try(if('indegree' %in% list.vertex.attributes(g)) stop("Vertex attribure indegree already exists"))
    # try(if('outdegree' %in% list.vertex.attributes(g)) stop("Vertex attribure outdegree already exists"))
    V(g)$indegree <- igraph::degree(g, V(g), mode = 'in') 
    V(g)$outdegree <- igraph::degree(g, V(g), mode = 'out') 
  } else {
    try(if('degree' %in% list.vertex.attributes(g)) stop("Vertex attribure degree already exists"))
    V(g)$degree <- igraph::degree(g, V(g)) 
  }
  return(g)
}

vertexAttributesAsDataFrame <- function(g) {
  try(if(!(is.igraph(g))) stop("Not an igraph graph"))
  vertex_attributes <- list.vertex.attributes(g)
  df <- data.frame(matrix(NA, nrow = vcount(g), ncol = 0))
  for (a in vertex_attributes) {
    df <- cbind(df, data.frame(get.vertex.attribute(g, a)))
  }
  names(df) <- list.vertex.attributes(g)
  return(df)
}


