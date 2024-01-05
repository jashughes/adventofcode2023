library(igraph)
instr <- readLines("23.txt", warn = FALSE) |>
  strsplit("")

m <- matrix(unlist(instr), nrow = length(instr), byrow = T)

pos_around <- function(pos, maxx = dim(m)[1], maxy = dim(m)[2]) {
  p <- rbind(pos + c(-1, 0), pos + c(0, -1), pos + c(1, 0),  pos + c(0, 1))
  p[p[,1] > 0 & p[,1] <= maxx & p[,2] > 0 & p[,2] <= maxy,]
}
zip <- function(m) paste0(m[,1], "_", m[,2])

idx <- which(m != "#", arr.ind = T)

check_slope <- function(pos, new, val, pt2 = T) {
  if (pt2) return(TRUE)
  if (val == "^") return(pos[,1] > new[,1])
  if (val == "v") return(pos[,1] < new[,1])
  if (val == ">") return(pos[,2] < new[,2])
  if (val == "<") return(pos[,2] > new[,2])
  TRUE
}

g <- make_directed_graph(NULL)
g <- add_vertices(g, nrow(idx))
for (i in 1:nrow(idx)) {
  nbs <- pos_around(idx[i,,drop=F])
  nbs <- nbs[m[nbs] != "#",,drop=F]
  for (nb in 1:nrow(nbs)) {
    if (check_slope(idx[i,,drop=F], nbs[nb,,drop=F], m[nbs[nb,,drop=F]])) {
      node <- which(zip(idx) %in% zip(nbs[nb,,drop=F]))
      g <- add_edges(g, c(i, node))
    }
  }
}

# part 1
# paths <- all_simple_paths(g, 1, nrow(idx))
# max(vapply(paths, length, numeric(1))) - 1

# part 2
# find branches of > 2 create new graphs
n_nodes <-vapply(1:length(g), function(x) length(adjacent_vertices(g,x)[[1]]), numeric(1))
g2 <- make_directed_graph(NULL)
g2 <- add_vertices(g2, sum(n_nodes > 2) + 2)

# for each fork and start and end, see if they are linked, if they are,
# add a link between the nodes
v <- sort(c(1, length(g), which(n_nodes > 2)))
for (i in 1:length(v)) {
  nc <- length(adjacent_vertices(g, v[i])[[1]])
  all_lengths <- vapply(v, function(x) length(shortest_paths(g, v[i], x)$vpath[[1]]), numeric(1))
  new <- setdiff(head(v[order(all_lengths)], nc + 1), v[i])
  for (n in new) {
    g2 <- add_edges(g2, c(i, which(v == n)))
  }
}




all_paths <- all_simple_paths(g2, 1, length(v))

len <- rep(0, length(all_paths))

for (p in 1:length(all_paths)) {
  path <- all_paths[[p]]
  for (s in 2:length(path)) {
    len[[p]] <- len[[p]] + length(shortest_paths(g, v[path[s-1]], v[path[s]])$vpath[[1]])
  }
}
max(len)
