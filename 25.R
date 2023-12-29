library(igraph)
instr <- strsplit(readLines("25.txt", warn = FALSE), ":* +")

make_graph <- function(instr) {
  nodes <- unique(unlist(instr))
  g <- make_undirected_graph(NULL, n = length(nodes))
  for (i in 1:length(instr)) {
    for (w in instr[[i]][-1]) {
      g <- add_edges(g, c(which(nodes == instr[[i]][1]), which(nodes == w)))
    }
  }
  g
}

cuts <- min_cut(make_graph(instr), value.only = FALSE)
message("Part 1: ", length(cuts$partition1) * length(cuts$partition2))
